#####################################################################################################
# function to generate weather inputs, inflow/outflow given start/end date
# coeffs--Kw, coeff_mix_hyp, sw_factor, lw_factor, sed_temp_mean
# this should work for future or hindcast scenarios
# enter start/stop dates as characters:"YYYY-MM-DD". If dates are not formatted correctly
# an error will show up
# 0.5 < Kw < 1.5 generally
# 0 < sed_temp_mean < 1
# sw_factor is scaling for radiation but anything much greater or less than 1 would be unusual
# lw_factor same as sw_factor but for longwave radiation
# sed_temp_mean is temperature of soil of lake so 0 would be frozen (unlikely) and 32 would be very
# very warm
# bdat = filter(biasdat, YEAR >=2020)
# mydates = unique(paste(bdat$YEAR, bdat$MONTH, bdat$DAY, sep = "-"))
# which(mydates == "2020-9-25")
# good_dates = mydates[270:length(mydates)] #1-456


#start_date = "2022-10-30"
#biasdat = read.csv("C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/bias_dat_forHetGP.csv")
#biasdat$date = paste(biasdat$YEAR, biasdat$MONTH, biasdat$DAY, sep = "-")
#biasdat$date = as.POSIXct(biasdat$date, tz = "UTC")
# including 7 day spinup, start date should be 2020-10-02 or later

#start_date = "2021-08-04"
#main(start_date = "2021-01-29")
get_inputs = function(start_date, stop_date = NULL,
                      Kw = 0.87, 
                      coeff_mix_hyp = 0.6458725, 
                      sw_factor = 1,
                      lw_factor = 1, 
                      sed_temp_mean = c(11.39072901, 14.85279613)){
  
  # first check validity of date
  # fut_flag = TRUE means we are forecasting mode
  # will automatically return stop date for a 35 day forecast if stop_date 
  # implies > 35 day forecast. Will throw error for incorrectly formatted dates
  date_results = check_date_forecast(start_date)
  
  fut_flag = date_results$fut_flag
  
  stop_date = date_results$stop_actual
  
  # grab noaa forecasts
  # returns data.frame containing all ensemble members
  # w/ stop_date = NULL, we always get 35 day forecasts
  # checked
  forecast = get_NOAA_forecast(start_date, stop_date=NULL)

  hindcast = get_NOAA_hindcast(start_date)
  
  forecast = rbind(hindcast, forecast)
  # set start_date to be first date in new 'forecast'
  start_date_hindcast =gsub("\\s+\\d{1,2}:\\d{1,2}", "", forecast$time[1])
  # initial conditions -- vector of length 10 
  # for depths 0 thru 9
  # checked
  initial_temps = get_initial_conditions(start_date_hindcast)
  if (is.na(initial_temps)){
    print("skipping day--not enough observed data to calculate initial values!")
    return(NA)
  }
  # NOT SURE IF I NEED THIS FUT FLAG bc it will always be true for this first exercise!!
  if (fut_flag){
    # if fut_flag: do the interpolation 
    inflow_outflow_list = get_inflow_forecast(start_date)
    inflows = inflow_outflow_list$all_inflow
    outflows = inflow_outflow_list$all_outflow
  }else{
    print("Fut_flag == FALSE! This option has not yet been implemented.")
    return()
  }
  
  inputs_param_list = list(fut_flag = fut_flag, inflows = inflows, outflows=outflows, 
                           noaa_met_GLM = forecast, initial_temps= initial_temps,
                           coeff_list = list(Kw = Kw,
                                             coeff_mix_hyp = coeff_mix_hyp,
                                             sw_factor = sw_factor,
                                             lw_factor = lw_factor,
                                             sed_temp_mean = sed_temp_mean))
  return(inputs_param_list)  
  
}



####################################################################
# checks forecast date for formatting errors and ensures
# start_date < stop_date if given 
# if stop_date not given, checks if 'forecast' exceeds today, 
# assuming forecast length of 35 days
# NOTE: I made future_flag always = TRUE b/c we want to 
# calculate inflows/outflows using Quinns method, not historical data
check_date_forecast = function(start_date, stop_date = NULL){
    # check start for format
    date_check = as.integer(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", start_date, value=F))
    x = integer(0)
    if (identical(x, date_check)){
      stop("invalid time format. Please enter date as 'YYYY-MM-DD' ")}
    
    date_actual = as.POSIXct(start_date, tz = "UTC")
    
    # check if start date is less than 2018/07/06 (that's where sensor data starts)
    if (date_actual < as.POSIXct("2018-07-06", tz = "UTC")){
      stop("start_date must be after 2018-07-05")
    }
    
    start_time_NOAA = as.POSIXct("2020-09-25", tz = "UTC") + lubridate::days(7)
    if ( (date_actual < start_time_NOAA) ){
      message("Desired forecast start date is prior to new NOAA forecasting model (2020-10-02 including 7day spinup).")
    }
    curr_date = Sys.Date()
    curr_date = as.POSIXct(curr_date, tz = "UTC")
    
    # if no stop date given, assume 35 days into future
    if (is.null(stop_date)){
      stop_test = curr_date - lubridate::days(35)
      if (date_actual > stop_test){
        message("No stop date given; 35 day assumed forecast length exceeds today. Calculating stop
                date for 35 day forecast")
        # set stop date to be start_date + 35 days
        stop_actual = as.character(date_actual + lubridate::days(35))
        return(list(fut_flag=TRUE, stop_actual = stop_actual))
      }else{
        message("No stop date given; 35 day assumed forecast does NOT exceed today. Calculating stop
                date for 35 day forecast")
        # no stop date given , but assuming 35 day forecast, does not exceed today
        stop_actual = as.character(date_actual + lubridate::days(35))
        return(list(fut_flag=TRUE, stop_actual = stop_actual))
      }
        
    }else{
      # if stop date IS given, check stop_date for format
      date_check = as.integer(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", stop_date, value=F))
      x = integer(0)
      if (identical(x, date_check)){
        stop("invalid time format. Please enter date as 'YYYY-MM-DD' ")
        }
      # check if stop is less than start
      stop_actual = as.POSIXct(stop_date, tz = "UTC")
      print("here 3")
      if (stop_actual < date_actual){
        stop("stop date is less than start date!")
      }
      # if stop is more than 35 days ahead, just make it 35 days ahead
      if ((stop_actual - date_actual) > 35){
        message("stop date implies a forecast longer than 35 days. Returning stop date for a 35 day forecast")
        stop_actual = as.character(date_actual + lubridate::days(35))
        
        if (stop_actual > curr_date){
          return(list(fut_flag = TRUE, stop_actual = stop_actual))
        }else{
          # if stop_actual does not exceed today, return fut flag = F and the date
          return(list(fut_flag = TRUE, stop_actual = stop_actual))
        }
      }else{
        # stop date is fine, but goes into future period, adjust fut flag
        return(list(fut_flag = TRUE, stop_actual = stop_actual))
      }
    }
    print("you forgot some case")
}

###############################################################################
# returns initial conditions--10 values of temps in increasing order of depth
# if depths are missing, temps are interpolated using linear interpolation
# sensor data of lake temperature--updated every day
###############################################################################

get_initial_conditions = function(start_date){
  init_date = as.POSIXct(start_date, tz = "UTC") - as.difftime(1, unit="days")
  lake_temps <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv")
  
  lake_init = filter(lake_temps) %>%
    dplyr::filter(variable == "temperature") %>%
    dplyr::filter(datetime == init_date) %>%
    dplyr::arrange(depth)
  
  # check if we have all the depths
  # these are the depths of interest
  good_depths = 0.00:9.00
  
  # are any depths missing?
  idx = match(good_depths, lake_init$depth)
  avail_depths  = lake_init$depth[idx]
  if (sum(is.na(avail_depths))>=8){
    return(NA)
  }

  if (setequal(avail_depths, good_depths)){
    return(lake_init$observation[idx])
  }else{
    # if any depths are missing, interpolate missing depths to depths of interest
    approx_inits = approx(x = lake_init$depth, y = lake_init$observation, xout = 0:9)$y
    return(approx_inits)
  }
}

############################################################################
# Function to get NOAA forecast for given start_date from cloud
# start_date is when the forecast starts (character 'YYYY-MM-DD')
# stop_date is when forecast stops
# returns data.frame of NOAA forecasts containing 30 ensemble members
# no need to check start/stop dates--they will already have been checked
############################################################################
get_NOAA_forecast = function(start_date, stop_date = NULL){
  #use_ler_vars = FALSE # don't know what this is!
  
  # cloud directory for NOAA forecasts
  forecast_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12/stage2/parquet",
                                   endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
  df_fut <- arrow::open_dataset(forecast_dir, partitioning = c("cycle","reference_date"))
  
  # cyle = 0 means start at hour 0 I think?
  # start_date is just the character date -- 'YYYY-MM-DD'
  forecast <- df_fut |>
    dplyr::filter(site_id == "fcre", reference_date == start_date, cycle == 0) |>
    dplyr::select(datetime, parameter,variable,prediction) |>
    dplyr::collect() |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::arrange(parameter, datetime) |>
    dplyr::mutate(WindSpeed = sqrt(unlist(eastward_wind)^2 + unlist(northward_wind)^2)) |>
    dplyr::rename(AirTemp = air_temperature,
                  ShortWave = surface_downwelling_shortwave_flux_in_air,
                  LongWave = surface_downwelling_longwave_flux_in_air,
                  RelHum = relative_humidity,
                  Rain = precipitation_flux,
                  ensemble = parameter,
                  time = datetime) |>
    dplyr::mutate(AirTemp = unlist(AirTemp) - 273.15,
                  RelHum = RelHum * 100,
                  RelHum = ifelse(RelHum > 100, 100, RelHum),
                  Rain = Rain * (60 * 60 * 24)/1000,
                  Snow = 0.0) |>
    dplyr::mutate_at(dplyr::vars(all_of(c("AirTemp", "ShortWave","LongWave","RelHum","WindSpeed"))), list(~round(., 2))) |>
    dplyr::mutate(Rain = round(Rain, 5),
                  time = strftime(time, format="%Y-%m-%d %H:%M", tz = "UTC")) |>
    dplyr::select(ensemble, time, AirTemp,ShortWave, LongWave, RelHum, WindSpeed,Rain) |>
    dplyr::group_by(ensemble) |>
    dplyr::slice(-dplyr::n()) |>
    dplyr::ungroup()
 
  if (!(is.null(stop_date))){
    print("here 2")
    # if we want a forecast of less than 35 days...
    # these timestamps will only be selected
    forecast_start = paste(start_date, " 00:00:00", sep = "")
    forecast_stop = paste(stop_date, " 00:00:00", sep = "")
    forecast_start = as.POSIXct(start_date, tz = "UTC")
    forecast_stop = as.POSIXct(stop_date, tz = "UTC")
    
    if (forecast_stop - forecast_start < 35){
  
      
      time_seq = seq(forecast_start, forecast_stop, by = "1 hour")
      time_seq = strftime(time_seq, format="%Y-%m-%d %H:%M", tz = "UTC")
      forecast = dplyr::filter(forecast, time %in% time_seq)
      
    }
  }
  return(forecast)
  
}


######################################################################################
# function to get only the 1 day ahead forecasts used for spinup
# hindcast will start at start_date - lookback number of days (set to 7 by default)
######################################################################################
get_NOAA_hindcast = function(start_date, lookback = 7){
  start_date_forecast = as.POSIXct(start_date, tz = "UTC")
  start_hind_posix = start_date_forecast - lubridate::days(lookback)
  #stop_hind_posix = start_date_forecast - lubridate::hours(1)
  
  full_time_hist <- seq(start_hind_posix, start_date_forecast, by = "1 hour")
  
  
  past_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12/stage3/parquet",
                               endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
  df_past <- arrow::open_dataset(past_dir, partitioning = c("site_id"))
  
  historical_noaa <- arrow::open_dataset(past_dir) |>
    dplyr::filter(site_id == "fcre") |>
    dplyr::select(datetime, parameter,variable,prediction) |>
    dplyr::collect() |>
    dplyr::filter(datetime %in% full_time_hist) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::arrange(parameter, datetime) |>
    dplyr::mutate(WindSpeed = sqrt(eastward_wind^2 + northward_wind^2)) |>
    dplyr::rename(AirTemp = air_temperature,
                  ShortWave = surface_downwelling_shortwave_flux_in_air,
                  LongWave = surface_downwelling_longwave_flux_in_air,
                  RelHum = relative_humidity,
                  Rain = precipitation_flux,
                  ensemble = parameter,
                  time = datetime) |>
    dplyr::mutate(AirTemp = AirTemp - 273.15,
                  RelHum = RelHum * 100,
                  RelHum = ifelse(RelHum > 100, 100, RelHum),
                  Rain = Rain * (60 * 60 * 24)/1000,
                  Snow = 0.0) |>
    dplyr::mutate_at(dplyr::vars(all_of(c("AirTemp", "ShortWave","LongWave","RelHum","WindSpeed"))), list(~round(., 2))) |>
    dplyr::mutate(Rain = round(Rain, 5),
                  time = strftime(time, format="%Y-%m-%d %H:%M", tz = "UTC")) |>
    dplyr::select(ensemble, time, AirTemp,ShortWave, LongWave, RelHum, WindSpeed,Rain) |>
    dplyr::group_by(ensemble) |>
    dplyr::slice(-dplyr::n()) |>
    dplyr::ungroup()
  
  return(historical_noaa)
}

#############################################################################
# extract NOAA data for given start date. 
# used for function below for generating inflow/outflow
# lookback is the time in days used for spinup. Set to 7 as default
#############################################################################
get_NOAA_inflow = function(start_date, lookback = 7){
  # cloud directory for NOAA forecasts
  forecast_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12/stage2/parquet",
                                   endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
  df_fut <- arrow::open_dataset(forecast_dir, partitioning = c("cycle","reference_date"))
  
  # cyle = 0 means start at hour 0 I think?
  # start_date is just the character date -- 'YYYY-MM-DD'
  forecast <- df_fut |>
    dplyr::filter(site_id == "fcre", reference_date == start_date, cycle == 0) |>
    dplyr::select(datetime, parameter,variable,prediction) |>
    dplyr::collect() |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::arrange(parameter, datetime)
  
  # do same shit for hindcast...
  start_date_forecast = as.POSIXct(start_date, tz = "UTC")
  start_hind_posix = start_date_forecast - lubridate::days(lookback)
  stop_hind_posix = start_date_forecast - lubridate::hours(1)
  
  full_time_hist <- seq(start_hind_posix, stop_hind_posix, by = "1 hour")
  
  
  past_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12/stage3/parquet",
                               endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
  df_past <- arrow::open_dataset(past_dir, partitioning = c("site_id"))
  hindcast = df_past |>
    dplyr::filter(site_id == "fcre") |>
    dplyr::select(datetime, parameter,variable,prediction) |>
    dplyr::collect() |>
    dplyr::filter(datetime %in% full_time_hist) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::arrange(parameter, datetime)
  
  forecast = rbind(forecast, hindcast)
  return(forecast)
}

#############################################################################
# function to get inflow and outflow based on NOAA met conditions
# returns list with inflow and outflow data.frames for all ensemble members
#############################################################################
get_inflow_forecast = function(start_date, start_date_hindcast, stop_date = NULL){
  
  # this is just the NOAA file with all ensemble members,
  # but without extra formatting
  noaa_met = get_NOAA_inflow(start_date)

  
  ensemble_members <- unique(noaa_met$parameter)
  
  # read in observed inflow
  obs_inflow <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-inflow.csv")
  
  inflow <- obs_inflow %>%
    tidyr::pivot_wider(names_from = variable, values_from = observation) %>%
    dplyr::select(datetime, TEMP, FLOW) 


  # get day before forecast and use as initial inflow
  init_flow_temp <- inflow %>%
    filter(datetime == (noaa_met$datetime[1] - lubridate::days(1)))

  # set initial TEMP and FLOW to be those for the day before forecast starts
  init_flow <- rep(init_flow_temp$FLOW[1], length(ensemble_members))
  init_temp <- rep(init_flow_temp$TEMP[1], length(ensemble_members))
  
  # quinn's code
  # read in observed met data
  met <-  data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/observed-met_fcre.csv") %>%
    dplyr::filter(variable %in% c("air_temperature", "precipitation_flux")) %>%
    tidyr::pivot_wider(names_from = variable, values_from = observation) %>%
    dplyr::select(-site_id)
  
  # extract previous day's met data (hourly)
  obs_met <- met %>%
    dplyr::filter((datetime >= noaa_met$datetime[1] - lubridate::days(1))
                             & (datetime < noaa_met$datetime[1]) )
  head(noaa_met)
  # merge data from noaa met , obs met, initial flow and initial temp data
  d = process_noaa_for_inflow(ensemble_members,noaa_met, obs_met, init_flow, init_temp)
  
  # process so that this is formatted for GLM--with exception 
  # that 'ensemble' variable needs to be deleted later
  d_inflow = dplyr::filter(d, flow_type == "inflow") %>%
    dplyr::select(datetime, parameter, variable, prediction) %>%
    tidyr::pivot_wider(names_from = variable, values_from = prediction) %>%
    dplyr::rename(time = datetime, ensemble = parameter) %>%
    dplyr::select(time, ensemble, FLOW, TEMP, SALT)
  
  # NOT 100% sure that this is correct. How to deal w/ the other inflow file?
  
  d_outflow = dplyr::filter(d, flow_type == "outflow") %>%
    dplyr::select(datetime, parameter, variable, prediction) %>%
    tidyr::pivot_wider(names_from = variable, values_from = prediction) %>%
    dplyr::rename(time = datetime, ensemble = parameter) %>%
    dplyr::select(time, ensemble, FLOW)
  
  # if stop date is given extract just the dates we want
  if (!(is.null(stop_date))){
    print("here 1")
    forecast_start = paste(start_date_hindcast, " 00:00:00", sep = "")
    forecast_stop = paste(stop_date, " 00:00:00", sep = "")
    forecast_start = as.POSIXct(start_date_hindcast, tz = "UTC")
    forecast_stop = as.POSIXct(stop_date, tz = "UTC")
    
    time_seq = seq(forecast_start, forecast_stop, by = "1 day")
    d_inflow = dplyr::filter(d_inflow, time %in% time_seq)
    d_outflow = dplyr::filter(d_outflow, time %in% time_seq)
  }
  
  
  # Don't know what to do w/ this ask Quinn
  # Do I need the two inflows?? Or is just 1 OK?
  # weir_inflow_dates <- inflow %>%
  #   dplyr::filter(datetime > clima_start & datetime < clima_end) %>%
  #   dplyr::mutate(DOY = yday(datetime)) %>%
  #   dplyr::select(datetime, DOY)
  
  return(list(all_inflow = d_inflow, all_outflow = d_outflow))
}


#############################################################################################
# function to process noaa met file, observed met file, initial flows and initial temps data
# returns dataframe w/ all of the ensembles (1-31 under 'parameter') and 
# values for flow, temp, salt for each datetime. 
#############################################################################################
process_noaa_for_inflow = function(ensemble_members, noaa_met, obs_met, init_flow, init_temp,
                                   future_inflow_flow_coeff = c(0.0010803, 0.9478724, 0.3478991),
                                   inflow_error = 0.00965,
                                   future_inflow_temp_coeff = c(0.20291, 0.94214, 0.04278),
                                   future_inflow_temp_error = 0.943){
  d <- purrr::map_dfr(ensemble_members, function(ens, noaa_met, obs_met, init_flow, init_temp){
    df <- noaa_met |>
      dplyr::filter(parameter == ens) |>
      dplyr::select(-parameter) |>
      dplyr::bind_rows(obs_met) |>
      dplyr::arrange(datetime) |>
      dplyr::rename(AirTemp = air_temperature,
                    Rain = precipitation_flux) |>
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    Rain = Rain * (60 * 60 * 24)/1000) %>%
      dplyr::mutate(datetime = lubridate::with_tz(datetime, tzone = "UTC"),
                    datetime = datetime - lubridate::hours(lubridate::hour(datetime[1]))) %>%
      dplyr::mutate(datetime = lubridate::as_date(datetime)) %>%
      dplyr::group_by(datetime) %>%
      dplyr::summarize(Rain = mean(Rain),
                       AirTemp = mean(AirTemp),.groups = 'drop') %>%
      dplyr::mutate(ensemble = ens) %>%
      dplyr::mutate(AirTemp_lag1 = dplyr::lag(AirTemp, 1),
                    Rain_lag1 = dplyr::lag(Rain, 1)) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(FLOW = NA,
                    TEMP = NA)
    
    df$FLOW[1] <- init_flow[ens]
    df$TEMP[1] <- init_temp[ens]
    
    
    inflow_error <- rep(0.0, nrow(df))
    temp_error <- rep(0.0, nrow(df))
    
    
    for(i in 2:nrow(df)){
      df$FLOW[i] = future_inflow_flow_coeff[1] +
        future_inflow_flow_coeff[2] * df$FLOW[i - 1] +
        future_inflow_flow_coeff[3] * df$Rain_lag1[i] + inflow_error[i]
      df$TEMP[i] = future_inflow_temp_coeff[1] +
        future_inflow_temp_coeff[2] * df$TEMP[i-1] +
        future_inflow_temp_coeff[3] * df$AirTemp_lag1[i] + temp_error[i]
    }
    
    df <- df %>%
      dplyr::mutate(FLOW = ifelse(FLOW < 0.0, 0.0, FLOW))
    
    df <- df %>%
      dplyr::mutate(SALT = 0.0) %>%
      dplyr::select(datetime, ensemble, FLOW, TEMP, SALT, AirTemp, Rain) %>%
      dplyr::mutate_at(dplyr::vars(c("FLOW", "TEMP", "SALT")), list(~round(., 4)))
    
    clima_start <- lubridate::as_date("2015-07-07")
    clima_end <-  lubridate::as_date(max(obs_met$datetime))
    
    df_output <- df %>%
      dplyr::select(datetime,ensemble, FLOW, TEMP)
    
    reference_datetime <- min(df$datetime)
    
    df <- df |>
      tidyr::pivot_longer(-c("datetime","ensemble"), names_to = "variable", values_to = "prediction") |>
      dplyr::mutate(site_id = "fcre",
                    family = "ensemble",
                    flow_type = "inflow",
                    flow_number = 1,
                    reference_datetime = reference_datetime) |>
      dplyr::rename(parameter = ensemble) |>
      dplyr::select(site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number)
    
    df_output <- df_output |>
      tidyr::pivot_longer(-c("datetime","ensemble"), names_to = "variable", values_to = "prediction") |>
      dplyr::mutate(site_id = "fcre",
                    family = "ensemble",
                    flow_type = "outflow",
                    flow_number = 1,
                    reference_datetime = reference_datetime) |>
      dplyr::rename(parameter = ensemble) |>
      dplyr::select(site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number)
    
    
    combined <- dplyr::bind_rows(df, df_output)
  },
  noaa_met, obs_met, init_flow, init_temp)
  
}

###################################################################################
# JUNK JUNK JUNK
# this was meant to generate 1 inflow/outflow file for anything in the historical 
# period (e.g. forecast does not exceed today)
# not sure we want this for noaa data? should it not be based on noaa met file 
# even if it's a historical date? Ask Quinn
get_inflow_historical = function(start_date, stop_date){
  start_date= as.POSIXct(start_date, tz="UTC")
  stop_date = as.POSIXct(stop_date, tz = "UTC")
  time_seq = as.Date(seq(start_date, stop_date, by = "1 day"))
  obs_inflow <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-inflow.csv")
  variables = c("time", "FLOW", "TEMP", "SALT")
  
  obs_inflow_sub = dplyr::filter(obs_inflow, datetime %in% time_seq)
  
  obs_inflow_tmp = dplyr::filter(obs_inflow_sub, variable %in% variables) %>%
    dplyr::rename(time = datetime) %>%
    dplyr::select(time, variable, observation) %>%
    tidyr::pivot_wider(names_from = variable, values_from = observation)
  head(obs_inflow_tmp)
  obs_outflow_tmp = obs_inflow_tmp #??????
  return(list(obs_inflow_tmp = obs_inflow_tmp, obs_outflow_tmp = obs_outflow_tmp))
}


