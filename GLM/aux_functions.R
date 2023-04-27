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
########################################################################################################

get_inputs = function(start_date, 
                      lake_temps, 
                      obs_inflow, 
                      stop_date = NULL,
                      Kw = 0.87, 
                      coeff_mix_hyp = 0.6458725, 
                      sw_factor = 1,
                      lw_factor = 1, 
                      sed_temp_mean = c(11.39072901, 14.85279613,
                      req_spinup = FALSE)){
  
  # first check validity of date
  # IMPORTANT: we are always assuming a 35 day forecast!
  # Will throw error for incorrectly formatted dates
  check_date_forecast(start_date)
  
  # grab noaa forecasts
  # returns data.frame containing all ensemble members
  # we always get 35 day forecasts
  # checked
  forecast = get_NOAA_forecast(start_date)

  # if spinup is required, get NOAA hindcasts
  if (req_spinup){
  hindcast = get_NOAA_hindcast(start_date)
  
  forecast = rbind(hindcast, forecast)
  # set start_date to be first date in new 'forecast'
  start_date_hindcast =gsub("\\s+\\d{1,2}:\\d{1,2}", "", forecast$time[1])
  start_date = start_date_hindcast
  }
  # Get initial conditions -- vector of length 10 
  # for depths 0 thru 9
  initial_temps = get_initial_conditions(start_date, lake_temps)
  if (anyNA(initial_temps)){
    print("skipping day--not enough observed data to calculate initial values!")
    return(NULL)
  }

  # Get inflow/outflow given start_date
  inflow_outflow_list = get_inflow_forecast(start_date, obs_inflow, req_spinup)
  inflows = inflow_outflow_list$all_inflow
  outflows = inflow_outflow_list$all_outflow

  
  inputs_param_list = list(inflows = inflows, outflows=outflows, 
                           noaa_met_GLM = forecast, initial_temps= initial_temps,
                           coeff_list = list(Kw = Kw,
                                             coeff_mix_hyp = coeff_mix_hyp,
                                             sw_factor = sw_factor,
                                             lw_factor = lw_factor,
                                             sed_temp_mean = sed_temp_mean))
  return(inputs_param_list)  
  
}



####################################################################
# checks forecast date for formatting errors 
# assuming forecast length of 35 days
check_date_forecast = function(start_date){
    # check start for format
    date_check = as.integer(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", start_date, value=F))
    x = integer(0)
    if (identical(x, date_check)){
      stop("invalid time format. Please enter date as 'YYYY-MM-DD' ")
    }
    
    date_actual = as.POSIXct(start_date, tz = "UTC")
    
    # check if start date is less than 2018/07/06 (that's when sensor data starts)
    if (date_actual < as.POSIXct("2018-07-06", tz = "UTC")){
      stop("start_date must be after 2018-07-05")
    }
    
    start_time_NOAA = as.POSIXct("2020-09-25", tz = "UTC") + lubridate::days(7)
    if ( (date_actual < start_time_NOAA) ){
      stop("Desired forecast start date is prior to new NOAA forecasting model (2020-10-02 including 7day spinup).")
    }
  print("date format is correct")
        
}

###############################################################################
# returns initial conditions--10 values of temps in increasing order of depth
# if depths are missing, temps are interpolated using linear interpolation
# sensor data of lake temperature--updated every day
###############################################################################

get_initial_conditions = function(start_date, lake_temps){
  init_date = as.POSIXct(start_date, tz = "UTC") - as.difftime(1, unit="days")
  #lake_temps <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv")
  
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
  
  print(sum(is.na(avail_depths)))
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
get_NOAA_forecast = function(start_date){
  #use_ler_vars = FALSE # don't know what this is
  
  # cloud directory for NOAA forecasts
   forecast_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", 
   endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)

   df_fut <- arrow::open_dataset(forecast_dir, partitioning = c("reference_date", "site_id"))
  
  # cyle = 0 means start at hour 0 I think?
  # start_date is just the character date -- 'YYYY-MM-DD'
  forecast <- df_fut |>
    #dplyr::filter(site_id == "fcre", reference_date == start_date, cycle == 0)
    dplyr::filter(site_id == "fcre", reference_date == start_date) |>
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
  
  
  past_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage3/parquet",
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
get_NOAA_inflow = function(start_date, lookback = 7, req_spinup = FALSE){
  # cloud directory for NOAA forecasts
  forecast_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", 
   endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
   
  df_fut <- arrow::open_dataset(forecast_dir, partitioning = c("reference_date", "site_id"))
  # cyle = 0 means start at hour 0 I think?
  # start_date is just the character date -- 'YYYY-MM-DD'
  forecast <- df_fut |>
    #dplyr::filter(site_id == "fcre", reference_date == start_date, cycle == 0)
    dplyr::filter(site_id == "fcre", reference_date == start_date) |>
    dplyr::select(datetime, parameter,variable,prediction) |>
    dplyr::collect() |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::arrange(parameter, datetime)
  
  if (req_spinup){
    # do same for hindcast...
  start_date_forecast = as.POSIXct(start_date, tz = "UTC")
    start_hind_posix = start_date_forecast - lubridate::days(lookback)
    stop_hind_posix = start_date_forecast - lubridate::hours(1)
  
    full_time_hist <- seq(start_hind_posix, stop_hind_posix, by = "1 hour")
  
  
    past_dir <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage3/parquet",
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
  }
  return(forecast)
}

#############################################################################
# function to get inflow and outflow based on NOAA met conditions
# returns list with inflow and outflow data.frames for all ensemble members
#############################################################################
get_inflow_forecast = function(start_date, obs_inflow, req_spinup = FALSE){
  
  # this is just the NOAA file with all ensemble members,
  # but without extra formatting
  noaa_met = get_NOAA_inflow(start_date)

  
  ensemble_members <- unique(noaa_met$parameter)
  
  # read in observed inflow
  #obs_inflow <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-inflow.csv")
  
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
  #head(noaa_met)
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

