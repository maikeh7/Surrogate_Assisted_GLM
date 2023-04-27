# Here I construct an observed dataset for each horizon, then merge w/ GLM surrogate and calculate bias
# also I added code so the temperature input is added
# see below for how to get observed data 
make_bias_horizon_datasets = function(train_dates, obs_depth=1, method="Average", lookback = 4,
                                      surrogate_dir = "SURROGATES", train_end_date, obs_data){

  surrogate_preds = read_surrogate_preds(surrogate_dir)
  
  # filter data so observed data only includes up until 2022-12-31 (earlier dates like 2018 won't
  # matter b/c they are filtered out later, but I don't want dates the surrogate was NOT trained on to 
  # be in the observed data -- basically, we can't use observations in the 'future' when validating,
  # b/c that would be cheating

  train_end_date = as.Date(train_end_date) 
  
  obs_data = filter(obs_data, date >= as.Date("2020-08-01"))
  obs_data = dplyr::filter(obs_data, date <= train_end_date)
  
  myobs_df = make_obs_data(obs_data, obs_depth=1, lookback = 4)

  counter = 1
  horizon=1:30
  obs_List = list()
  for(h in horizon){
    print(paste("horizon", h))
    for (i in train_dates){
      start_date = i

      start_date = as.Date(start_date) 
      
      # grab DOY that corresponds to the start date
      start_DOY = dplyr::filter(obs_data, date == start_date)$DOY[1]
      
      # if the day is not in the observed data, skip day
      if(is.na(start_DOY)){
        #print("day is missing!")
        next
      }
      # grab the OBSERVED XX-day head 'forecast' for a given start date
      end_date = start_date + lubridate::days(h)
      
      # filter so the dataset only contains the date (end date) , temperature, depth
      temp_file = dplyr::filter(obs_data, date == end_date) %>%
        dplyr::select(date, temp_obs, depth_int)
      if(nrow(temp_file) == 0){
        #print("day is missing!")
        next
      }
      # get the correct temperature input value
      start_date_obs_temp = get_obs_temp(actual_start = start_date, obs_depth, obs_df = myobs_df)
      
      if (nrow(temp_file)==0){
        next
      }
      # add DOY so we can merge w/ surrogate data
      temp_file$DOY = start_DOY
      temp_file$Horizon = h
      temp_file$Temp_covar = start_date_obs_temp
      temp_file$start_date = start_date
      obs_List[[counter]] = temp_file  
      counter = counter + 1
    }
  }
  
  
  obs_DF = data.table::rbindlist(obs_List)
  colnames(obs_DF)[3] = "Depth"
  colnames(obs_DF)[1] = "end_date"
  
  obs_DF = as.data.frame(obs_DF)
  obs_DF$start_date = as.character(obs_DF$start_date)
  obs_DF$Temp_covar = NULL
  head(obs_DF)
  # Now, merge w/ mean from surrogate
  newdf = right_join(surrogate_preds, obs_DF, by = c("Depth", "DOY", "Horizon", "start_date"))
  
  # calculate bias -> observed - surrogate
  # Mean is mean of y_hat, GP surrogate for GLM
  newdf$bias = newdf$temp_obs - newdf$Mean
  # save and fit with another script
  newdf = newdf %>% dplyr::select(DOY, Depth ,Horizon, Temp_covar, start_date, end_date, temp_obs, bias)
  write.csv(newdf, file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  
}


make_bias_horizon_datasets_validation = function(train_dates, 
                                                 new_date,
                                                 obs_depth=1, 
                                                 method="Average",
                                                 lookback = 4,
                                                 surrogate_dir, 
                                                 obs_data){
  
  surrogate_preds = read_surrogate_preds(surrogate_dir)
  
  # filter data so observed data only includes up until 2022-12-31 (earlier dates like 2018 won't
  # matter b/c they are filtered out later, but I don't want dates the surrogate was NOT trained on to 
  # be in the observed data -- basically, we can't use observations in the 'future' when validating,
  # b/c that would be cheating
  obs_data = dplyr::filter(obs_data, date >= as.Date("2020-09-01"))
  obs_data = dplyr::filter(obs_data, date <= as.Date(new_date))
  obs_data = arrange(obs_data, date)

  #obs_df = make_obs_data(obs_data, obs_depth=1, lookback = 4)
  
  train_dates[length(train_dates)+1] = new_date

  horizon=1:30
  obs_List = list()
  counter = 1
  for(h in horizon){
   # print(h)
    for (i in 1:length(train_dates)){
      
      start_date = train_dates[i]
      
      start_date = as.Date(start_date) 
      # grab DOY that corresponds to the start date
      start_DOY = dplyr::filter(obs_data, date == start_date)$DOY[1]
      
      # grab the OBSERVED XX-day head 'forecast' for a given start date
      end_date = start_date + lubridate::days(h)
      
      # filter so the dataset only contains the date (end date) , temperature, depth
      temp_file = dplyr::filter(obs_data, date == end_date) %>%
        dplyr::select(date, temp_obs, depth_int)
      
      # get the correct temperature input value
      #start_date_obs_temp = get_obs_temp(start_date, obs_depth, obs_df = obs_df)
      
      if (nrow(temp_file)==0){
        next
      }
      # add DOY so we can merge w/ surrogate data
      temp_file$DOY = start_DOY
      temp_file$Horizon = h
      #temp_file$Temp_covar = start_date_obs_temp
      temp_file$start_date = start_date
      obs_List[[counter]] = temp_file  
      counter = counter + 1
    }
  }
  
  obs_DF = data.table::rbindlist(obs_List)
  
  colnames(obs_DF)[3] = "Depth"
  colnames(obs_DF)[1] = "end_date"
  obs_DF$start_date = as.character(obs_DF$start_date)

  # Now, merge w/ mean from surrogate
  newdf = right_join(surrogate_preds, obs_DF, by = c("Depth", "DOY", "Horizon", "start_date"))
  
  # calculate bias -> observed - surrogate
  # Mean is mean of y_hat, GP surrogate for GLM
  newdf$bias = newdf$temp_obs - newdf$Mean

  newdf = newdf %>% dplyr::select(DOY, Depth ,Horizon, Temp_covar, start_date, end_date, temp_obs, bias)

  newdf = newdf[complete.cases(newdf), ]
  write.csv(newdf, file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  
}

make_obs_testing_data = function(new_date, obs_depth=1, lookback = 4, obs_data){

  start_date = as.Date(new_date) 
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  obs_data = dplyr::filter(obs_data, date <= (start_date + lubridate::days(30)))
  
  # grab DOY that corresponds to the start date
  start_DOY = dplyr::filter(obs_data, date == start_date)$DOY[1]
  
  counter = 1
  horizon=1:30
  obs_List = list()
  for(h in horizon){
    # grab the OBSERVED XX-day head 'forecast' for a given start date
    end_date = start_date + lubridate::days(h)
    
    # filter so the dataset only contains the date (end date) , temperature, depth
    temp_file = dplyr::filter(obs_data, date == end_date) %>%
      dplyr::select(date, temp_obs, depth_int)
    
    if (nrow(temp_file)==0){
      next
    }
    # add DOY so we can merge w/ surrogate data
    temp_file$DOY = start_DOY
    temp_file$Horizon = h
    # temp_file$Temp_covar = start_date_obs_temp
    obs_List[[counter]] = temp_file  
    counter = counter + 1
  }
  
  obs_DF = data.table::rbindlist(obs_List)
  colnames(obs_DF)[3] = "Depth"
  colnames(obs_DF)[1] = "end_date"
  
  obs_DF = as.data.frame(obs_DF)
  
  obs_DF$start_date = as.character(start_date)

  
  return(obs_DF)
}


# this is how I constructed observed data
# this is the observed data NOTE THAT IT IS 00UTC--NOT AVERAGED!!
#ymd = make_ymd()
#lake_temps <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv")
#lake_temps = dplyr::filter(lake_temps, variable == "temperature")
#lake_temps = filter(lake_temps, depth %in% 0:9)
#newdate = strsplit(as.character(lake_temps$datetime), "-")
#head(newdateDF)
#newdateDF = as.data.frame(do.call("rbind", newdate))
#names(newdateDF) = c("YEAR", "MONTH", "DAY")
#newdateDF$MONTH = sub("^0", "", newdateDF$MONTH)
#newdateDF$DAY = sub("^0", "", newdateDF$DAY)
#newdateDF$YEAR = as.numeric(newdateDF$YEAR)
#newdateDF$MONTH = as.numeric(newdateDF$MONTH)
#newdateDF$DAY = as.numeric(newdateDF$DAY)
#lake_temps = cbind(lake_temps, newdateDF)
#lake_temps = dplyr::select(lake_temps, datetime, depth, observation, YEAR, MONTH, DAY)
#lake_temps = right_join(lake_temps, ymd, by = c("MONTH", "DAY"))
#lake_temps = lake_temps[complete.cases(lake_temps), ]
#colnames(lake_temps)[2] = "depth_int"
#colnames(lake_temps)[3] = "temp_obs"
#lake_temps = dplyr::select(lake_temps, YEAR, MONTH, DAY, depth_int, temp_obs, DOY , datetime)
