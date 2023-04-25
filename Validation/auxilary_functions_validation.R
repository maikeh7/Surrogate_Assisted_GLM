
read_obs_data = function(){
  obs_data = read.csv("/home/maike/GP_surrogate_code/DATA/Bias_dataset/Observed_data.csv")
  obs_data$date = as.POSIXct(obs_data$datetime, tz = "UTC")
  obs_data$X = NULL
  return(obs_data)
}



read_surrogate_preds= function(surrogate_dir){
  surrogate_preds = readRDS(file.path(surrogate_dir,"GLM_Surrogate_SK.Rds"))
  return(surrogate_preds)
}

# function to make MONTH/DAY/DOY dataframe for merging w/ other datasets
make_ymd = function(){
  month_table = data.frame(MONTH = 1:12, freq = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  MONTH = rep(1:12, month_table$freq)
  DAY = unlist(Map(function(start_num, stop_num) seq(start_num,stop_num ),1,month_table$freq ))
  DOY = 1:366
  ymd = data.frame(MONTH = MONTH, DAY = DAY, DOY = DOY)
}


get_obs_temp = function(actual_start, obs_depth=1, obs_df){
  df1 = filter(obs_df, date == actual_start)
  while(nrow(df1) == 0){
    #print("bad")
    
    actual_start = actual_start - lubridate::days(1)
    
    df1 = dplyr::filter(obs_df, date == actual_start)
    
  }
  
  start_date_obs_temp = dplyr::filter(df1, depth_int %in% c(obs_depth, 0))
  return(mean(start_date_obs_temp$mean_temp))
}

make_obs_data = function(obs_data, obs_depth=1, lookback = 4){
  d1 = filter(obs_data, depth_int %in% c(obs_depth,0))
  dates = unique(d1$date)
  mydates = dates[(lookback+1):length(dates)]
  ave_df = list()
  for (j in 1:length( mydates)){
    
    cur_date = mydates[j]
    
    previous_start = cur_date - lubridate::days(lookback)
    
    obs_chunk = filter(d1, (date >= previous_start) & (date <= cur_date))
    previous_ave = obs_chunk %>% group_by(depth_int) %>%
      summarize(mean_temp = mean(temp_obs, na.rm = TRUE))
    tempdf = data.frame(cbind(previous_ave,  cur_date))
    colnames(tempdf)[3] = "date"
    ave_df[[j]] = tempdf
  }
  obs_df = data.table::rbindlist(ave_df)
  return(obs_df)
}

# function to update observed data. 
# will update to whatever current day is.
update_observed_data = function(){
  ymd = make_ymd()
  lake_temps <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv")
  lake_temps = lake_temps[lake_temps$variable == "temperature", ]
  lake_temps = lake_temps[lake_temps$depth %in% 0:9, ]
  newdate = strsplit(as.character(lake_temps$datetime), "-")
  
  newdateDF = as.data.frame(do.call("rbind", newdate))
  names(newdateDF) = c("YEAR", "MONTH", "DAY")
  
  newdateDF$MONTH = sub("^0", "", newdateDF$MONTH)
  newdateDF$DAY = sub("^0", "", newdateDF$DAY)
  newdateDF$YEAR = as.numeric(newdateDF$YEAR)
  newdateDF$MONTH = as.numeric(newdateDF$MONTH)
  newdateDF$DAY = as.numeric(newdateDF$DAY)
  lake_temps = cbind(lake_temps, newdateDF)
  lake_temps = lake_temps[ , c("datetime", "depth", "observation", "YEAR", "MONTH", "DAY")] 
  lake_temps = right_join(lake_temps, ymd, by = c("MONTH", "DAY"))
  lake_temps = lake_temps[complete.cases(lake_temps), ]
  colnames(lake_temps)[2] = "depth_int"
  colnames(lake_temps)[3] = "temp_obs"
  lake_temps = lake_temps[ , c("YEAR", "MONTH", "DAY", "depth_int", "temp_obs", "DOY" , "datetime")]
  write.csv(lake_temps, "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/Observed_data.csv")
}