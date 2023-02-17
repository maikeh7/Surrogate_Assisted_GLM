library(hetGP)
library(dplyr)
library(lubridate)

# Here I construct an observed dataset for each horizon, then merge w/ GLM surrogate and calculate bias
# see below for how to get observed data 
obs_data = read.csv("DATA/Bias_dataset/Observed_data_2018_2022.csv")
obs_data$X = NULL

# filter data so observed data only includes up until 2022-12-31 (earlier dates like 2018 won't
# matter b/c they are filtered out later, but I don't want dates the surrogate was NOT trained on to 
# be in the observed data
# start_train = as.Date("2020-10-03")
end_train = as.Date("2022-12-31")
obs_data = dplyr::filter(obs_data, datetime <= end_train)


# file path to GLM simulations
mypath = "DATA/GLM_sims"
all_files = file.path(mypath, list.files(mypath) )
file_len = length(all_files)
# These are all the dates in the training period
all_files = all_files[1:file_len] # adjust once I know what we are designating as test period

# grab all the dates in the training set
# updated so you can put in the full file path and it will only extract dates: yyyy-mm-dd
mydate_2 = gsub(".+([0-9]{4}-[0-9]{1,2}-[0-9]{1,2}).+", "\\1", all_files)
print(head(mydate_2))
# these are GLM surrogate preds for all training data, variance adjusted via stochastic kriging
surrogate_preds = readRDS("SVecchia/Vecchia_results/GLM_surrogate_vecchia.Rds")
surrogate_preds$YEAR = 2020
# go thru all horizons and all dates in training data
# bias will be OBSERVED - SURROGATE
# there may be duplicate bias for depth/DOY combos but that is fine
make_bias_horizon_datasets = function(horizons=1:30, surrogate_preds, train_dates, obs_data){
  # read in the correct dataset of predictions from our surrogate
  # process observed data so we get the correct dataset
  obs_DF = data.frame()
  for(h in horizons){
    for (i in train_dates){
      start_date = i
      start_date = as.Date(start_date)
      # grab DOY that corresponds to the start date
      start_DOY = dplyr::filter(obs_data, datetime == start_date)$DOY[1]
      
      # grab the OBSERVED XX-day head 'forecast' for a given start date
      end_date = start_date + lubridate::days(horizons[h])
      
      # filter so the dataset only contains the date (end date) , temperature, depth
      temp_file = dplyr::filter(obs_data, datetime == end_date) %>% dplyr::select(datetime, temp_obs, depth_int, YEAR)
      if (nrow(temp_file)==0){
        next
      }
      # add DOY so we can merge w/ surrogate data
      temp_file$DOY = start_DOY
      temp_file$Horizon = h
      obs_DF = rbind(obs_DF, temp_file)  
    }
  }
  
  colnames(obs_DF)[3] = "Depth"
  colnames(obs_DF)[1] = "end_date"
  
  # Now, merge w/ mean from surrogate
  newdf = right_join(surrogate_preds, obs_DF, by = c("Depth", "DOY", "Horizon"))
  
  # calculate bias -> observed - surrogate
  # Mean is mean of y_hat, GP surrogate for GLM
  newdf$bias = newdf$temp_obs - newdf$Mean
  # Let's save and fit with another script
  write.csv(newdf, "DATA/Bias_dataset/Bias_dataset_train_Svecchia_withYear.csv")
  
}
make_bias_horizon_datasets(surrogate_preds = surrogate_preds, train_dates = mydate_2,
 obs_data = obs_data)
 print("done")
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
