library(dplyr)
library(data.table)

read_obs_data = function(){
  #obs_data = read.csv("C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/Observed_data.csv")
  obs_data = read.csv("../Data/Observed_data.csv")
  obs_data$date = as.POSIXct(obs_data$datetime, tz = "UTC")
  obs_data$X = NULL
  return(obs_data)
}
# check
obs=read_obs_data()
tail(obs)

# filter to only train on train data!
obs = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2023-06-10"))
#obs = filter(obs, date <= as.Date("2022-03-13"))
head(obs)
colnames(obs)[4] = "Depth"
tail(obs)

###################################################
# construct GLM data
###################################################
biglist = list(length=30)
horizon_dir = "../Data/HORIZON_TRAIN"
method = "Average"
lookback = 4
obs_depth = 1
for (i in 1:30){
  #fix this file path
  temp = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                         "D", obs_depth, ".csv")))
  biglist[[i]] = temp
}

bigdf = data.table::rbindlist(biglist)
bigdf = as.data.frame(bigdf)
colnames(bigdf)[which(colnames(bigdf) == "depth_int")] = "Depth"
colnames(bigdf)[which(colnames(bigdf) == "horizon")] = "Horizon"
tail(bigdf)
bigdf$datetime = as.POSIXct( bigdf$start_date ) + lubridate::days(bigdf$Horizon)
bigdf$V1=NULL

# filter observed data
obs = read_obs_data()
obs2 = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2023-06-11"))
obs2$datetime = NULL
obs2$datetime = obs2$date
obs2$date = NULL
colnames(obs2)[4] = "Depth"
obs2 = dplyr::select(obs2, Depth, temp_obs, datetime)
obs2$datetime = as.character(obs2$datetime)

bigdf$datetime = as.character(bigdf$datetime)
str(bigdf)
glmv2 = right_join(bigdf, obs2, by = c("Depth", "datetime"))
glmv2 = glmv2[complete.cases(glmv2), ]

#mydate = glmv2$datetime
#mydate = as.Date(mydate)
#glmv2$date = mydate
#head(glmv2)
write.csv(glmv2, "rawGLM_withObs_reps.csv")
