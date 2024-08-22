
# run this if you want to update the observed data
update_observed_data = function(){
  # make_ymd = function(){
  #   month_table = data.frame(MONTH = 1:12, freq = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  #   MONTH = rep(1:12, month_table$freq)
  #   DAY = unlist(Map(function(start_num, stop_num) seq(start_num,stop_num ),1,month_table$freq ))
  #   DOY = 1:366
  #   ymd = data.frame(MONTH = MONTH, DAY = DAY, DOY = DOY)
  # }
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
  #write.csv(lake_temps, "home/maike/GP_surrogate_code/DATA/Bias_dataset/Observed_data.csv")
  write.csv(lake_temps, "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/Observed_data.csv")
}
# update observed data
# if you get an error, it could be that the cloud storage destination has changed
# please contact Quinn Thomas: rqthomas@vt.edu
update_observed_data()


read_obs_data = function(){
  obs_data = read.csv("C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/Observed_data.csv")
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

# fit GP to observations
library(hetGP)
ModelMat = cbind(obs$DOY, obs$depth_int)
Z = obs$temp_obs
start=Sys.time()
het_depth <- mleHetGP(ModelMat, Z, covtype =  "Gaussian")
stop = Sys.time()
print(stop-start)

het_depth_rebuild = rebuild(het_depth, robust = TRUE)
pred_times = cbind(rep(1:366, 10), rep(0:9, each=366))

preds = predict(x = pred_times, object = het_depth_rebuild)

colnames(obs)[4] = "Depth"
mean_trend = preds$mean
temp_sd = sqrt(preds$sd2 + preds$nugs)
pred_times = data.frame(pred_times)
colnames(pred_times) = c("DOY", "Depth")
pred_times$mean = mean_trend
pred_times$hetSD = temp_sd
pred_times$Lower <- qnorm(0.05, pred_times$mean, pred_times$hetSD)
pred_times$Upper <- qnorm(0.95, pred_times$mean, pred_times$hetSD)
ggplot(pred_times, aes(x = DOY, y = mean)) +
  geom_point(data = obs, aes(x = DOY, y = temp_obs), col = "gray", size = .6) +
  geom_line(aes(x = DOY, y = mean)) +
  geom_line(aes(x = DOY, y = Lower), linetype = "dashed") +
  geom_line(aes(x = DOY, y = Upper), linetype = "dashed") +
  theme_bw() +
  ylab("Water temperature °C") +
  facet_wrap(~factor(Depth), nrow = 2, ncol = 5)
ggsave(file.path(plot_dir, "ObservedGP.png"))
library(ggplot2)

obs=read_obs_data()
obs2 = filter(obs, date >= as.Date("2022-06-10") & date <= as.Date("2023-06-11"))
colnames(obs2)[4] = "Depth"
head(obs2)
obs2 = obs2[, c("YEAR", "MONTH", "DAY", "Depth", "temp_obs", "DOY", "date")]
pred_times2= right_join(pred_times, obs2, by = c("DOY", "Depth"))
pred_times2 = pred_times2[complete.cases(pred_times2), ]
get_rmse(pred_times2$temp_obs, pred_times2$mean)
write.csv(pred_times2, "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/Observed_hetGP_withOb_TESTONLY.csv")
head(pred_times2)

# now, join observations with preds from GP model
obs=read_obs_data()
obs2 = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2023-06-11"))
colnames(obs2)[4] = "Depth"
obs2 = obs2[, c("YEAR", "MONTH", "DAY", "Depth", "temp_obs", "DOY", "date")]
head(obs2)
pred_times2= right_join(pred_times, obs2, by = c("DOY", "Depth"))
pred_times2 = pred_times2[complete.cases(pred_times2), ]
write.csv(pred_times2, "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/Observed_hetGP_withOb_TRAINONLY.csv")
head(pred_times2)

###################################################
# construct GLM data
###################################################
biglist = list(length=30)
horizon_dir = "HORIZON_TRAIN"
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

obs = read_obs_data()
obs2 = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2023-06-11"))
obs2$datetime = NULL
obs2$datetime = obs2$date
obs2$date = NULL
colnames(obs2)[4] = "Depth"
obs2 = dplyr::select(obs2, Depth, temp_obs, datetime)
obs2$datetime = as.character(obs2$datetime)
head(bigdf)
glmmeans = bigdf %>% group_by(Depth, Horizon, datetime, DOY) %>% summarise(GLM_mean = mean(Temp_C_00UTC), 
                                                                      GLM_std = sd(Temp_C_00UTC))
glmmeans$datetime = as.character(glmmeans$datetime)
head(glmmeans)
glmv2 = right_join(glmmeans, obs2, by = c("Depth", "datetime"))
glmv2 = glmv2[complete.cases(glmv2), ]
mydate = glmv2$datetime
mydate = as.Date(mydate)
tail(mydate)
glmv2$date = mydate
head(glmv2)
write.csv(glmv2, "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/rawGLM_withObs.csv")

#####################
# junk
#####################
head(pred_times2)
head(obs2)
head(obs)
tail(obs)
head(pred_times)

obs=read_obs_data()
head(obs)
obs = filter(obs, date >= as.Date("2019-12-30"))
colnames(obs)[4] = "Depth"
alldates = unique(obs$date)
past_date = obs$date[1]
past_data = filter(obs, date == past_date)
past_data

biglist = list(length = length(alldates))
counter=1

for (i in 2:length(alldates)){
  past_date = alldates[i-1]
  past_data = filter(obs, date == past_date) %>% dplyr::select(Depth, temp_obs)
  cur_date = alldates[i]
  cur_data = filter(obs, date == cur_date) %>% dplyr::select(YEAR, MONTH, DAY, Depth, DOY, date)
  pred_data = right_join(cur_data, past_data, by = "Depth")
  biglist[[counter]] = pred_data
  counter = counter + 1
}
persist_preds = data.table::rbindlist(biglist)
head(persist_preds)
colnames(persist_preds)[7] = "pred"
persist_preds = dplyr::select(persist_preds, Depth, date, pred)
str(persist_preds)
persist_preds = right_join(persist_preds, obs, by = c("Depth", "date"))
persist_preds = persist_preds[complete.cases(persist_preds), ]
get_rmse(persist_preds$temp_obs, persist_preds$pred)
i=1
head(mydates)
alldates = unique(obs1$date)
mydates = alldates[8:length(alldates)]
tail(mydates)
mydates[1] - lubridate::days(7)
biglist = list(length = length(mydates))
for (i in 1:length(mydates)){
  
  curdate = mydates[i]
  curtemp = filter(obs1, date == curdate)$temp_obs
  
  prevdate = curdate - lubridate::days(1)
  prev7date = curdate - lubridate::days(7)
  prevdata = filter(obs1, (date >= prev7date) & (date <= prevdate))
  if (nrow(prevdata) ==0){
    next
  }
  prevdata = dplyr::select(prevdata, temp_obs)
  prevdata$T = rev(1:nrow(prevdata))
  prevdata$Temp = curtemp
  prevdata$date = curdate
  biglist[[i]] = prevdata
}
mydat = data.table::rbindlist(biglist)
head(mydat, n=8)
mydat

plot(mydat$Temp, mydat$temp_obs)
i=397
