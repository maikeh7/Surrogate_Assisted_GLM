
fit_bias_surrogate = function(method="Average", lookback=4, obs_depth=1,
horizon_dir="HORIZON_TRAIN", surrogate_dir="SURROGATES"){
  # get one dataset from the 30 horizon datasets. Will be used to 
  # make the prediction grid. Not most elegant solution but couldn't think quickly of another way
  df30=read.csv(file.path(horizon_dir, paste0("BigDF_H", 30, "_TRAIN", method, lookback,
                                              "D", obs_depth, ".csv")))

  dummy_data = df30 %>% group_by(depth_int, DOY, start_date_obs_temp, start_date) %>%
    summarise(meanTemp = mean(Temp_C_00UTC))
  
  dummy_data$meanTemp = NULL
  
  bias_data = as.data.frame(data.table::fread(file.path(surrogate_dir, "Bias_dataset_validation.csv")))
  
  bias_data = bias_data[complete.cases(bias_data), ]
 
  # normalize response (bias)
  bias_mean = mean(bias_data$bias)
  bias_sd = sd(bias_data$bias)
  bias_normalized = (bias_data$bias - bias_mean ) / bias_sd

  # standardize inputs to be in [0,1]
  # this is just so you know what I did! And also so I remember...
  # standardize inputs to be in [0,1]
  DOY_range = range(bias_data$DOY)
  depth_range = range(bias_data$Depth)
  horizon_range = range(bias_data$Horizon)
  tempcovar_range = range(bias_data$Temp_covar)
  
  DOY_standardized <- (bias_data$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  depth_standardized <- (bias_data$Depth - depth_range[1]) / (depth_range[2] - depth_range[1]) 
  horizon_standardized <- (bias_data$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
  tempcovar_standardized <- (bias_data$Temp_covar - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1]) 
  
  X <- cbind(DOY_standardized, depth_standardized, horizon_standardized, tempcovar_standardized)
  Y <- bias_normalized
  
  # standardize prediction locs to be in [0,1]
  # this is just so you know what I did
  pred_df = data.frame(DOY =rep(1:366, 10), 
                                    depth_int=rep(0:9, each=366),
                                    horizon=rep(1:30, each = 3660))
  # right join to get the temp 'covariate'
  pred_times = right_join(pred_df, dummy_data, by = c("DOY", "depth_int"))
  start_dates = pred_times$start_date
  
  pred_times = as.matrix(pred_times[,1:4])
  #pred_times = cbind(rep(1:366, 10), rep(0:9, each=366), rep(1:30, each = 3660))
  pred_times_scaled = pred_times
  pred_times_scaled[, 1] <- (pred_times_scaled[,1] - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  pred_times_scaled[, 2] <- (pred_times_scaled[,2] - depth_range[1]) / (depth_range[2] - depth_range[1])
  pred_times_scaled[, 3] <- (pred_times_scaled[,3]- horizon_range[1]) / (horizon_range[2] - horizon_range[1])
  pred_times_scaled[, 4] <- (pred_times_scaled[,4] - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
  
  
  # size of conditioning sets
  m <- 30
  
  # fit scaled vecchia using the sourced code 
  # I give nu a value of 3.5
  start=Sys.time()
  est <- fit_scaled(Y, X,  ms = m, nug = NULL, trend = "zero",  nu=3.5)
  saveRDS(est, file.path(surrogate_dir, "bias_fit.Rds"))
  stop=Sys.time()
  print(stop-start)

  # make predictions
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- fit$means * bias_sd + bias_mean
  s2_orig <- fit$vars * (bias_sd^2)
  
  # make data.frame so we can plot stuff
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  colnames(vecDF) <- c("DOY", "Depth", "Horizon", "Temp_covar", "Mean", "Var")
  vecDF$SD <- sqrt(vecDF$Var)
  vecDF$Lower <- qnorm(0.05, vecDF$Mean, vecDF$SD)
  vecDF$Upper <- qnorm(0.95, vecDF$Mean, vecDF$SD)
  vecDF$start_date = start_dates

  

saveRDS(vecDF, file.path(surrogate_dir, "bias_surrogate.Rds"))

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
