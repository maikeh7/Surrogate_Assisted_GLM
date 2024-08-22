#' this function fits a GP to bias data via Vecchia approximation (no stochastic kriging)
#'
#' @param method method to obtain value of phi: "Average" (denotes average of 4 days of observed data prior to a given 
#' reference date). Do not change.
#' @param lookback 4 (do not change)
#' @param obs_depth 1 (do not change)
#' @param horizon_dir directory where train data lives (default is HORIZON_TRAIN) 
#' @param surrogate_dir directory where all surrogates will be saved to (default is SURROGATES)
#'
#' @return data.frames containing the model fit object (bias_fit.Rds) and predictions/sd's (bias_surrogate.Rds)
#' are saved to surrogate_dir
#' @export
#'
#' @examples
fit_bias_surrogate = function(method="Average",
                              lookback=4,
                              obs_depth=1,
                              horizon_dir="HORIZON_TRAIN",
                              surrogate_dir="SURROGATES"){
    # get one dataset from the 30 horizon datasets. Will be used to 
    # make the prediction grid. Not most elegant solution but couldn't think quickly of another way
    df30=read.csv(file.path(horizon_dir, paste0("BigDF_H", 30, "_TRAIN", method, lookback,
                                                "D", obs_depth, ".csv")))
    
    colnames(df30)[which(colnames(df30) == "depth_int")] = "Depth"
    colnames(df30)[which(colnames(df30) == "horizon")] = "Horizon"
    
    dummy_data = df30 %>% group_by(Depth, DOY, start_date_obs_temp, start_date) %>%
      summarise(meanTemp = mean(Temp_C_00UTC))
    
    dummy_data$meanTemp = NULL
    
    bias_data = as.data.frame(data.table::fread(file.path(surrogate_dir, "Bias_dataset_validation.csv")))
    
    bias_data = bias_data[complete.cases(bias_data), ]
    
    pred_df = data.frame(DOY =rep(1:366, 10), 
                         Depth=rep(0:9, each=366),
                         Horizon=rep(1:30, each = 3660))
    
    # right join to get the temp 'covariate'
    pred_times = right_join(pred_df, dummy_data, by = c("DOY", "Depth"))
    start_dates = pred_times$start_date
    
    pred_times = as.matrix(pred_times[,1:4])
    
  # normalize response (bias)
  bias_mean = mean(bias_data$bias)
  bias_sd = sd(bias_data$bias)
  bias_normalized = (bias_data$bias - bias_mean ) / bias_sd
  
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
  #pred_times = cbind(rep(1:366, 10), rep(0:9, each=366), rep(1:30, each = 3660))
  pred_times_scaled = pred_times
  pred_times_scaled[, 1] <- (pred_times_scaled[,1] - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  pred_times_scaled[, 2] <- (pred_times_scaled[,2] - depth_range[1]) / (depth_range[2] - depth_range[1])
  pred_times_scaled[, 3] <- (pred_times_scaled[,3]- horizon_range[1]) / (horizon_range[2] - horizon_range[1])
  pred_times_scaled[, 4] <- (pred_times_scaled[,4] - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
  
  
  # size of conditioning sets
  m <- 40
  
  # fit scaled vecchia using the sourced code 
  est <- fit_scaled(Y, X,  ms = m, nug = NULL, trend = "zero")
  saveRDS(est, file.path(surrogate_dir, "bias_fit.Rds"))

  # make predictions
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- fit$means * bias_sd + bias_mean
  s2_orig <- fit$vars * (bias_sd^2)
  print("fit bias GP")
  # make data.frame with important quantities
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  colnames(vecDF) <- c("DOY", "Depth", "Horizon", "Temp_covar", "BiasMean", "BiasVar")
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
