

#####################################################################################################
# fit surrogate to GLM simulations using stochastic kriging
# fit mean and variance separately, then combine
# note that in the calculation of variance, we divide by 30 b/c there are 30
# reps of GLM sims for each combo of depth,doy,horizon, temp covariate
# this is to ensure that we are not including the nugget, which would give us error bars that are too wide
#####################################################################################################

fit_GLM_surrogate=function(method="Average", lookback=4, obs_depth=1,horizon_dir, surrogate_dir,
                           num_reps = 31){
  
  # make the training dataset
  biglist = list(length=30)
  for (i in 1:30){
    #fix this file path
    temp = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                              "D", obs_depth, ".csv")))
    biglist[[i]] = temp
  }
  bigdf = data.table::rbindlist(biglist)
  bigdf = as.data.frame(bigdf)
  # get one dataset from the 30 horizon datasets. Will be used to 
  # make the prediction grid. Not most elegant solution but couldn't think quickly of another way
  df30 = dplyr::filter(bigdf, horizon==30)
  dummy_data = df30 %>% group_by(depth_int, DOY, start_date_obs_temp, start_date) %>%
    summarise(meanTemp = mean(Temp_C_00UTC))
  dummy_data$meanTemp = NULL
  
  # calculate means for stochastic kriging
  my_means = bigdf %>% group_by(DOY, depth_int, horizon, start_date_obs_temp) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC))

  # normalize response (temperature)
  temp_mean = mean(my_means$mean_temp)
  temp_sd = sd(my_means$mean_temp)
  temp_normalized = (my_means$mean_temp - temp_mean ) / temp_sd

  # do same for variance
  # note we divide by 30 b/c we have 30 reps for each combo of doy/depth/horizon/start_date_temp_obs
  my_Vardf = bigdf %>% group_by(DOY, depth_int, horizon, start_date_obs_temp) %>% 
    summarise(mean_var = log(var(Temp_C_00UTC)/num_reps))
  var_mean = mean(my_Vardf$mean_var)
  var_sd = sd(my_Vardf$mean_var)
  var_normalized = (my_Vardf$mean_var - var_mean ) / var_sd

  # standardize inputs to be in [0,1]
  DOY_range = range(my_means$DOY)
  depth_range = range(my_means$depth_int)
  horizon_range = range(my_means$horizon)
  tempcovar_range = range(my_means$start_date_obs_temp)

  DOY_standardized <- (my_means$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  depth_standardized <- (my_means$depth_int - depth_range[1]) / (depth_range[2] - depth_range[1]) 
  horizon_standardized <- (my_means$horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
  tempcovar_standardized <- (my_means$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1]) 


  X <- cbind(DOY_standardized, depth_standardized, horizon_standardized, tempcovar_standardized)
  Y <- temp_normalized
  Z <- var_normalized

  # Make prediction grid
  # standardize prediction locations to be in [0,1]
  pred_df = data.frame(DOY =rep(1:366, 10), 
                       depth_int=rep(0:9, each=366),
                       horizon=rep(1:30, each = 3660))
  # right join to get the temp 'covariate' and start_date that will be used to merge on later
  pred_times = right_join(pred_df, dummy_data, by = c("DOY", "depth_int"))
  start_dates = pred_times$start_date
  
  pred_times = as.matrix(pred_times[,1:4])
  
  pred_times_scaled = pred_times
  pred_times_scaled[, 1] <- (pred_times_scaled[,1] - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  pred_times_scaled[, 2] <- (pred_times_scaled[,2] - depth_range[1]) / (depth_range[2] - depth_range[1])
  pred_times_scaled[, 3] <- (pred_times_scaled[,3]- horizon_range[1]) / (horizon_range[2] - horizon_range[1])
  pred_times_scaled[, 4] <- (pred_times_scaled[,4] - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])

  # size of conditioning sets for scaled Vecchia
  m <- 30

  # fit scaled vecchia using the sourced code from Katzfuss et al.
  # I give nu a value of 3.5 (to ensure smoothness)
  print("fitting mean")
  est <- fit_scaled(Y, X,  ms = m, nug = NULL, trend = "zero")
  print("cov params: var, ranges, nug")
  est$covparms
  print("info matrix")
  est$info
  
  saveRDS(est, file.path(surrogate_dir, "SK_Mean_fit.Rds"))

  # make predictions
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- fit$means * temp_sd + temp_mean
  s2_orig <- fit$vars * (temp_sd^2)

  # make data.frame so we can plot stuff
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  
  colnames(vecDF) <- c("DOY", "Depth", "Horizon", "Temp_covar", "Mean", "Var")
  
  vecDF$SD <- sqrt(vecDF$Var)
  vecDF$Lower <- qnorm(0.05, vecDF$Mean, vecDF$SD)
  vecDF$Upper <- qnorm(0.95, vecDF$Mean, vecDF$SD)
  vecDF$start_date = as.character(start_dates)
  vecdf_mean = vecDF
  
  ##############################################################################################
  ##############################################################################################
  # fit variance
  # I give nu a value of 3.5
  est <- fit_scaled(Z, X,  ms = m, nug = NULL, trend = "zero")
  saveRDS(est, file.path(surrogate_dir, "SK_Variance_fit.Rds"))
  # make predictions
  # we don't need the variance for anything, only the mean, which is our estimation of variance
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- exp( fit$means * var_sd + var_mean )
  s2_orig <- (exp( fit$vars * (var_sd^2) ))

  # make data.frame with important quantities
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  colnames(vecDF) <- c("DOY", "Depth", "Horizon", "Temp_covar", "Mean", "Var")
  vecDF$SD <- sqrt(vecDF$Var)
  vecDF$Lower <- qnorm(0.05, vecDF$Mean, vecDF$SD)
  vecDF$Upper <- qnorm(0.95, vecDF$Mean, vecDF$SD)
 
  # the mean from the variance GP is actually the estimated variance, so we need to take sqrt to get SD
  vecdf_mean$SD_SK = sqrt(vecDF$Mean)
  vecdf_mean$Var_SK = vecDF$Mean
  vecdf_mean$HetLower <- qnorm(0.05, vecdf_mean$Mean, vecdf_mean$SD_SK)
  vecdf_mean$HetUpper <- qnorm(0.95, vecdf_mean$Mean, vecdf_mean$SD_SK)
  vecdf_mean$Lower = NULL
  vecdf_mean$Upper = NULL
  saveRDS(vecdf_mean, file.path(surrogate_dir, "GLM_Surrogate_SK.Rds"))
}

#####################################################################################################
# fit surrogate to GLM simulations by depth, so independent GPs for each depth
#####################################################################################################

#####################################################################################################
# fit surrogate to GLM simulations by depth, so independent GPs for each depth
#####################################################################################################

fit_GLM_surrogate_byDepth=function(method="Average", 
                                   lookback=4,
                                   obs_depth=1,
                                   horizon_dir,
                                   surrogate_dir,
                                   num_reps = 1){
  
  # make the training dataset
  biglist = list(length=30)
  for (i in 1:30){
    #fix this file path
    temp = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                           "D", obs_depth, ".csv")))
    biglist[[i]] = temp
  }
  bigdf = data.table::rbindlist(biglist)
  bigdf = as.data.frame(bigdf)
  
  results_list = list(length=10)
 
  for (i in 0:9){
    print(i)
    mydepth = i
  # get one dataset from the 30 horizon datasets. Will be used to 
  # make the prediction grid. Not most elegant solution but couldn't think quickly of another way
  df30 = dplyr::filter(bigdf, horizon==30, depth_int == mydepth)
  
  dummy_data = df30 %>% group_by(DOY, start_date_obs_temp, start_date) %>%
    summarise(meanTemp = mean(Temp_C_00UTC))
  dummy_data$meanTemp = NULL
  
  # calculate means for stochastic kriging
  my_means = filter(bigdf, depth_int == mydepth) %>% group_by(DOY, horizon, start_date_obs_temp) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC))
  
  # normalize response (temperature)
  temp_mean = mean(my_means$mean_temp)
  temp_sd = sd(my_means$mean_temp)
  temp_normalized = (my_means$mean_temp - temp_mean ) / temp_sd
  
  # do same for variance
  # note we divide by 30 b/c we have 30 reps for each combo of doy/depth/horizon/start_date_temp_obs
  my_Vardf = filter(bigdf, depth_int == mydepth) %>% group_by(DOY, horizon, start_date_obs_temp) %>% 
    summarise(mean_var = log(var(Temp_C_00UTC)/num_reps))
  var_mean = mean(my_Vardf$mean_var)
  var_sd = sd(my_Vardf$mean_var)
  var_normalized = (my_Vardf$mean_var - var_mean ) / var_sd
  
  # standardize inputs to be in [0,1]
  DOY_range = range(my_means$DOY)
  horizon_range = range(my_means$horizon)
  tempcovar_range = range(my_means$start_date_obs_temp)
  
  DOY_standardized <- (my_means$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  horizon_standardized <- (my_means$horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
  tempcovar_standardized <- (my_means$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1]) 
  
  
  X <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized)
  Y <- temp_normalized
  Z <- var_normalized
  
  # Make prediction grid
  # standardize prediction locations to be in [0,1]
  pred_df = data.frame(DOY =rep(1:366, 30), horizon=rep(1:30, each = 366))
  # right join to get the temp 'covariate' and start_date that will be used to merge on later
  
  pred_times = right_join(pred_df, dummy_data, by = "DOY")
  pred_times = pred_times[complete.cases(pred_times), ]
  start_dates = pred_times$start_date
 
  pred_times = as.matrix(pred_times[,1:3])
  
  pred_times_scaled = pred_times
  pred_times_scaled[, 1] <- (pred_times_scaled[,1] - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  pred_times_scaled[, 2] <- (pred_times_scaled[,2] - horizon_range[1]) / (horizon_range[2] - horizon_range[1])
  pred_times_scaled[, 3] <- (pred_times_scaled[,3] - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
  
  # size of conditioning sets for scaled Vecchia
  m <- 30
  
  # fit scaled vecchia using the sourced code from Katzfuss et al.
  # I give nu a value of 3.5 (to ensure smoothness)
  print("fitting mean - GLM")
  est <- fit_scaled(Y, X,  ms = m, nug = NULL, trend = "zero",  nu=3.5)
  print("cov params: var, ranges, nug")
  print(est$covparms)
  
  # make predictions
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- fit$means * temp_sd + temp_mean
  s2_orig <- fit$vars * (temp_sd^2)
  
  # make data.frame so we can plot stuff
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  
  colnames(vecDF) <- c("DOY", "Horizon", "Temp_covar", "Mean", "Var")
  
  vecDF$SD <- sqrt(vecDF$Var)
  vecDF$Lower <- qnorm(0.05, vecDF$Mean, vecDF$SD)
  vecDF$Upper <- qnorm(0.95, vecDF$Mean, vecDF$SD)
  vecDF$start_date = as.character(start_dates)
  vecdf_mean = vecDF
  
  ##############################################################################################
  ##############################################################################################
  # fit variance
  # I give nu a value of 3.5
  # nug = 1e-5
  print("fitting variance - GLM")
  est <- fit_scaled(Z, X,  ms = m, nug = NULL, trend = "zero",  nu=3.5)
  print("cov params: var, ranges, nug")
  print(est$covparms)

  # make predictions
  # we don't need the variance for anything, only the mean, which is our estimation of variance
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- exp( fit$means * var_sd + var_mean )
  s2_orig <- (exp( fit$vars * (var_sd^2) ))
  
  # make data.frame with important quantities
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  colnames(vecDF) <- c("DOY", "Horizon", "Temp_covar", "Mean", "Var")
  vecDF$SD <- sqrt(vecDF$Var)
  vecDF$Lower <- qnorm(0.05, vecDF$Mean, vecDF$SD)
  vecDF$Upper <- qnorm(0.95, vecDF$Mean, vecDF$SD)
  
  # the mean from the variance GP is actually the estimated variance, so we need to take sqrt to get SD
  vecdf_mean$SD_SK = sqrt(vecDF$Mean)
  vecdf_mean$Var_SK = vecDF$Mean
  vecdf_mean$HetLower <- qnorm(0.05, vecdf_mean$Mean, vecdf_mean$SD_SK)
  vecdf_mean$HetUpper <- qnorm(0.95, vecdf_mean$Mean, vecdf_mean$SD_SK)
  vecdf_mean$Lower = NULL
  vecdf_mean$Upper = NULL
  vecdf_mean$Depth = i
  results_list[[(i+1)]] = vecdf_mean
  
  }
  vecdf_mean = rbindlist(results_list)
  saveRDS(vecdf_mean, file.path(surrogate_dir, "GLM_Surrogate_SK.Rds"))
}


