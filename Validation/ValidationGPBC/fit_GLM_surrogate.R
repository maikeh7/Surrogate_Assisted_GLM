

#####################################################################################################
# fit surrogate to GLM simulations using stochastic kriging
# fit mean and variance separately, then combine
# This is for the 'GPC' method
#####################################################################################################

fit_GLM_surrogate=function(method="Average", 
                           lookback=4,
                           obs_depth=1,
                           horizon_dir,
                           surrogate_dir,
                           num_reps){
  q2 = qnorm(0.95) # for variance -- upper 95th quantile
  q = qnorm(0.95) # for PI/CIs
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
  colnames(bigdf)[which(colnames(bigdf) == "depth_int")] = "Depth"
  colnames(bigdf)[which(colnames(bigdf) == "horizon")] = "Horizon"
  
  # get last start_date
  # we do not want to include data corresponding to last start_date
  # want to predict using ONLY past GLM sims
  dates = unique(bigdf$start_date)
  new_date = dates[length(dates)]
  #bigdf_sub = filter(bigdf, !(new_date %in% bigdf$start_date))
  bigdf_sub = bigdf[bigdf$start_date != new_date, ]
  
  # calculate means for stochastic kriging using the subset of glm sims w/o the newest date
  my_means = bigdf_sub %>% group_by(DOY, Depth, Horizon, start_date, start_date_obs_temp) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC))
  vecDF_train = my_means %>% dplyr::select(DOY, Depth, Horizon, start_date_obs_temp, start_date)
    
    # this is for prediction
    my_means_new = bigdf %>% group_by(DOY, Depth, Horizon, start_date, start_date_obs_temp) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC))
    
  
  start_dates = as.character(my_means_new$start_date)
  
  # standardize inputs to be in [0,1]
  DOY_range = range(my_means$DOY)
  depth_range = range(my_means$Depth)
  horizon_range = range(my_means$Horizon)
  tempcovar_range = range(my_means$start_date_obs_temp)
  
  DOY_standardized <- (my_means$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  depth_standardized <- (my_means$Depth - depth_range[1]) / (depth_range[2] - depth_range[1]) 
  horizon_standardized <- (my_means$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
  tempcovar_standardized <- (my_means$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1]) 
  
  #training inputs
  X <- cbind(DOY_standardized, depth_standardized, horizon_standardized, tempcovar_standardized)
  
  DOY_rangeT = range(my_means_new$DOY)
  depth_rangeT = range(my_means_new$Depth)
  horizon_rangeT = range(my_means_new$Horizon)
  tempcovar_rangeT = range(my_means_new$start_date_obs_temp)
  
  DOY_standardizedT <- (my_means_new$DOY - DOY_rangeT[1]) / (DOY_rangeT[2] - DOY_rangeT[1])
  depth_standardizedT <- (my_means_new$Depth - depth_rangeT[1]) / (depth_rangeT[2] - depth_rangeT[1]) 
  horizon_standardizedT <- (my_means_new$Horizon - horizon_rangeT[1]) / (horizon_rangeT[2] - horizon_rangeT[1]) 
  tempcovar_standardizedT <- (my_means_new$start_date_obs_tempT - tempcovar_rangeT[1]) / (tempcovar_rangeT[2] - tempcovar_rangeT[1]) 
  
  vecDF = my_means_new %>% dplyr::select(DOY, Depth, Horizon, start_date_obs_temp, start_date)
  
  # testing inputs to include the new date
  X_test <- cbind(DOY_standardizedT, depth_standardizedT, horizon_standardizedT, tempcovar_standardizedT)


  Y = my_means$mean_temp
  
  # size of conditioning sets for scaled Vecchia
  m <- 40
  
  # fit scaled vecchia using the sourced code from Katzfuss et al.
  est <- fit_scaled(Y, X,  ms = m, nug = NULL)
  
    # make predictions--TRAIN
  fit = predictions_scaled(est, X, m = m, joint = FALSE, predvar = TRUE)
  mean_orig = fit$means 
  
  vecDF_train$Mean <- mean_orig
  colnames(vecDF_train) <- c("DOY", "Depth", "Horizon", "Temp_covar", "start_date", "Mean")
  vecdf_mean_train = vecDF_train
  
  
  # make predictions--TEST
  fit = predictions_scaled(est, X_test, m = m, joint = FALSE, predvar = TRUE)
  mean_orig = fit$means 
  
  vecDF$Mean <- mean_orig
  colnames(vecDF) <- c("DOY", "Depth", "Horizon", "Temp_covar", "start_date", "Mean")
  vecdf_mean = vecDF
  
  ##############################################################################################
  ##############################################################################################
  # calculate variance using the mean of GP fitted to means
  # this incorporates variability in the mean 
  #DOY, Depth, Horizon, start_date_obs_temp, start_date
  meanDF = vecdf_mea_train[ ,c("Horizon", "start_date", "DOY", "Depth", "Mean")]
  # joing with training data only
  meanDF2 = right_join(bigdf_sub, meanDF, by = c("DOY", "Depth", "Horizon", "start_date"))
  
  # now calculate 'variance' using the mean of GP fit, not empirical mean
  # get squared deviations
  meanDF2$sq_devs_smooth = (meanDF2$Temp_C_00UTC - meanDF2$Mean)^2
  
  # get sum of squared deviations
  #DOY, Depth, Horizon, start_date, start_date_obs_temp
  meanDF3 = meanDF2 %>% group_by(DOY, Depth, Horizon, start_date, start_date_obs_temp) %>% 
    summarize(sum_devs = sum(sq_devs_smooth))
  
  # divide by num_reps = 31 for each grouping
  meanDF3$Var = meanDF3$sum_devs / num_reps
  meanDF3$rootVar = sqrt(meanDF3$Var)
  Z =  meanDF3$rootVar 
  
  # fit variance--use training locations
  estvar <- fit_scaled(Z, X,  ms = m, nug = NULL)
  
  saveRDS(estvar, file.path(surrogate_dir, "SK_Variance_fit.Rds"))
  
  # make predictions on test locations (include the new date)
  fitvar <- predictions_scaled(estvar, X_test, m = m, joint = FALSE, predvar = TRUE)
  print("fit GLM surrogate")
  ###########################################################
  # use delta method to get variance
  # upper 95 quantile of variance plus uncertainty in mean
  # d/dx(g(\mu))^2 * var(x) // g(x) = x^2 
  ###########################################################
  #Delta_variance = fitvar$means + q2*sqrt(fitvar$vars)
  Delta_variance = (2*fitvar$means)^2 * fitvar$vars
  #Delta_variance = (1/fitvar$means)^2 * fitvar$vars
  
  # backtransform mean of GP(var) + Q95 (this is the SK variance)
  upper95_var2 = (fitvar$means)^2 + q2*sqrt(Delta_variance)

  # calculate 90% confidence intervals and prediction intervals
  vecdf_mean$Var_SK = upper95_var2
  vecdf_mean$Var_SK_divReps = upper95_var2/num_reps
  vecdf_mean$SD_SK = sqrt(upper95_var2)
  vecdf_mean$SD_SK_divReps = sqrt(upper95_var2/num_reps)
  
  vecdf_mean$HetLower = vecdf_mean$Mean - q*(sqrt(upper95_var2/num_reps))
  vecdf_mean$HetUpper = vecdf_mean$Mean + q*(sqrt(upper95_var2/num_reps))
  
  vecdf_mean$HetLowerPI = vecdf_mean$Mean - q*(sqrt(upper95_var2))
  vecdf_mean$HetUpperPI = vecdf_mean$Mean + q*(sqrt(upper95_var2))
  vecdf_mean$start_date = as.character(vecdf_mean$start_date)
  
  saveRDS(vecdf_mean, file.path(surrogate_dir, "GLM_Surrogate_SK.Rds"))
}



