
fit_my_skv2 = function(traindf, testdf, num_reps, test_reps){
  q2 <- qnorm(0.95)
  q = qnorm(0.975
 
  my_mean_starts = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
    summarise(start_date_obs_temp = mean(start_date_obs_temp))
  
  my_means = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC))
  
  inputsXtrain = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_mean_starts$start_date_obs_temp,
                            DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                            ybar = my_means$mean_temp)
  
  # standardize inputs to be in [0,1]
  DOY_range = range(inputsXtrain$DOY)
  horizon_range = range(inputsXtrain$Horizon)
  depth_range = range(inputsXtrain$Depth)
  tempcovar_range = range(inputsXtrain$start_date_obs_temp)
  
  DOY_standardized <- (inputsXtrain$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  horizon_standardized <- (inputsXtrain$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
  tempcovar_standardized <- (inputsXtrain$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
  depth_standardized <- (inputsXtrain$Depth - depth_range[1]) / (depth_range[2] - depth_range[1])
  
  Xtrain <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized, depth_standardized)
  Ytrain <- my_means$mean_temp
  ###################################################################################################
  my_mean_starts = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
    summarise(start_date_obs_temp = mean(start_date_obs_temp))
  
  my_means = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC))
  
  inputsXTest = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_mean_starts$start_date_obs_temp,
                           DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                           ybar = my_means$mean_temp)
  
  # size of conditioning sets for scaled Vecchia
  m <- 75
  estmean <- fit_scaled(Ytrain, Xtrain,  ms = m, nug = NULL)
  fitmean <- predictions_scaled(estmean, Xtrain, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- fitmean$means 
  
  vecDF = inputsXTest
  vecDF$MyMean = mean_orig
  ##############################################################################################
  ##############################################################################################
  # calculate variance using the mean of GP fitted to means
  meanDF = select(vecDF, Horizon, start_date, DOY, Depth, MyMean)
  meanDF2 = right_join(traindf, meanDF, by = c("start_date", "Horizon", "DOY", "Depth"))
  meanDF2$sq_devs_smooth = (meanDF2$Temp_C_00UTC - meanDF2$MyMean)^2
  meanDF3 = meanDF2 %>% group_by(start_date, Horizon, DOY, Depth) %>% summarize(sum_devs = sum(sq_devs_smooth))
  meanDF3$Var = meanDF3$sum_devs / num_reps
  meanDF3$rootVar = sqrt(meanDF3$Var)
  #var_mean = mean(meanDF3$logVar)
  #var_sd = sd(meanDF3$logVar)
  #var_normalized = (meanDF3$logVar - var_mean ) / var_sd
  #Ztrain = var_normalized
  Ztrain <-  meanDF3$rootVar 
  
  estvar <- fit_scaled(Ztrain, Xtrain,  ms = m, nug = NULL)
  fitvar <- predictions_scaled(estvar, Xtrain, m = m, joint = FALSE, predvar = TRUE)
  var_mean_orig <- (fitvar$means)^2
  
  # upper 95 quantile of variance plus uncertainty in mean
  # d/dx(g(\mu))^2 * var(x) // g(x) = x^2 
  Delta_variance = (2*fitvar$means)^2 * fitvar$vars
  
  upper95_var2 = (fitvar$means)^2 + q2*sqrt(Delta_variance)
  vecDF$SKlower = vecDF$MyMean - q*(sqrt(upper95_var2/test_reps))
  vecDF$SKupper = vecDF$MyMean + q*(sqrt(upper95_var2/test_reps))
  
  vecDF$SKlowerPI = vecDF$MyMean - q*(sqrt(upper95_var2))
  vecDF$SKupperPI = vecDF$MyMean + q*(sqrt(upper95_var2)) 

  return(list(vecDF = vecDF, var_mean_orig = var_mean_orig))
}