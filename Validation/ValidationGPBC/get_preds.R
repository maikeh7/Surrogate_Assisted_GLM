##########################################################
# function to get bias-corrected predictions
# add first and second moments of GLM and bias surrogates
# IN-SAMPLE PREDS ONLY
get_preds = function(results_dir, surrogate_dir){
  q = qnorm(0.95)
  GLM_fit = readRDS(file.path(surrogate_dir, "GLM_Surrogate_SK.Rds")) 
  Bias_fit =  readRDS(file.path(surrogate_dir, "bias_surrogate.Rds")) 
  GLM_fit = dplyr::arrange(GLM_fit, DOY, Depth, Horizon, Temp_covar)
  Bias_fit = dplyr::arrange(Bias_fit, DOY, Depth, Horizon, Temp_covar)
  
 # obs_data = read.csv(file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  GLM_fit$BC_mean = GLM_fit$Mean + Bias_fit$BiasMean
  
  # Note: Var_SK_divReps is SK variance divided by num_reps
  GLM_fit$BC_sd = sqrt(GLM_fit$Var_SK_divReps + Bias_fit$BiasVar)
  
  # 90% prediction intervals
  GLM_fit$BCLower <- GLM_fit$BC_mean - q*GLM_fit$BC_sd
  GLM_fit$BCUpper <- GLM_fit$BC_mean + q*GLM_fit$BC_sd

  saveRDS(GLM_fit, file.path(results_dir, paste0("preds_all_training.Rds")))
  
}

# OUT OF SAMPLE PREDS for validation exercise
get_preds_valid = function(new_date, results_dir, surrogate_dir, obs_data){
  q = qnorm(0.95)
  GLM_fit = readRDS(file.path(surrogate_dir, "GLM_Surrogate_SK.Rds")) 
  Bias_fit =  readRDS(file.path(surrogate_dir, "bias_surrogate.Rds")) 
  GLM_fit = arrange(GLM_fit, DOY, Depth, Horizon, Temp_covar)
  Bias_fit = arrange(Bias_fit, DOY, Depth, Horizon, Temp_covar)
  
 # obs_data = read.csv(file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  GLM_fit$BC_mean = GLM_fit$Mean + Bias_fit$BiasMean
  
  # Note: Var_SK_divReps is SK variance divided by num_reps
  GLM_fit$BC_sd = sqrt(GLM_fit$Var_SK_divReps + Bias_fit$BiasVar)
  
  GLM_fit$BCLower <- GLM_fit$BC_mean - q*GLM_fit$BC_sd
  GLM_fit$BCUpper <- GLM_fit$BC_mean + q*GLM_fit$BC_sd
  
  obs_test_data = make_obs_testing_data(new_date, obs_data = obs_data)
  
  start_DOY = obs_test_data$DOY[1]
  start_date = obs_test_data$start_date[1]
  
  #SD_SK is SD of GLM fit WITHOUT DIVIDING BY REPS=31
  preds_df = filter(GLM_fit, DOY == start_DOY) %>% 
    dplyr::select(DOY, Depth, Horizon, Temp_covar, start_date, BC_mean, BC_sd, BCLower, BCUpper,
                  Mean, SD_SK, Var_SK_divReps, HetLower, HetUpper, HetLowerPI, HetUpperPI)
  
  preds_df = right_join(preds_df, obs_test_data, by = c("DOY", "Depth", "Horizon", "start_date"))
  
  write.csv(preds_df, file.path(results_dir, paste0("preds", start_date, ".csv")))
  #write.csv(formatted, file.path(results_dir_formatted, paste0("preds_formatted", start_date, ".csv")))
  
}

# head(GLM_fit)
# library(ggplot2)
# #test = filter(TrueSK_df, start_date == "2022-06-15")
# test = filter(GLM_fit, start_date == "2021-02-19")
# head(test)
# ggplot(test, aes(x = Horizon, y = BC_mean)) +
#   geom_line() +
#   #facet_wrap(~Depth)
#   geom_line(data=test, aes(x = Horizon, y = BCLower), col = "red") +
#   geom_line(data = test, aes(x = Horizon, y = BCUpper), col = "red") +
#   facet_wrap(~Depth)
