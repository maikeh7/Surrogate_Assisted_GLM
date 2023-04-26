get_preds = function(results_dir, surrogate_dir){
  GLM_fit = readRDS(file.path(surrogate_dir, "GLM_Surrogate_SK.Rds")) 
  Bias_fit =  readRDS(file.path(surrogate_dir, "bias_surrogate.Rds")) 
  GLM_fit = arrange(GLM_fit, DOY, Depth, Horizon, Temp_covar)
  Bias_fit = arrange(Bias_fit, DOY, Depth, Horizon, Temp_covar)
  
 # obs_data = read.csv(file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  GLM_fit$BC_mean = GLM_fit$Mean + Bias_fit$Mean
  GLM_fit$BC_sd = sqrt(GLM_fit$Var_SK + Bias_fit$Var)
  
  GLM_fit$BCLower <- qnorm(0.05, GLM_fit$BC_mean, GLM_fit$BC_sd)
  GLM_fit$BCUpper <- qnorm(0.95, GLM_fit$BC_mean, GLM_fit$BC_sd)
  

  saveRDS(GLM_fit, file.path(results_dir, paste0("preds_all_training.Rds")))
  
}

tail(GLM_fit)
tail(Bias_fit)
obs_data = observed_data
get_preds_valid = function(new_date, results_dir, results_dir_formatted, surrogate_dir, obs_data){
  GLM_fit = readRDS(file.path(surrogate_dir, "GLM_Surrogate_SK.Rds")) 
  Bias_fit =  readRDS(file.path(surrogate_dir, "bias_surrogate.Rds")) 
  GLM_fit = arrange(GLM_fit, DOY, Depth, Horizon, Temp_covar)
  Bias_fit = arrange(Bias_fit, DOY, Depth, Horizon, Temp_covar)
  
 # obs_data = read.csv(file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  GLM_fit$BC_mean = GLM_fit$Mean + Bias_fit$Mean
  GLM_fit$BC_sd = sqrt(GLM_fit$Var_SK + Bias_fit$Var)
  
  GLM_fit$BCLower <- qnorm(0.05, GLM_fit$BC_mean, GLM_fit$BC_sd)
  GLM_fit$BCUpper <- qnorm(0.95, GLM_fit$BC_mean, GLM_fit$BC_sd)
  
  obs_test_data = make_obs_testing_data(new_date, obs_data = obs_data)
  
  start_DOY = obs_test_data$DOY[1]
  start_date = obs_test_data$start_date[1]
  
  preds_df = filter(GLM_fit, DOY == start_DOY) %>% 
    dplyr::select(DOY, Depth, Horizon, Temp_covar, start_date, BC_mean, BC_sd, BCLower, BCUpper,
                  Mean, SD_SK, HetLower, HetUpper)
  
  preds_df = right_join(preds_df, obs_test_data, by = c("DOY", "Depth", "Horizon", "start_date"))
  
  # start_date = preds_df$start_date[1]
  # 
  # formatted = data.frame(model_id = "Surrogate", 
  #                        datetime = preds_df$start_date + lubridate::days(preds_df$Horizon),
  #                        reference_datetime = preds_df$start_date,
  #                        site_id = "FCR",
  #                        family = "Gaussian",
  #                        parameter= preds_df$Depth,
  #                        variable = "temperature",
  #                        prediction = preds_df$BC_mean 
  #                        )

  write.csv(preds_df, file.path(results_dir, paste0("preds", start_date, ".csv")))
  #write.csv(formatted, file.path(results_dir_formatted, paste0("preds_formatted", start_date, ".csv")))
  
}
class(start_date)
#get_scores = function(test_data){
  
#}