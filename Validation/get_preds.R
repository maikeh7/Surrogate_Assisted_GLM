
# function to get in-sample predictions and construct bias-corrected GLM surrogate
# the predictions for the bias-corrected surrogate are saved in results_dir as 'preds_all_training.Rds'
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

# a function to generate predictions (forecast) for a new reference date (new_date)
# with a bias-corrected surrogate, which is also constructed in this function
# predictions for new_date are saved to results_dir as preds<new_date>.csv
# new_date is a character vector formatted as YYY-MM-DD
get_preds_valid = function(new_date, results_dir, surrogate_dir, obs_data){
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

# get predictions for when model_type is PERSISTENCE
get_preds_valid_persistence = function(new_date,
                                       results_dir,
                                       surrogate_dir,
                                       persist_dir,
                                       obs_data){
  persist_fit = readRDS(file.path(persist_dir, "PersistenceDF.Rds")) 

  persist_fit = persist_fit[, c(
                                "Horizon",
                                "fit",
                                "se.fit",
                                "temp_obs",
                                "DOY",
                                "Depth",
                                "bias",
                                "start_date")]

  Bias_fit =  readRDS(file.path(surrogate_dir, "bias_surrogate_persist.Rds")) 

  persist_fit = right_join(Bias_fit, persist_fit, by = c("DOY", "Depth", "Horizon", "start_date"))

  # obs_data = read.csv(file.path(surrogate_dir, "Bias_dataset_validation.csv"))
  persist_fit$BC_mean = persist_fit$fit + persist_fit$Mean
  persist_fit$BC_sd = sqrt((persist_fit$se.fit)^2 + persist_fit$Var)
  
  persist_fit$BCLower <- qnorm(0.05, persist_fit$BC_mean, persist_fit$BC_sd)
  persist_fit$BCUpper <- qnorm(0.95, persist_fit$BC_mean, persist_fit$BC_sd)
  
  obs_test_data = make_obs_testing_data(new_date, obs_data = obs_data)
 
  start_DOY = obs_test_data$DOY[1]
  start_date = obs_test_data$start_date[1]
  
  preds_df = filter(persist_fit, DOY == start_DOY) %>% 
    dplyr::select(DOY, Depth, Horizon, Temp_covar, start_date, BC_mean, BC_sd, BCLower, BCUpper,
                  Mean, se.fit, fit)
  preds_df$start_date = as.character(preds_df$start_date)
  preds_df = right_join(preds_df, obs_test_data, by = c("DOY", "Depth", "Horizon", "start_date"))

  write.csv(preds_df, file.path(results_dir, paste0("preds-persist", start_date, ".csv")))
  #write.csv(formatted, file.path(results_dir_formatted, paste0("preds_formatted", start_date, ".csv")))
  
}
