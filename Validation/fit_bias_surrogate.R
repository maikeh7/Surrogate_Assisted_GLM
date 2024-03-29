#' this function fits a GP to bias data via Vecchia approximation (no stochastic kriging)
#'
#' @param method method to obtain value of phi: "Average" (denotes average of 4 days of observed data prior to a given 
#' reference date). Do not change.
#' @param lookback 4 (do not change)
#' @param obs_depth 1 (do not change)
#' @param horizon_dir directory where train data lives (default is HORIZON_TRAIN) 
#' @param surrogate_dir directory where all surrogates will be saved to (default is SURROGATES)
#' @param persist_dir directory where PERSISTENCE results will be saved to (if PERSISTENCE is selected as model_type,
#' default is PERSISTENCE)
#' @param model_type character vector denoting which model to fit "GLM" (recommended, used in paper) or 
#' ("PERSISTENCE" or "GLM_byDepth" , not recommended)
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
                              surrogate_dir="SURROGATES",
                              persist_dir = "PERSISTENCE",
                              model_type){
  if (model_type == "GLM"){
    # get one dataset from the 30 horizon datasets. Will be used to 
    # make the prediction grid. Not most elegant solution but couldn't think quickly of another way
    df30=read.csv(file.path(horizon_dir, paste0("BigDF_H", 30, "_TRAIN", method, lookback,
                                                "D", obs_depth, ".csv")))
    
    dummy_data = df30 %>% group_by(depth_int, DOY, start_date_obs_temp, start_date) %>%
      summarise(meanTemp = mean(Temp_C_00UTC))
    
    dummy_data$meanTemp = NULL
    
    bias_data = as.data.frame(data.table::fread(file.path(surrogate_dir, "Bias_dataset_validation.csv")))
    
    bias_data = bias_data[complete.cases(bias_data), ]
    
    pred_df = data.frame(DOY =rep(1:366, 10), 
                         depth_int=rep(0:9, each=366),
                         horizon=rep(1:30, each = 3660))
    
    # right join to get the temp 'covariate'
    pred_times = right_join(pred_df, dummy_data, by = c("DOY", "depth_int"))
    start_dates = pred_times$start_date
    
    pred_times = as.matrix(pred_times[,1:4])
  }
  
  if (model_type == "PERSISTENCE"){
    #print("persistence model surrogate")
    # note that for the persistence model GP, horizon = T, but T ranges from -7 to 30
    bias_data = readRDS(file.path(persist_dir, "PersistenceDF.Rds"))
    bias_data = bias_data[complete.cases(bias_data), ]
    dummy_data = filter(bias_data, Horizon== -4) 
    
    dummy_data = dummy_data %>% group_by(Depth, DOY, start_date, Temp_covar) %>%
      summarise(meanTemp = mean(temp_obs, na.rm=TRUE))

    dummy_data$meanTemp = NULL
    
    pred_df = data.frame(DOY = rep(1:366, 10), 
                         Depth = rep(0:9, each=366),
                         horizon = rep(1:30, each = 3660))

    # right join to get the temp 'covariate'
    pred_times = right_join(dummy_data, pred_df, by = c("DOY", "Depth"))
    
    pred_times = pred_times[complete.cases(pred_times), ]
    
    start_dates = pred_times$start_date
    
    pred_times = as.matrix(pred_times[,c("DOY", "Depth", "horizon", "Temp_covar")])
      
  }
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
  est <- fit_scaled(Y, X,  ms = m, nug = NULL, trend = "zero")
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
  
  
  if (model_type == "GLM"){
    saveRDS(vecDF, file.path(surrogate_dir, "bias_surrogate.Rds"))
  }
  if (model_type == "PERSISTENCE"){
    saveRDS(vecDF, file.path(surrogate_dir, "bias_surrogate_persist.Rds"))
  }
}

###############################################################################################
### fit by depth--we do not recommend this
###############################################################################################
fit_bias_surrogate_byDepth = function(method="Average",
                              lookback=4,
                              obs_depth=1,
                              horizon_dir="HORIZON_TRAIN",
                              surrogate_dir="SURROGATES"){

  # get one dataset from the 30 horizon datasets. Will be used to 
  # make the prediction grid. Not most elegant solution but couldn't think quickly of another way
  df30 = read.csv(file.path(horizon_dir, paste0("BigDF_H", 30, "_TRAIN", method, lookback,
                                                "D", obs_depth, ".csv")))
  
  bias_df = as.data.frame(data.table::fread(file.path(surrogate_dir, "Bias_dataset_validation.csv")))
  
  bias_df = bias_df[complete.cases(bias_df), ]
  
  results_list = list(length=10)
  i=0
  for (i in 0:9){
  mydepth = i
  dummy_data = filter(df30, depth_int == mydepth) %>% group_by(DOY, start_date_obs_temp, start_date) %>%
      summarise(meanTemp = mean(Temp_C_00UTC))
  
    
  dummy_data$meanTemp = NULL
    
  bias_data = filter(bias_df, Depth == mydepth)
  
  pred_df = data.frame(DOY =rep(1:366, 30), Horizon=rep(1:30, each = 366))
    
  # right join to get the temp 'covariate'
  pred_times = right_join(pred_df, dummy_data, by = c("DOY"))
  
  start_dates = pred_times$start_date
  
  pred_times = as.matrix(pred_times[, 1:3])
  
  # normalize response (bias)
  bias_mean = mean(bias_data$bias)
  bias_sd = sd(bias_data$bias)
  bias_normalized = (bias_data$bias - bias_mean ) / bias_sd
  
  # standardize inputs to be in [0,1]
  DOY_range = range(bias_data$DOY)
  horizon_range = range(bias_data$Horizon)
  tempcovar_range = range(bias_data$Temp_covar)
  
  DOY_standardized <- (bias_data$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  horizon_standardized <- (bias_data$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
  tempcovar_standardized <- (bias_data$Temp_covar - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1]) 
  
  X <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized)
  Y <- bias_normalized
  
  # standardize prediction locs to be in [0,1]
  #pred_times = cbind(rep(1:366, 10), rep(0:9, each=366), rep(1:30, each = 3660))
  pred_times_scaled = pred_times
  pred_times_scaled[, 1] <- (pred_times_scaled[, 1] - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
  pred_times_scaled[, 2] <- (pred_times_scaled[, 2]- horizon_range[1]) / (horizon_range[2] - horizon_range[1])
  pred_times_scaled[, 3] <- (pred_times_scaled[, 3] - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
  
  
  # size of conditioning sets
  m <- 30
  
  # fit scaled vecchia using the sourced code 
  # I give nu a value of 3.5
  print("fitting bias")
  est <- fit_scaled(Y, X,  ms = m, nug = NULL, trend = "zero")
  print("cov params: var, ranges, nug")
  print(est$covparms)

  # make predictions
  fit <- predictions_scaled(est, pred_times_scaled, m = m, joint = FALSE, predvar = TRUE)
  mean_orig <- fit$means * bias_sd + bias_mean
  s2_orig <- fit$vars * (bias_sd^2)
  
  # make data.frame so we can plot stuff
  vecDF <- data.frame(cbind(pred_times, mean_orig, s2_orig))
  colnames(vecDF) <- c("DOY", "Horizon", "Temp_covar", "Mean", "Var")
  vecDF$SD <- sqrt(vecDF$Var)
  vecDF$Lower <- qnorm(0.05, vecDF$Mean, vecDF$SD)
  vecDF$Upper <- qnorm(0.95, vecDF$Mean, vecDF$SD)
  vecDF$start_date = start_dates
  
  vecDF$Depth = i
  
  results_list[[(i+1)]] = vecDF

  }
  
  vecDF = rbindlist(results_list)
  saveRDS(vecDF, file.path(surrogate_dir, "bias_surrogate.Rds"))
}

