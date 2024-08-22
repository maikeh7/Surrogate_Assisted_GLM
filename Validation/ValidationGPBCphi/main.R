library(data.table)
library(lubridate)
library(dplyr)
source("SVecchia_Katzfussetal.R")
source("auxilary_functions_validation.R")
source("fit_GLM_surrogate.R")
source("fit_bias_surrogate.R")
source("make_horizon_data_initial_valid.R")
source("make_bias_dataset_validation_new.R")
source("get_preds.R")
#' Main function for constructing training data for GLM and bias surrogates
#' in Holthuijzen, Maike and Gramacy, Robert M., and Thomas, Quinn R., and 
#' Carey, Cayelan C. "Synthesizing data products, mathematical models, and observational 
#' measurements for lake temperature forecasting". Annals of Applied Statistics. (In preparation)
#' 
#' @param make_train_data If true, only training data is constructed (no validation takes place)
#' @param glm_path file path to directory where GLM forecasts live (must be constructed using code in 'GLM' folder)
#' @param train_end_date end date for training period
#' @param validation_end_date end date for validation period (must be after train_end_date)
#' @param horizon_dir Name of directory where training data for GLM surrogate will be placed (default is "HORIZON_TRAIN")
#' @param surrogate_dir Name of directory where GP model fits will be saved (default is "SURROGATES")
#' @param results_dir Name of directory where results will be saved (default is "RESULTS")
#' @param obs_dir Complete file path to observed data 
#' @param num_reps Number of replicates to be used for stochastic kriging
#'
#' @export
#'
#' @examples
#' main(make_train_data = TRUE) # constructs initial training data only
#' main(make_train_data = FALSE) # carries out validation, uses previously constructed training data
# glm_path ="C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Important_code/GLM_related/GLM/GLM_sims_NEW"
main = function(make_train_data = TRUE, 
                glm_path = "/home/maike/GP_surrogate_code/DATA/GLM_sims_NEW",
                train_end_date = "2022-06-10", #"2022-06-10"
                validation_end_date = "2023-06-11", #"2023-06-11"
                horizon_dir="HORIZON_TRAIN",
                surrogate_dir = "SURROGATES",
                results_dir = "RESULTS",
                obs_dir = "/home/maike/GP_surrogate_code/DATA/Bias_dataset/Observed_data.csv",
                num_reps = 31
){
  
  # set up dirs
  if(!dir.exists(horizon_dir)){
    dir.create(horizon_dir)
  }
  if(!dir.exists(surrogate_dir)){
    dir.create(surrogate_dir)
  }
  if(!dir.exists(results_dir)){
    dir.create(results_dir)
  }
  
  # extract dates for training/training file names
  # we assume files are in chronological order!!
  all_files = list.files(glm_path)
  all_dates = sub(".*_(.*).csv", "\\1", all_files)
  print(head(all_dates))
  end_idx = which(all_dates == train_end_date)
  if(is.na(end_idx)){
    stop("Invalid end date! Please pick another end_date")
    
  }
  # NOTE NOTE NOTE
  # train_dates represent the first forecast date! Not the reference date
  # this is handled in all of the functions (due to how NOAA defines reference date)
  # extract dates for training/training file names
  # we assume files are in chronological order!!
  train_dates = all_dates[1:end_idx]
  print(tail(train_dates))
  train_files = all_files[1:end_idx]
  
  if (!is.null(validation_end_date)){
    # get validation dates/ validation files
    end_valid_idx = which(all_dates == validation_end_date)
    
    valid_dates = all_dates[(end_idx + 1):end_valid_idx]
    
    valid_files = all_files[(end_idx + 1):end_valid_idx]
  }
  
  observed_data = read_obs_data(obs_dir)
  
  if (make_train_data){
    # if we run this for the first time, construct initial training dataset
    process_GLM_sims(obs_depth=1,
      method="Average",
      lookback=4,
      train_files,
      train_end_date,
      glm_path,
      horizon_dir,
      obs_data = observed_data,
      spinup=0)
    # train on the initial dataset, save results
    fit_GLM_surrogate(method="Average",
      lookback=4,
      obs_depth=1,
      horizon_dir,
      surrogate_dir = "SURROGATES",
      num_reps = num_reps)
             
    make_bias_horizon_datasets(train_dates,
      obs_depth=1,
      method="Average",
      lookback = 4,
      surrogate_dir,
      train_end_date,
      obs_data = observed_data)
           
    fit_bias_surrogate(
      method="Average", 
      lookback=4,
      obs_depth=1, 
      horizon_dir="HORIZON_TRAIN",
      surrogate_dir="SURROGATES")
             
    get_preds(results_dir, surrogate_dir)
    print('got preds')
             
    get_preds_valid(new_date = train_dates[length(train_dates)], 
      results_dir,
      surrogate_dir, 
      obs_data = observed_data)
    }else{
      for (i in 1:length(valid_dates)){
        print(paste("working on validation date:", valid_dates[i]))
        
        append_GLM_data(new_date = valid_dates[i],
          method="Average",
          lookback=4,
          obs_depth=1,
          glm_path,
          horizon_dir,
          obs_data = observed_data,
          spinup = 0)
               
        fit_GLM_surrogate(method="Average",
          lookback=4,
          obs_depth=1,
          horizon_dir,
          surrogate_dir,
          num_reps = num_reps)
             
        # need to rebuild this for every date in valid_dates unfortunately
        make_bias_horizon_datasets_validation(train_dates,
          new_date = valid_dates[i],
          obs_depth=1,
          method="Average",
          lookback = 4,
          surrogate_dir, 
          glm_path,
          obs_data = observed_data)
               
        fit_bias_surrogate(method="Average",
          lookback=4,
          obs_depth=1,
          horizon_dir="HORIZON_TRAIN",
          surrogate_dir="SURROGATES")
               
        get_preds_valid(new_date = valid_dates[i],
          results_dir,
          surrogate_dir,
          obs_data = observed_data)
        }
    }
}

main(make_train_data = TRUE)
main(make_train_data = FALSE)

