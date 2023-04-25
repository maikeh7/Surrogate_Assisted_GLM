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

main = function(glm_path = "/home/maike/GP_surrogate_code/DATA/GLM_sims_NEW",
train_end_date = "2022-08-01",
horizon_dir="HORIZON_TRAIN",
surrogate_dir = "SURROGATES",
results_dir = "RESULTS"
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
  
  # NOTE NOTE NOTE
  # train_dates represent the first forecast date! Not the reference date
  # this is handled in all of the functions (due to how NOAA defines reference date)
  # extract dates for training/training file names
  # we assume files are in chronological order!!
  all_files = list.files(glm_path)
  all_dates = sub(".*_(.*).csv", "\\1", all_files)
  print(head(all_dates))
  end_idx = which(all_dates == train_end_date)
  
  # initial training dates
  # these dates are actually the first horizon, so 
  # the reference date for all forecast is the day BEFORE these dates
  train_dates = all_dates[1:end_idx]
  print(tail(train_dates))
  train_files = all_files[1:end_idx]
  
  observed_data = read_obs_data()

    # if we run this for the first time, construct initial training dataset
   #process_GLM_sims(obs_depth=1, method="Average", lookback=4, train_files, 
   #                train_end_date, glm_path, horizon_dir, obs_data = observed_data)
   # train on the initial dataset, save results
   #fit_GLM_surrogate(method="Average", lookback=4, obs_depth=1, horizon_dir, 
   #                  surrogate_dir = "SURROGATES", num_reps = 31)
    
   #make_bias_horizon_datasets(train_dates, obs_depth=1, method="Average", lookback = 4,
   #                        surrogate_dir, train_end_date, obs_data = observed_data)
   
   #fit_bias_surrogate(method="Average", lookback=4, obs_depth=1)
   
   get_preds(results_dir, surrogate_dir)
   
   get_preds_valid(new_date = train_dates[length(train_dates)], 
                   results_dir, results_dir_formatted, surrogate_dir, obs_data = observed_data)
  
}

main()
print("done")

