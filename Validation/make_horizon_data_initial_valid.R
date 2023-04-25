
process_GLM_sims = function(obs_depth=1, method="Average",lookback=4,train_files, 
                            train_end_date, glm_path, 
                            horizon_dir = "HORIZON_TRAIN",
                            obs_data){
  ymd = make_ymd()
  # get rid of dates that occurred after training dates
  obs_data= filter(obs_data, date <= as.Date(train_end_date))
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  # make the observed dataset 
  obs_df = make_obs_data(obs_data, obs_depth = 1, lookback = 4)
  
  horizon = 1:30

  for (i in horizon){
    print(i)
    BigDF = list(length = length(train_files))
    for (j in 1:length(train_files)){
      #print(j)
      
      temp_file = data.table::fread(file.path(glm_path, train_files[j]))
    
      mydates= do.call(paste, c(temp_file[, .(YEAR, MONTH, DAY)], sep="-"))
     # mydates = do.call(paste, c(temp_file[c("YEAR", "MONTH", "DAY")], sep="-"))
      temp_file = cbind(cbind, temp_file[,-1 ], mydates)
      colnames(temp_file)[which(colnames(temp_file) == "mydates")] = "date"
      #temp_file$date = paste(temp_file$YEAR, temp_file$MONTH, temp_file$DAY, sep = "-")
      temp_file$date = as.POSIXct(temp_file$date, tz = "UTC")
      temp_file = arrange(temp_file, date)
      # filter to only get actual start date, as the 7 day spinup is included in these simulations
      actual_start = temp_file$date[1] + lubridate::days(6)
      
      # get the actual reference date 
      # actual_start is the first forecast horizon, so the ref date is the day before actual_start
      ref_date = actual_start - lubridate::days(1)
      
      # get the temperature corresponding to start_date
      # which will be the average of the PREVIOUS XX days
      start_date_obs_temp = get_obs_temp(ref_date, obs_depth, obs_df)
      
      temp_file = filter(temp_file, date >= actual_start)
      temp_file$ref_date = ref_date
      
      # grab the correct forecast horizon
      end_date = ref_date + lubridate::days(i)
      temp_file = filter(temp_file, date == end_date )
      
      # add on observed temp at START DATE here (it will be the same for all depths!)
      temp_file$start_date_obs_temp = start_date_obs_temp
      col_idx = which(colnames(head(temp_file)) == "mean_Temp_C")
      if (identical(col_idx, integer(0))){
       # wanna put pass here but alas that is not a thing in r 
      }else{
      colnames(temp_file)[col_idx] = "Temp_C_00UTC"
      }
      BigDF[[j]] = temp_file
    }
    
    
    BigDF = data.table::rbindlist(BigDF)
    BigDF$horizon = i
    BigDF_sub = BigDF %>% dplyr::select(depth_int, Temp_C_00UTC, ref_date, horizon, start_date_obs_temp)
    
    BigDF_sub$start_date = as.character(BigDF_sub$ref_date)
    BigDF_sub$ref_date = NULL
    mydates = strsplit(BigDF_sub$start_date, "-")
    
    mydates = data.frame(do.call("rbind", mydates))
    
    colnames(mydates) = c("YEAR",  "MONTH", "DAY")
    
    mydates = data.frame(apply(mydates, 2, function(x) as.numeric(x)))
    
    BigDF_sub = cbind(BigDF_sub, mydates)
  
    BigDF_sub = right_join(BigDF_sub,ymd, by = c("MONTH", "DAY"))
    BigDF_sub = BigDF_sub[complete.cases(BigDF_sub), ]
    BigDF_sub = as.data.frame(BigDF_sub)

    write.csv(BigDF_sub, file.path(horizon_dir, paste0("BigDF_H",i,
                    "_TRAIN", method, lookback, "D", obs_depth, ".csv")))

  }
  print(paste("method=", method, "lookback=", lookback, "obs_depth=", obs_depth))
  
}


# checked
append_GLM_data=function(new_date, method="Average", lookback=4, obs_depth=1,
                             glm_path, horizon_dir, obs_data){
  ymd = make_ymd()
  
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  obs_data = filter(obs_data, date <= as.Date(new_date)) 
  
  # make the observed dataset 
  obs_df = make_obs_data(obs_data, obs_depth = 1, lookback = 4)
  
  # list all glm sims
  all_files = list.files(glm_path)

  #all_files = list.files("/home/maike/GP_surrogate_code/DATA/GLM_sims/") 
  
  # extract the glm sim corresponding to date of interest
  all_dates = sub(".*_(.*).csv", "\\1", all_files)
  file_name = all_files[grep(new_date, all_dates)]

  curr_file = read.csv(file.path(glm_path, file_name))
  
  curr_file$X=NULL
  curr_file$date = paste(curr_file$YEAR, curr_file$MONTH, curr_file$DAY, sep = "-")
  curr_file$date = as.POSIXct(curr_file$date, tz = "UTC")
  curr_file = dplyr::arrange(curr_file, date)

  # filter to only get actual start date, as the 7 day spinup is included in these simulations
  actual_start = temp_file$date[1] + lubridate::days(6)
  
  # get the actual reference date 
  # actual_start is the first forecast horizon, so the ref date is the day before actual_start
  ref_date = actual_start - lubridate::days(1)
  
  # get the temperature corresponding to start_date
  # which will be the average of the PREVIOUS XX days
  start_date_obs_temp = get_obs_temp(ref_date, obs_depth, obs_df)

  #print(actual_start)
  # get the temperature corresponding to start_date
  start_date_obs_temp = get_obs_temp(actual_start,
                                     obs_depth, obs_df)
  
  curr_file = dplyr::filter(curr_file, date >= actual_start)
  curr_file$ref_date = ref_date

  horizon=1:30
 
  for (i in horizon){

    bigDF = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                            "D", obs_depth, ".csv")))
    
    # this start_date IS actually the reference date so we don't need to change it
    bigDF$V1=NULL
    bigDF$start_date = as.character(bigDF$start_date)
    
    # grab the correct forecast horizon
    end_date = ref_date + lubridate::days(i)
    temp_file = filter(curr_file, date == end_date )
    # add on observed temp at START DATE here (it will be the same for all depths!)
    temp_file$start_date_obs_temp = start_date_obs_temp
 
    temp_file$horizon = i
    
    temp_sub = temp_file[,c("depth_int", "Temp_C_00UTC", "ref_date",
                            "horizon", "start_date_obs_temp")]

    temp_sub$start_date = as.character(temp_sub$ref_date)
    temp_sub$ref_date = NULL
    
    mydates = strsplit(temp_sub$start_date, "-")
    
    mydates = data.frame(do.call("rbind", mydates))
    
    colnames(mydates) = c("YEAR",  "MONTH", "DAY")
    
    mydates = data.frame(apply(mydates, 2, function(x) as.numeric(x)))
    
    temp_sub = cbind(temp_sub, mydates)
   
    temp_sub = right_join(temp_sub, ymd, by = c("MONTH", "DAY"))
    temp_sub = temp_sub[complete.cases(temp_sub), ]

    bigDF_append = data.table::rbindlist(list(bigDF, temp_sub))
    write.csv(bigDF_append, file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                          "D", obs_depth, ".csv")))

  }
}
