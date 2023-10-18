

#' Construct training data for GLM surrogate
#' 

#'
#' @param obs_depth the depth at which the latent variable phi is calculated (it should be 1--phi is calculated as an 
#' average over depths 0 and 1)
#' @param method The method by which phi is calculated. 'Average' is the only accepted value.
#' @param lookback The number of days over which the average should be taken to obtain phi (we recommend between 4 and 7)
#' @param train_files character, The names of files for GLM forecasts in the training period 
#' @param train_end_date character, the end of the training period, formatted as YYYY-MM-DD
#' @param glm_path The directory in which GLM forecasts live
#' @param horizon_dir The name of the directory that training data will be saved to
#' @param obs_data Observed data, a csv file 
#' @param spinup spinup is days of spinup minus 1 (so for a 7 day spinup, use spinup=6). We do not recommend using 
#' any spinup and recommend a spinup value of 0.
#'
#' @return
#' @export
#'
#' @examples
process_GLM_sims = function(obs_depth=1, 
                            method = "Average",
                            lookback=4,
                            train_files, 
                            train_end_date,
                            glm_path, 
                            horizon_dir = "HORIZON_TRAIN",
                            obs_data, 
                            spinup=0){
  ymd = make_ymd()
  # get rid of dates that occurred after training dates
  obs_data= filter(obs_data, date <= as.Date(train_end_date))
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  
  # make the observed dataset of reference dates and the corresponding
  # value of 'phi' which is the average of the previous 4 dates before the 
  # reference date. It is necessary for capturing year to year variability
  obs_df = make_obs_data(obs_data, obs_depth = 1, lookback = 4)
  
  horizon = 1:30
  for (i in horizon){
    print(i)
    BigDF = list(length = length(train_files))
    for (j in 1:length(train_files)){
      #print(j)
      
      temp_file = data.table::fread(file.path(glm_path, train_files[j]))
  
      mydates= do.call(paste, c(temp_file[, .(YEAR, MONTH, DAY)], sep="-"))
    
      temp_file = cbind(cbind, temp_file[,-1 ], mydates)
      colnames(temp_file)[which(colnames(temp_file) == "mydates")] = "date"
      
      temp_file$date = as.POSIXct(temp_file$date, tz = "UTC")
      temp_file = arrange(temp_file, date)
      
      # filter to only get actual start date, as the 7 day spinup is included in these simulations
      if (spinup > 0){
        actual_start = temp_file$date[1] + lubridate::days(spinup)
      }else{
        actual_start = as.POSIXct(temp_file$start_date[1], tz = "UTC")
      }
      
      # get the temperature corresponding to start_date
      # which will be the average of the PREVIOUS 'lookback' days
      start_date_obs_temp = get_obs_temp(actual_start, obs_depth, obs_df)
      
      temp_file = filter(temp_file, date >= actual_start)
     
      # grab the correct forecast horizon
      end_date = actual_start + lubridate::days(i)
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
    BigDF_sub = BigDF %>% dplyr::select(depth_int, Temp_C_00UTC, start_date, horizon, start_date_obs_temp)
    
    BigDF_sub$start_date = as.character(BigDF_sub$start_date)

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

#' Append data to training data for GLM surrogate for use in validation
#'
#' @param new_date character, formatted as YYYY-MM-DD, the new reference date at which a forecast is to be made
#' @param method The method by which phi is calculated. 'Average' is the only accepted value.
#' @param lookback The number of days over which the average should be taken to obtain phi (we recommend between 4 and 7)
#' @param obs_depth the depth at which the latent variable phi is calculated (it should be 1--phi is calculated as an 
#' average over depths 0 and 1)
#' @param glm_path The directory in which GLM forecasts live
#' @param horizon_dir The name of the directory that training data will be saved to
#' @param obs_data Observed data, a csv file 
#' @param spinup spinup is days of spinup minus 1 (so for a 7 day spinup, use spinup=6). We do not recommend using 
#' any spinup and recommend a spinup value of 0.
#'
#' @return
#' @export
#'
#' @examples
append_GLM_data = function(new_date, method="Average", lookback=4, obs_depth=1,
                             glm_path, horizon_dir, obs_data, spinup=0){
  ymd = make_ymd()
  
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  obs_data = filter(obs_data, date <= as.Date(new_date)) 
  
  # make the observed dataset 
  obs_df = make_obs_data(obs_data, obs_depth = 1, lookback = 4)
  
  # list all glm sims
  all_files = list.files(glm_path)

  # extract the glm sim corresponding to date of interest
  all_dates = sub(".*_(.*).csv", "\\1", all_files)
  file_name = all_files[grep(new_date, all_dates)]

  curr_file = read.csv(file.path(glm_path, file_name))
  
  curr_file$X=NULL
  curr_file$date = paste(curr_file$YEAR, curr_file$MONTH, curr_file$DAY, sep = "-")
  curr_file$date = as.POSIXct(curr_file$date, tz = "UTC")
  curr_file = dplyr::arrange(curr_file, date)

  # filter to only get actual start date, as the 7 day spinup is included in these simulations
  if (spinup > 0){
     actual_start = curr_file$date[1] + lubridate::days(spinup)
  }else{
      actual_start = as.POSIXct(curr_file$start_date[1], tz = "UTC")
  }

  # get the temperature corresponding to start_date
  start_date_obs_temp = get_obs_temp(actual_start,
                                     obs_depth, obs_df)
  
  curr_file = dplyr::filter(curr_file, date >= actual_start)
  curr_file$start_date = actual_start

  horizon=1:30
 
  for (i in horizon){

    bigDF = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                            "D", obs_depth, ".csv")))
    
    # this start_date IS actually the reference date so we don't need to change it
    bigDF$V1=NULL
    bigDF$start_date = as.character(bigDF$start_date)
    
    # grab the correct forecast horizon
    end_date = actual_start + lubridate::days(i)
    temp_file = filter(curr_file, date == end_date )
    # add on observed temp at START DATE here (it will be the same for all depths!)
    temp_file$start_date_obs_temp = start_date_obs_temp
 
    temp_file$horizon = i
    
    temp_sub = temp_file[,c("depth_int", "Temp_C_00UTC", "start_date",
                            "horizon", "start_date_obs_temp")]

    temp_sub$start_date = as.character(temp_sub$start_date)
    
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

# make initial persistence dataset
make_persistence_data = function(obs_depth=1,
                                 method="Average",
                                 lookback=4,
                                 train_dates, 
                                 train_end_date,
                                 obs_data,
                                 persist_dir){
  #ymd = make_ymd()
  # get rid of dates that occurred after training dates
  obs_data= filter(obs_data, date <= as.Date(train_end_date))
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  # make the observed dataset 
  obs_df = make_obs_data(obs_data, obs_depth = 1, lookback = 4)
  
  train_dates = as.Date(train_dates)
  
  Temp_covars = unlist(lapply(train_dates, function(x) get_obs_temp(x, obs_depth = 1, obs_df)))

  Temp_covar_df = data.frame(start_date = train_dates, Temp_covar = Temp_covars)
  
  colnames(obs_data)[which(colnames(obs_data) == "depth_int")] = "Depth"
  
  biglist=list(length = length(train_dates))
  giantlist = list(length=10)
  for (j in 0:9){
    biglist=list(length = length(train_dates))
    #print(j)
    d1 = filter(obs_data, Depth == j)
    d1$date = as.Date(d1$date)
    for (i in 1:length(train_dates)){
      cur_date = train_dates[i]
      myDOY = filter(obs_data, date == cur_date)$DOY[1]
      Lag7date = cur_date - lubridate::days(7)
      lag_dates = seq(Lag7date, cur_date, by = "day")
      lagdf = data.frame(T = -7:0, date = lag_dates)
      tempdf = filter(d1, (date >= Lag7date) & (date <= cur_date))
      if (nrow(tempdf) < 5){
        next
      }
      tempdf2 = tempdf[, c("temp_obs", "date")]
      tempdf2 = right_join(tempdf2, lagdf,by="date")
      
      mod1 = lm(temp_obs~T, data = tempdf2)
      
      xnew = data.frame(T = -7:30)
      preds = as.data.frame(predict(mod1, xnew, interval="confidence"))
      
      preds = as.data.frame(predict(mod1, xnew, se.fit = TRUE))
      preds = preds[, c("fit", "se.fit", "residual.scale")]
      
      forecast_dates = data.frame(date=seq(Lag7date, (cur_date + lubridate::days(30)), by = "day"),
                                  T = -7:30)
      forecast_dates = cbind(forecast_dates, preds)
      truevals= filter(d1, date %in% forecast_dates$date) 
      truevals = truevals[, c("date", "temp_obs", "DOY")]
      
      forecast_dates = right_join(forecast_dates, truevals, by = "date")
      
      forecast_dates$start_date = cur_date
      forecast_dates$bias = forecast_dates$temp_obs - forecast_dates$fit
      
      forecast_dates$DOY = myDOY
      biglist[[i]] = forecast_dates
    }
    bigdf = data.table::rbindlist(biglist)
    bigdf$Depth = j
    giantlist[[j+1]] = bigdf
    
  }

  persistencedf = data.table::rbindlist(giantlist)
  persistencedf = right_join(persistencedf, Temp_covar_df, by = "start_date")
  persistencedf = persistencedf[complete.cases(persistencedf), ]
  colnames(persistencedf)[which(colnames(persistencedf) == "T")] = "Horizon"
  saveRDS(persistencedf, file.path(persist_dir, "PersistenceDF.Rds"))
  #saveRDS(persistencedf, "PersistenceDF.Rds")
}

append_persistence_data = function(new_date,
                                   method="Average",
                                   lookback=4,
                                   obs_depth=1,
                                   obs_data,
                                   persist_dir){
  persistencedf = readRDS(file.path(persist_dir, "PersistenceDF.Rds"))
  
  obs_data = filter(obs_data, date >= as.Date("2020-09-01"))
  obs_data = filter(obs_data, date <= as.Date(new_date)) 

  # make the observed dataset 
  obs_df = make_obs_data(obs_data, obs_depth = 1, lookback = 4)
  
  
  start_date_obs_temp = get_obs_temp(as.Date(new_date),
                                     obs_depth, obs_df)
  colnames(obs_data)[which(colnames(obs_data) == "depth_int")] = "Depth"

  biglist = list(length=10)
  for (j in 0:9){
    d1 = filter(obs_data, Depth == j)
    cur_date = as.Date(new_date)
    myDOY = filter(obs_data, date == cur_date)$DOY[1]
    Lag7date = cur_date - lubridate::days(7)
    lag_dates = seq(Lag7date, cur_date, by = "day")
    lagdf = data.frame(T = -7:0, date = lag_dates)
    tempdf = filter(d1, (date >= Lag7date) & (date <= cur_date))
    if (nrow(tempdf) < 5){
      next
    }
    tempdf2 = tempdf[, c("temp_obs", "date")]
    tempdf2 = right_join(tempdf2, lagdf,by="date")
    
    mod1 = lm(temp_obs~T, data = tempdf2)
    
    xnew = data.frame(T = -7:30)
    preds = as.data.frame(predict(mod1, xnew, se.fit = TRUE))
    preds = preds[, c("fit", "se.fit", "residual.scale")]
    
    forecast_dates = data.frame(date=seq(Lag7date, (cur_date + lubridate::days(30)), by = "day"),
                                T = -7:30)
    forecast_dates = cbind(forecast_dates, preds)
    d1$date = as.Date(d1$date)
    truevals= filter(d1, date %in% forecast_dates$date) 
    
    truevals = truevals[, c("date", "temp_obs", "DOY")]
    
    forecast_dates = right_join(truevals, forecast_dates, by = "date")
    
    forecast_dates$start_date = cur_date
    forecast_dates$bias = forecast_dates$temp_obs - forecast_dates$fit
    forecast_dates$DOY = myDOY
    forecast_dates$Depth = j
    biglist[[(j+1)]] = forecast_dates
    
  }
 
  bigdf= data.table::rbindlist(biglist)
  bigdf$Temp_covar = start_date_obs_temp
  colnames(bigdf)[which(colnames(bigdf) == "T")] = "Horizon"

  bigdf$Temp_covar = start_date_obs_temp
  col_names = colnames(persistencedf)
  bigdf = bigdf[, ..col_names]
  colnames(bigdf)
  res = rbind(persistencedf, bigdf)

  saveRDS(res, file.path(persist_dir, "PersistenceDF.Rds"))
}