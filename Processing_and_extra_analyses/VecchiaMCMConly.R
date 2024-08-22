library(dplyr)
library(DiceKriging)
library(data.table)
source("SVecchia_Katzfussetal.R")
source("fit_sk.R")
q = qnorm(0.975)

##########################
# Grab data
###########################
biglist = list(length=30)
horizon_dir = "HORIZON_TRAIN"
method = "Average"
lookback = 4
obs_depth = 1
for (i in 1:30){
  temp = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                         "D", obs_depth, ".csv")))
  biglist[[i]] = temp
}
bigdf = data.table::rbindlist(biglist)
bigdf = as.data.frame(bigdf)
colnames(bigdf)[which(colnames(bigdf) == "depth_int")] = "Depth"
colnames(bigdf)[which(colnames(bigdf) == "horizon")] = "Horizon"
dates = format(bigdf$start_date, format="%Y")
date2022 = which(dates == "2022")
bigdfsub = bigdf[date2022, ]

##############################################
get_rmse = function(obs, mod){
  return(sqrt(mean((obs - mod)^2)))
}
###############################################

# constants
N_iter = 10
num_reps = 16
test_reps = 31-num_reps

start = 1
# iterate over DOY
for (i in 2:(365/5)){
  print(i)
  DOY_indices = start:(start+4)
  smalldf = filter(bigdfsub, DOY %in% DOY_indices)
  
  ens_no = rep(rep(1:31, each = 10), 5)
  smalldf$ens_no = ens_no
  
  my_means = smalldf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
    summarise(mean_temp = mean(Temp_C_00UTC), start_date_obs_temp = mean(start_date_obs_temp))
  
  trainDummy = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_means$start_date_obs_temp,
                          DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date)
  
  # for each subset of 5 days, do 100 mcmc reps of train/test subsets
  fracList = list(length=100)
  for (num in 1:N_iter){
    trainlist = list()
    testlist = list()
    for (j in 1:nrow(trainDummy)){
      tempdf = filter(smalldf, Depth == trainDummy$Depth[j], DOY == trainDummy$DOY[j],
                      start_date == trainDummy$start_date[j], Horizon == trainDummy$Horizon[j])
      trainIndex = sample(1:31, size=num_reps)
      testindex = (1:31)[-trainIndex]
      train = filter(tempdf, ens_no %in% trainIndex)
      test = filter(tempdf, ens_no %in% testindex)
      trainlist[[j]] = train
      testlist[[j]] = test
    }
    traindf = do.call("rbind", trainlist)
    testdf = do.call("rbind", testlist)
    
    # Vecchia approach using sqrt transform/uncertainty in mean/95th quantile of variance
    SK_fit = fit_my_skv2(traindf, testdf, num_reps, test_reps)
    var_mean_orig = SK_fit$var_mean_orig
    vecDF = SK_fit$vecDF    

    #######################################
    # calculate coverage/RMSE
    #######################################
    
    ############### CI ####################
    myDOY = unique(vecDF$DOY)
    frac_vec2 = vector(length = length(myDOY))
    for (j in 1:length(myDOY)){
      tempdf = filter(vecDF, DOY == myDOY[j])
      n = nrow(tempdf)
      # vecchia
      inside = length(tempdf$ybar[(tempdf$ybar <= tempdf$SKupper) & (tempdf$ybar >= tempdf$SKlower)])
      frac_inside = inside / n
      frac_vec2[j] = frac_inside
    }
    fracDF = data.frame(DOY = myDOY, My_coverage_95 = frac_vec2)
    
    ################ PI/RMSE ##################
    frac_vec2 = vector(length = length(myDOY))
    frac_vec4 = vector(length = length(myDOY))
    for (j in 1:length(myDOY)){
      tempdf = filter(vecDF, DOY == myDOY[j]) %>% select(DOY, Depth, Horizon, SKlowerPI, SKupperPI,
                                                          MyMean)
      temptest = filter(testdf, DOY == myDOY[j]) %>% select(DOY, Depth, Horizon, Temp_C_00UTC, ens_no)
      alldf = right_join(temptest, tempdf, by = c("DOY", "Depth", "Horizon"))
      n = nrow(alldf)
      vec_rmse = get_rmse(alldf$Temp_C_00UTC, alldf$MyMean)
      frac_vec4[j] = vec_rmse
      # vecchia
      inside = length(alldf$Temp_C_00UTC[(alldf$Temp_C_00UTC <= alldf$SKupperPI) & (alldf$Temp_C_00UTC >= alldf$SKlowerPI)])
      frac_inside = inside / n
      frac_vec2[j] = frac_inside
    }
    fracDFPI = data.frame(DOY = myDOY, My_PIcoverage_95 = frac_vec2)
    Mydf = data.frame(DOY = myDOY, VecchiaPI95 = fracDFPI$My_PIcoverage_95, VecchiaCI95 = fracDF$My_coverage_95,
                      VecchiaRMSE = frac_vec4)
    fracList[[num]] = Mydf
 
    
  }
 
  allfracdf = do.call("rbind", fracList)
  saveRDS(allfracdf, paste0("AllFracDF",i, ".Rds"))
  start=start+5
  print(paste("start is: ", start))
}
print("done")




