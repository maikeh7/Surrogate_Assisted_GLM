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
N_iter = 30
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
  fracList = list(length=N_iter)
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
    
    my_means = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_temp = mean(Temp_C_00UTC), start_date_obs_temp = mean(start_date_obs_temp))
    
    # Raw unique input locations
    inputsXtrain = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_means$start_date_obs_temp,
                              DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                              ybar = my_means$mean_temp)
    
    # standardize inputs to be in [0,1]
    DOY_range = range(inputsXtrain$DOY)
    horizon_range = range(inputsXtrain$Horizon)
    depth_range = range(inputsXtrain$Depth)
    tempcovar_range = range(inputsXtrain$start_date_obs_temp)
    
    DOY_standardized = (inputsXtrain$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
    horizon_standardized = (inputsXtrain$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
    tempcovar_standardized = (inputsXtrain$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
    depth_standardized = (inputsXtrain$Depth - depth_range[1]) / (depth_range[2] - depth_range[1])
    
    Xtrain = cbind(DOY_standardized, horizon_standardized, tempcovar_standardized, depth_standardized)
    Ytrain = my_means$mean_temp  
    ################################################
    # process test data
    my_means_test = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_temp = mean(Temp_C_00UTC), start_date_obs_temp = mean(start_date_obs_temp))
    
    # we will put results in this df for easy plotting and calculating metrics
    inputsXTest = data.frame(Horizon = my_means_test$Horizon, start_date_obs_temp = my_means_test$start_date_obs_temp,
                             DOY = my_means_test$DOY, Depth = my_means_test$Depth, start_date = my_means_test$start_date, 
                             ybar = my_means_test$mean_temp)
    
    # Vecchia approach using sqrt transform/uncertainty in mean/95th quantile of variance
    SK_fit = fit_my_skv2(traindf, testdf, num_reps, test_reps)
    var_mean_orig = SK_fit$var_mean_orig
    vecDF = SK_fit$vecDF
    
    # True SK, using mean of 'variance GP' obtained from Vecchia above for the variance
    model <- km(Y~1, design=Xtrain, response=data.frame(y=Ytrain),
                covtype="gauss", coef.trend=NULL, coef.cov=NULL, coef.var=NULL,
                noise.var = var_mean_orig/num_reps, control=list(trace=FALSE))
    
    # prediction --NOTE Xtrain is the same as the test input lcos!
    p <- predict.km(model, newdata = Xtrain, type="UK", cov.compute = TRUE)
    my_var = diag(p$cov)
    # modify variance to be reasonable for out of sample prediction
    SKSD = sqrt(my_var + var_mean_orig) # just using Ztest here...no dividing b/c of Bobby's formula
    SKSD2 = sqrt(my_var + var_mean_orig/test_reps) # need to make this adjustment 
    mypreds = p$mean
    
    lowerPI = mypreds - q*SKSD
    upperPI = mypreds + q*SKSD
    lower = mypreds - q*(SKSD2)
    upper = mypreds + q*(SKSD2)
    
    inputsXTest$SKPreds = mypreds
    inputsXTest$lower95 = lower
    inputsXTest$upper95 = upper
    inputsXTest$lowerPI = lowerPI
    inputsXTest$upperPI = upperPI
    TrueSK_df = inputsXTest
    
    vecDF$SKPreds = TrueSK_df$SKPreds
    vecDF$lower95 = TrueSK_df$lower95
    vecDF$upper95 = TrueSK_df$upper95
    vecDF$lowerPI = TrueSK_df$lowerPI
    vecDF$upperPI = TrueSK_df$upperPI
    
    #######################################
    # calculate coverage/RMSE
    #######################################
    
    ############### CI ####################
    myDOY = unique(vecDF$DOY)
    frac_vec2 = vector(length = length(myDOY))
    for (j in 1:length(myDOY)){
      tempdf = filter(vecDF, DOY == myDOY[j])
      n = nrow(tempdf)
      # True SK
      inside_true = length(tempdf$ybar[(tempdf$ybar <= tempdf$upper95) & (tempdf$ybar >= tempdf$lower95)])
      frac_inside_true = inside_true / n
      frac_vec2[j] = frac_inside_true
    }
    fracDF = data.frame(DOY = myDOY, True_coverage_95 = frac_vec2)
    
    ################ PI/RMSE ##################
    frac_vec3 = vector(length = length(myDOY))
    frac_vec5 = vector(length = length(myDOY))
    for (j in 1:length(myDOY)){
      tempdf = filter(vecDF, DOY == myDOY[j]) %>% select(DOY, Depth, Horizon, SKlowerPI, SKupperPI,
                                                         lowerPI, upperPI, SKPreds)
      temptest = filter(testdf, DOY == myDOY[j]) %>% select(DOY, Depth, Horizon, Temp_C_00UTC, ens_no)
      alldf = right_join(temptest, tempdf, by = c("DOY", "Depth", "Horizon"))
      n = nrow(alldf)
      SK_rmse = get_rmse(alldf$Temp_C_00UTC, alldf$SKPreds)
      frac_vec5[j] = SK_rmse
      # True SK
      inside_true = length(alldf$Temp_C_00UTC[(alldf$Temp_C_00UTC <= alldf$upperPI) & (alldf$Temp_C_00UTC >= alldf$lowerPI)])
      frac_inside_true = inside_true / n
      frac_vec3[j] = frac_inside_true
    }
    fracDFPI = data.frame(DOY = myDOY, True_PIcoverage_95 = frac_vec3)
    Mydf = data.frame(DOY = myDOY, SKPI95 = fracDFPI$True_PIcoverage_95, SKCI95 = fracDF$True_coverage_95,
                      SKRMSE = frac_vec5)
    fracList[[num]] = Mydf
 
    
  }
 
  allfracdf = do.call("rbind", fracList)
  saveRDS(allfracdf, paste0("RESULTS/AllFracDFSK",i, ".Rds"))
  start=start+5
  print(paste("start is: ", start))
}
print("done")




