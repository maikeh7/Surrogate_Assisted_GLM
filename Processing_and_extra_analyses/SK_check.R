library(dplyr)
library(data.table)
library(DiceKriging)
source("SVecchia_Katzfussetal.R")


biglist = list(length=30)
#horizon_dir = "../Data/HORIZON_TRAIN"
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

get_rmse = function(obs, mod){
  return(sqrt(mean((obs - mod)^2)))
}
print("made it")
trainIndex = sample(1:31, size=16)
testindex = (1:31)[-trainIndex]
compare_kriging2 = function(bigdfsub){
  # calculate means for stochastic kriging
  for (i in 354:(365-5)){
    print(i)
    DOY_indices = i:(i+4)
    smalldf = filter(bigdfsub, DOY %in% DOY_indices)
    
    ens_no = rep(rep(1:31, each = 10),5)
    smalldf$ens_no = ens_no
    
    traindf = filter(smalldf, ens_no %in% trainIndex)

    my_mean_starts = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(start_date_obs_temp = mean(start_date_obs_temp))
    
    my_means = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_temp = mean(Temp_C_00UTC))
    
    # do same for variance
    my_Vardf = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_var = var(Temp_C_00UTC))

    
    inputsX = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_mean_starts$start_date_obs_temp,
                         DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                         ybar = my_means$mean_temp)
    
    # standardize inputs to be in [0,1]
    DOY_range = range(inputsX$DOY)
    horizon_range = range(inputsX$Horizon)
    depth_range = range(my_means$Depth)
    tempcovar_range = range(inputsX$start_date_obs_temp)
    
    DOY_standardized <- (inputsX$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
    horizon_standardized <- (inputsX$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
    tempcovar_standardized <- (inputsX$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
    depth_standardized <- (inputsX$Depth - depth_range[1]) / (depth_range[2] - depth_range[1])
    
    Xtrain <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized, depth_standardized)
    Ytrain <- my_means$mean_temp
    Ztrain <- my_Vardf$mean_var
    
    testdf = filter(smalldf, ens_no %in% testindex)
    
    my_mean_starts = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(start_date_obs_temp = mean(start_date_obs_temp))
    
    my_means = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_temp = mean(Temp_C_00UTC))

    # do same for variance
    my_Vardf = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_var = var(Temp_C_00UTC))

    inputsXTest = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_mean_starts$start_date_obs_temp,
                         DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                         ybar = my_means$mean_temp)
    
    # standardize inputs to be in [0,1]
    DOY_range = range(inputsXTest$DOY)
    horizon_range = range(inputsXTest$Horizon)
    depth_range = range(inputsXTest$Depth)
    tempcovar_range = range(inputsXTest$start_date_obs_temp)
    
    DOY_standardized <- (inputsXTest$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
    horizon_standardized <- (inputsXTest$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
    tempcovar_standardized <- (inputsXTest$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
    depth_standardized <- (inputsXTest$Depth - depth_range[1]) / (depth_range[2] - depth_range[1])
    
    Xtest <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized, depth_standardized)
    Ytest <- my_means$mean_temp
    Ztest <- my_Vardf$mean_var
    
    # kriging model definition (no estimation here)
    model <- km(Y~1, design=Xtrain, response=data.frame(y=Ytrain),
                covtype="gauss", coef.trend=NULL, coef.cov=NULL, coef.var=NULL,
                noise.var = Ztrain/16)
    
    # prediction
    p <- predict.km(model, newdata = Xtest, type="UK", cov.compute = TRUE)
    
    my_var = diag(p$cov)
    SKSD = sqrt(my_var + Ztest) # just using Ztest here...no dividing b/c of Bobby's formula
    mypreds = p$mean
    lower <- p$lower95; upper <- p$upper95
    lowerPI = mypreds - 1.96*SKSD
    upperPI = mypreds + 1.96*SKSD

    inputsXTest$SKPreds = mypreds
    inputsXTest$lower95 = lower
    inputsXTest$upper95 = upper
    inputsXTest$lowerPI = lowerPI
    inputsXTest$upperPI = upperPI
    TrueSK_df = inputsXTest
###################################################################
###################################################################
    
    traindf = filter(smalldf, ens_no %in% trainIndex)
    
    my_mean_starts = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(start_date_obs_temp = mean(start_date_obs_temp))
    
    my_means = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_temp = mean(Temp_C_00UTC))
    
    # normalize response (temperature)
     temp_mean = mean(my_means$mean_temp)
     temp_sd = sd(my_means$mean_temp)
     temp_normalized = (my_means$mean_temp - temp_mean ) / temp_sd
    
    # do same for variance
    my_Vardf = traindf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_var = var(Temp_C_00UTC))
     var_mean = mean(my_Vardf$mean_var)
     var_sd = sd(my_Vardf$mean_var)
     var_normalized = (my_Vardf$mean_var - var_mean ) / var_sd

    
    inputsXtrain = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_mean_starts$start_date_obs_temp,
                         DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                         ybar = my_means$mean_temp)
    
    
    # standardize inputs to be in [0,1]
    DOY_range = range(inputsXtrain$DOY)
    horizon_range = range(inputsXtrain$Horizon)
    depth_range = range(inputsXtrain$Depth)
    tempcovar_range = range(inputsXtrain$start_date_obs_temp)
    
    DOY_standardized <- (inputsXtrain$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
    horizon_standardized <- (inputsXtrain$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
    tempcovar_standardized <- (inputsXtrain$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
    depth_standardized <- (inputsXtrain$Depth - depth_range[1]) / (depth_range[2] - depth_range[1])
    
    Xtrain <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized, depth_standardized)
    Ytrain <- temp_normalized
    Ztrain <- var_normalized
    
################################################
    #head(traindf)
    #head(testdf)
    testdf = filter(smalldf, ens_no %in% testindex)
    
    my_mean_starts = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(start_date_obs_temp = mean(start_date_obs_temp))
    
    my_means = testdf %>% group_by(start_date, Horizon, DOY, Depth) %>% 
      summarise(mean_temp = mean(Temp_C_00UTC))

    
    inputsXTest = data.frame(Horizon = my_means$Horizon, start_date_obs_temp = my_mean_starts$start_date_obs_temp,
                             DOY = my_means$DOY, Depth = my_means$Depth, start_date = my_means$start_date, 
                             ybar = my_means$mean_temp)
    
    # standardize inputs to be in [0,1]
    DOY_range = range(inputsXTest$DOY)
    horizon_range = range(inputsXTest$Horizon)
    depth_range = range(inputsXTest$Depth)
    tempcovar_range = range(inputsXTest$start_date_obs_temp)
    
    DOY_standardized <- (inputsXTest$DOY - DOY_range[1]) / (DOY_range[2] - DOY_range[1])
    horizon_standardized <- (inputsXTest$Horizon - horizon_range[1]) / (horizon_range[2] - horizon_range[1]) 
    tempcovar_standardized <- (inputsXTest$start_date_obs_temp - tempcovar_range[1]) / (tempcovar_range[2] - tempcovar_range[1])
    depth_standardized <- (inputsXTest$Depth - depth_range[1]) / (depth_range[2] - depth_range[1])
    
    Xtest <- cbind(DOY_standardized, horizon_standardized, tempcovar_standardized, depth_standardized)
   # Ytest <- temp_normalized_test
  #  Ztest <- var_normalized_test
    
    # size of conditioning sets for scaled Vecchia
    m <- 30
    est <- fit_scaled(Ytrain, Xtrain,  ms = m, nug = NULL, trend = "zero")
    fit <- predictions_scaled(est, Xtest, m = m, joint = FALSE, predvar = TRUE)
    mean_orig <- fit$means * temp_sd + temp_mean

    vecDF = inputsXTest
    vecDF$MyMean = mean_orig
    ##############################################################################################
    ##############################################################################################
    # fit variance
    m=30
    est <- fit_scaled(Ztrain, Xtrain,  ms = m, nug = NULL, trend = "zero")
    #saveRDS(est, file.path(surrogate_dir, "SK_Variance_fit.Rds"))
    # make predictions
    # we don't need the variance for anything, only the mean, which is our estimation of variance
    fit <- predictions_scaled(est, Xtrain, m = m, joint = FALSE, predvar = TRUE)
    
    var_mean_orig <- exp( fit$means * var_sd + var_mean )
    #s2_orig <- (exp( fit$vars * (var_sd^2) ))
    
    vecDF$SD_SK = sqrt(var_mean_orig) / sqrt(16)
    vecDF$Var_SK = var_mean_orig / 16
    vecDF$SKlower <- qnorm(0.025, vecDF$MyMean, vecDF$SD_SK)
    vecDF$SKupper <- qnorm(0.975, vecDF$MyMean, vecDF$SD_SK)
    
    vecDF$SD_SK_PI = sqrt(var_mean_orig)
    vecDF$SKlowerPI <- qnorm(0.025, vecDF$MyMean, vecDF$SD_SK_PI)
    vecDF$SKupperPI <- qnorm(0.975, vecDF$MyMean, vecDF$SD_SK_PI)

    
    # results for true SK
    vecDF$SKPreds = TrueSK_df$SKPreds
    vecDF$lower95 = TrueSK_df$lower95
    vecDF$upper95 = TrueSK_df$upper95
    vecDF$lowerPI = TrueSK_df$lowerPI
    vecDF$upperPI = TrueSK_df$upperPI
    
    # test = res %>% group_by(Horizon) %>% summarise(meantrue = mean(True_CI_width), MeanMy = mean(My_CI_width))
    # plot(test$MeanMy, col = "blue", ylim=c(0,2))
    # points(test$meantrue, col = "red")
    #  test = filter(vecDF, start_date == "2022-01-01")
    # 
    #  ggplot(test, aes(x = Horizon, y = MyMean)) + 
    #    geom_line() +
    #    geom_line(data = test, aes(x = Horizon, y = SKupper)) +
    #    geom_line(data = test, aes(x = Horizon, y = SKlower)) +
    #    facet_wrap(~Depth)
    #  filter(vecDF, Horizon ==1)
    #res = vecDF %>% group_by(Horizon, Depth, DOY) %>%
    #  summarise(True_SK_RMSE = get_rmse(SKPreds, ybar), My_RMSE = get_rmse(MyMean, ybar),
    #            True_CI_width = upper95 - lower95, My_CI_width = SKupper - SKlower)
    
    myDOY = unique(vecDF$DOY)
    frac_vec2 = vector(length = length(myDOY))
    frac_vec3 = vector(length = length(myDOY))
    for (j in 1:length(myDOY)){
      tempdf = filter(vecDF, DOY == myDOY[j])
      n = nrow(tempdf)
      
      inside = length(tempdf$ybar[(tempdf$ybar <= tempdf$SKupper) & (tempdf$ybar >= tempdf$SKlower)])
      frac_inside = inside / n
      frac_vec2[j] = frac_inside
      # True SK
      inside_true = length(tempdf$ybar[(tempdf$ybar <= tempdf$upper95) & (tempdf$ybar >= tempdf$lower95)])
      frac_inside_true = inside_true / n
      frac_vec3[j] = frac_inside_true
    }
    fracDF = data.frame(DOY = myDOY, My_coverage_95 = frac_vec2, True_coverage_95 = frac_vec3)
    
    ################ PI #########################
    frac_vec2 = vector(length = length(myDOY))
    frac_vec3 = vector(length = length(myDOY))
    for (j in 1:length(myDOY)){
      tempdf = filter(vecDF, DOY == myDOY[j]) %>% select(DOY, Depth, Horizon, SKlowerPI, SKupperPI,
                                                      lowerPI, upperPI)
      temptest = filter(testdf, DOY == myDOY[j]) %>% select(DOY, Depth, Horizon, Temp_C_00UTC, ens_no)
      alldf = right_join(temptest, tempdf, by = c("DOY", "Depth", "Horizon"))
      n = nrow(alldf)
  
      inside = length(alldf$Temp_C_00UTC[(alldf$Temp_C_00UTC <= alldf$SKupperPI) & (alldf$Temp_C_00UTC >= alldf$SKlowerPI)])
      frac_inside = inside / n
      frac_vec2[j] = frac_inside
      # True SK
      inside_true = length(alldf$Temp_C_00UTC[(alldf$Temp_C_00UTC <= alldf$upperPI) & (alldf$Temp_C_00UTC >= alldf$lowerPI)])
      frac_inside_true = inside_true / n
      frac_vec3[j] = frac_inside_true
    }
    fracDFPI = data.frame(DOY = myDOY, My_PIcoverage_95 = frac_vec2, True_PIcoverage_95 = frac_vec3)
    
    resdf = right_join(vecDF, fracDF, by = "DOY")
    resdf2 = right_join(resdf, fracDFPI, by = "DOY")

    write.csv(resdf2, paste0("RESULTS/results", i, ".csv"))
  }
}
compare_kriging2(bigdfsub)