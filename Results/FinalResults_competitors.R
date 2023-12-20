library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr)

# carry out all anaylses presented in results
# Make figures 7 and 8

# Results (raw) from GPBC
results_folder = "../Data/RESULTS7.17"
results = list.files(results_folder)[grep("preds\\d{4}-\\d{2}-\\d{2}", list.files(results_folder))]

resList = list(length = length(results))
for (i in 1:length(results)){
  resList[[i]] = data.table::fread(file.path(results_folder, results[i]))
}
allRes = data.table::rbindlist(resList)

# functions for RMSE/proper (log) score
get_rmse = function(obs, mod){
  return(sqrt(mean((obs - mod)^2)))
}

get_proper_score=function(obs, y_mean, y_sd){
  score = mean(- (obs - y_mean)^2/(y_sd)^2 - log(y_sd^2))
}

# Climatological model results (test ONLY--> train on train, predict on test set, these are OOB results only)
obs_gp = read.csv("../Data/Observed_hetGP_withOb_TESTONLY.csv")
obs_gp = obs_gp[complete.cases(obs_gp), ]

# raw GLM forecasts for the test period
glm2 = read.csv("../Data/rawGLM_withObs.csv")
glm2 = filter(glm2, date >= as.Date("2022-06-11") & date <= as.Date("2023-06-10"))

###########################################
# Calculate the following:
# - RMSE
# - Log score (or proper Gaussian score)
# - 90% confidence interval coverage
############################################
###########
# RMSE
############
par(mfrow=c(1,1))
library(viridis)


Surr_BC_rmse = allRes %>% group_by(Horizon) %>% summarise(rmse = get_rmse(temp_obs, BC_mean))
Surr_NOBC_rmse = allRes %>% group_by(Horizon) %>% summarise(rmse = get_rmse(temp_obs, Mean))
Obs_rmse = get_rmse(obs_gp$temp_obs, obs_gp$mean)
GLM_rmse = glm2 %>% group_by(Horizon) %>% summarise(rmse = get_rmse(temp_obs, GLM_mean))

#png("threePanelMetrics.png", res = 300, height = 2000, width = 3500)
par(mfrow=c(1,3))
plot(Surr_BC_rmse$Horizon, Surr_BC_rmse$rmse, type = "l", col = "#E66100",
     ylab = "RMSE (°C)", xlab = "Horizon",
     ylim = c(.3,2), xlim=c(0,30), lwd = 2, main = "RMSE", cex.lab=2, cex.main= 2, cex.axis=2)
lines(Surr_NOBC_rmse$Horizon, Surr_NOBC_rmse$rmse, col = "#E66100", lty = 2, lwd = 2)
lines(1:30, rep(Obs_rmse, 30), col = "#503A9B", lwd = 2)
lines(1:30, GLM_rmse$rmse, col = "darkgray", lwd = 2)
text(2, 1.55, "A", cex=3)
legend("topleft", legend = c("GPBC", "GPGLM", "OGP", "GLM"),
       col = c("#E66100", "#E66100", "#503A9B", "darkgray"), lty = c(1,2,1,1), lwd=2, cex=1.7)

############################
# proper score by horizon
############################
Surr_BC_pscore = allRes %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, BC_mean, BC_sd))
Surr_NOBC_pscore = allRes %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, Mean, SD_SK))
Obs_pscore = get_proper_score(obs_gp$temp_obs, obs_gp$mean, obs_gp$hetSD)
GLM_pscore = glm2 %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, GLM_mean, GLM_std))
plot(Surr_BC_pscore$Horizon, Surr_BC_pscore$pscore, type = "l",
     ylab = "proper score",main="Proper score", xlab = "Horizon", xlim=c(0,30), ylim = c(-8,0), lwd = 2,
     col = "#E66100", cex.lab=2, cex.main=2, cex.axis=2)
lines(Surr_NOBC_pscore$Horizon, Surr_NOBC_pscore$pscore , col =  "#E66100", lty = 2, lwd= 2)
lines(1:30, rep(Obs_pscore, 30), col = "#503A9B", lwd = 2)
text(2, -1.45, "B", cex=3)
############################
# 90% CI coverage
############################

glm2$SE = glm2$GLM_std 
glm2$Lower = glm2$GLM_mean - 1.645*glm2$SE
glm2$Upper = glm2$GLM_mean + 1.645*glm2$SE
frac_vec = vector(length = 30)
for (i in 1:30){
  tempdf = filter(allRes, Horizon == i)
  n = nrow(tempdf)
  head(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$BCUpper) & (tempdf$temp_obs >= tempdf$BCLower)])
  frac_inside = inside / n
  frac_vec[i] = frac_inside
}

frac_vec2 = vector(length = 30)
for (i in 1:30){
  tempdf = filter(allRes, Horizon == i)
  n = nrow(tempdf)
  head(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$HetUpper) & (tempdf$temp_obs >= tempdf$HetLower)])
  frac_inside = inside / n
  frac_vec2[i] = frac_inside
}


# GLM
frac_vecGLM = vector(length = 30)
for (i in 1:30){
  tempdf = filter(glm2, Horizon == i)
  n = nrow(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$Upper) & (tempdf$temp_obs >= tempdf$Lower)])
  frac_inside = inside / n
  frac_vecGLM[i] = frac_inside
}


inside = length(obs_gp$temp_obs[(obs_gp$temp_obs <= obs_gp$Upper) & (obs_gp$temp_obs >= obs_gp$Lower)])
n = nrow(obs_gp)
frac_inside = inside / n

plot(1:30, frac_vec, type = "l",
     ylab = "90% CI coverage", main="90% PI coverage", xlab = "Horizon",
     xlim=c(0,30), ylim = c(.1, 1), col = "#E66100", lwd = 2, cex.lab = 2, cex.main=2, cex.axis=2)
lines(1:30, rep(frac_inside,30), col = "#503A9B", lwd = 2)
lines(1:30, frac_vec2, lwd = 2, col = "#E66100", lty=2)
lines(1:30, frac_vecGLM, col = "darkgray", lwd = 2)
text(2, .85, "C", cex=3)
#legend("bottomright", legend = c("GPBC", "GPO", "GLM"),
#       col = c("#E66100", "#503A9B", "darkgray"), lty = c(1,1,1), lwd=2)
dev.off()


#############################################################################################################
# Make Results Plots
# Make Figure 5
#############################################################################################################
library(dplyr)
library(ggpubr)
library(ggplot2)

# directory for GLM forecasts
horizon_dir = "../Data/HORIZON_TRAIN"
method = "Average"
lookback = 4
obs_depth = 1

biglist = list(length=30)
for (i in 1:30){
  temp = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                         "D", obs_depth, ".csv")))
  biglist[[i]] = temp
}

bigdf = data.table::rbindlist(biglist)
bigdf = as.data.frame(bigdf)
colnames(bigdf)[which(colnames(bigdf) == "depth_int")] = "Depth"
colnames(bigdf)[which(colnames(bigdf) == "horizon")] = "Horizon"
bigdf$start_date = as.POSIXct(bigdf$start_date)
bigdf$datetime = bigdf$start_date + lubridate::days(bigdf$Horizon)

bigdf = filter(bigdf, Depth == 1)
bigdf$V1=NULL
head(bigdf)

# get the bias GP resulting from validation run
biasfit = readRDS("../Data/SURROGATES/bias_surrogate.Rds")
obs_data = read.csv("../Data/SURROGATES/Bias_dataset_validation.csv")

ogp = read.csv("../Data/Observed_hetGP_withOb_TESTONLY.csv")

# merge w/ observed GP (test only)
ogp = filter(ogp, Depth == 1)
ogp = dplyr::select(ogp, mean, Lower, Upper, date)

colnames(ogp) = c("ObsGP_mean", "ObsGP_Lower", "ObsGP_Upper", "date")
ogp$date = as.character(ogp$date)

mydoy=50

smalldf = filter(bigdf, DOY == mydoy)
smalldf$start_date = as.character(smalldf$start_date)
smalldf = filter(smalldf, start_date == "2023-02-19")
smalldf$ensemble_no = rep(1:31, 30)

pred_sub = filter(allRes, DOY == mydoy, Depth == 1)

obs_sub = filter(obs_data, DOY == mydoy, Depth == 1)

bias_sub = filter(biasfit, DOY == mydoy, Depth == 1)

bias_sub$s_date = as.POSIXct(bias_sub$start_date)
bias_sub$date = bias_sub$s_date + lubridate::days(bias_sub$Horizon)
bias_sub$date = as.character(bias_sub$date)
bias_sub2 = right_join(bias_sub, ogp, by = "date")

obs_sub = filter(obs_sub, start_date == "2023-02-19")

#bias_sub2 = bias_sub2[complete.cases(bias_sub2), ]

obs_sub$ensemble_no = 1
pred_sub$ensemble_no = 1
bias_sub$ensemble_no = 1
bias_sub2$ensemble_no = 1


plotgpglm =  ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = 0.7) +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = Mean, col = "GPGLM"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations" ))+
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_mean, col = "OGP"), linewidth = 1.1) +
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_Lower), linetype = "dashed", linewidth = 1.1, col = "#999933") +
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_Upper), linetype = "dashed", linewidth = 1.1, col = "#999933") +
  ylim(c(1,13)) + 
  theme_bw() +
  scale_color_manual(breaks = c("GPGLM", "OGP", "Observations", "GLM"), 
                     values = c(GPGLM = "#D55E00", OGP = "#999933", Observations = "#503A9B",
                                GLM = "darkgray"),
                     labels = c("GPGLM",  "OGP", "Observations", "GLM"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "blank", "solid"),
                       shape = c(NA, NA, 16, NA))),
                     name="") +
  theme(legend.position = c(0.3, 0.92), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1))) +
  labs(tag = "A") 

plotgpbc = ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, group = ensemble_no)) +
  geom_line(alpha = 0.7, col = "darkgray") +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = BC_mean, col = "GPBC"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#E66100") +
  geom_line(data=pred_sub, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#E66100") +
  
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_mean), linewidth = 1.1, col = "#999933") +
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_Lower), linetype = "dashed", linewidth = 1.1, col = "#999933") +
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_Upper), linetype = "dashed", linewidth = 1.1, col = "#999933") +
  
  ylab("Temp (°C)") +
  xlab("Horizon (days)")+
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs), col ="#503A9B") +
  
  theme_bw() +
  
  scale_color_manual(breaks = c("GPBC"), 
                     values = c(GPBC = "#E66100"),
                     labels = c("GPBC"),
                     name = "") +
  labs(tag = "B") +
  ylim(c(1,13)) + 
  # scale_color_manual(breaks = c("GPBC", "OGP", "Observations", "GLM"), 
  #                    values = c(Observations = "#503A9B", OGP = "#999933", GLM = "darkgray", GPBC = "#E66100"),
  #                    labels = c("GPBC", "OGP", "Observations", "GLM"),
  #                    guide = guide_legend(override.aes = list(
  #                      linetype = c("solid", "solid", "blank", "blank"),
  #                      shape = c(NA, NA, 16, 16))),
  #                    name = "") +
  theme(legend.position = c(0.19, 0.99), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1))) 
#facet_wrap(~factor(start_date))
#
ggarrange(plotgpglm, plotgpbc, common.legend = FALSE)
#ggsave("BCnoBCTestperiodExample2.png")

################
# not important
################
obs = read_obs_data()
myobs = filter(obs, date > as.Date("2020-09-10"))
str(myobs)
head(obs2)
obs2 = dplyr::select(myobs, depth_int, temp_obs, datetime)
str(obs2)
library(data.table)
library(dplyr)
library(lubridate)
biglist = list(length=30)
for (i in 1:30){
  #fix this file path
  temp = data.table::fread(paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                  "D", obs_depth, ".csv"))
  biglist[[i]] = temp
}
bigdf = data.table::rbindlist(biglist)
bigdf = as.data.frame(bigdf)
bigdf$start_date = as.POSIXct(bigdf$start_date)
bigdf$date = bigdf$start_date + lubridate::days(bigdf$horizon)
bigdf$datetime = as.character(bigdf$date)
bigdf = filter(bigdf, date >= as.Date("2022-03-13") & date <= as.Date("2023-03-15"))
glm_means = bigdf %>% group_by(DOY, depth_int, horizon, datetime) %>% summarise(GLM_mean = mean(Temp_C_00UTC),
                                                                                GLM_std = sd(Temp_C_00UTC))


odate=unique(obs$datetime)
gdate = unique(glm_means$datetime)
odate[odate == "2021-01-02"]
t1=filter(obs2, datetime == "2021-01-02")
filter(glm_means, datetime == "2021-01-02")
glm2 = right_join(obs2, glm_means, by = c("depth_int", "datetime"))
glm2 = glm2[complete.cases(glm2), ]
saveRDS(glm2, "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/Data/rawGLM_withObs.csv")

head(glm2)


