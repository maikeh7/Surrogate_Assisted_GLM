library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr)

# carry out all anaylses presented in results
# Make figures 7 and 8

# Results (raw) from GPBC
results_folder = "../Data/RESULTS2.28.2024"
results_folder = "../Data/RESULTS4.30.2024"
results = list.files(results_folder)[grep("preds\\d{4}-\\d{2}-\\d{2}", list.files(results_folder))]

resList = list(length = length(results))
for (i in 1:length(results)){
  resList[[i]] = data.table::fread(file.path(results_folder, results[i]))
}

allRes = data.table::rbindlist(resList)
head(allRes)
allRes$SD_SK = sqrt(allRes$Var_SK_divReps*31)

######################################################################
results_folder = "../Data/GPBCphi"
results = list.files(results_folder)[grep("preds\\d{4}-\\d{2}-\\d{2}", list.files(results_folder))]
results
resListphi = list(length = length(results))
for (i in 1:length(results)){
  resListphi[[i]] = data.table::fread(file.path(results_folder, results[i]))
}

allResphi = data.table::rbindlist(resListphi)
head(allRes)
allResphi$SD_SK = sqrt(allResphi$Var_SK_divReps*31)

# functions for RMSE/proper (log) score
get_rmse = function(obs, mod){
  return(sqrt(mean((obs - mod)^2, na.rm = TRUE)))
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

Surr_BC_rmse = allRes %>% group_by(Depth) %>% summarise(rmse = get_rmse(temp_obs, BC_mean))
Surr_NOBC_rmse = allRes %>% group_by(Depth) %>% summarise(rmse = get_rmse(temp_obs, Mean))
Surr_BCphi_rmse = allResphi %>% group_by(Depth) %>% summarise(rmse = get_rmse(temp_obs, BC_mean))
Obs_rmse = obs_gp %>% group_by(Depth) %>% summarise(rmse = get_rmse(temp_obs, mean))
GLM_rmse = glm2 %>% group_by(Depth) %>% summarise(rmse = get_rmse(temp_obs, GLM_mean))

#png("threePanelMetrics.png", res = 300, height = 2000, width = 3700)
pdf("threePanelMetricsDEPTH.pdf", height=8, width=18)
par(mar=c(7,5,4,1)+.1)
oldp <- par(mgp=c(3, 1.5, 0))
par(mfrow=c(1,4))
plot(Surr_BC_rmse$Depth, Surr_BC_rmse$rmse, type = "l", col = "#E66100",
     ylab = "", xlab = "",
     ylim = c(.3,2.5), xlim=c(0,9), lwd = 3, main = "RMSE (A)", cex.lab=3.5, cex.main = 3, cex.axis=3.5)
lines(Surr_BCphi_rmse$Depth, Surr_BCphi_rmse$rmse, col = "#E66100", lty = 3, lwd = 3)
lines(Surr_NOBC_rmse$Depth, Surr_NOBC_rmse$rmse, col = "#E66100", lty = 2, lwd = 3)
lines(0:9, Obs_rmse$rmse, col = "#503A9B", lwd = 3)
lines(0:9, GLM_rmse$rmse, col = "darkgray", lwd = 3)
mtext("Depth", side=1, line=5, cex = 3)
#text(.5,1, "A", cex=3)
legend("topright", legend = c("GPBC",  expression(paste("GPBC w/o ", phi)), "GPGLM", "OGP", "GLM"),
       col = c("#E66100", "#E66100","#E66100", "#503A9B", "darkgray"), lty = c(1,3,2,1,1), lwd=2, cex=2)

############################
# proper score by horizon
############################
Surr_BC_pscore = allRes %>% group_by(Depth) %>% summarise(pscore = get_proper_score(temp_obs, BC_mean, BC_sd))

# to get a more reasonable plot, replace horizon 15 sd_sk w/ that of horizon 14
# b/c the score is like -20K so it throws everything off!!
# same with horizon 24 depth 8 DOY 231...
allTemp = allRes
allTemp[DOY == 135 & Depth == 8 & Horizon ==15, ]$SD_SK = 0.016776600
allTemp[DOY == 231 & Depth == 8 & Horizon ==24, ]$SD_SK = 0.0099

Surr_NOBC_pscore = allTemp %>% group_by(Depth) %>% summarise(pscore = get_proper_score(temp_obs, Mean,
                                                                                         SD_SK))

Surr_BCphi_pscore = allResphi %>% group_by(Depth) %>% summarise(pscore = get_proper_score(temp_obs, BC_mean,
                                                                                            BC_sd))
Obs_pscore = obs_gp %>% group_by(Depth) %>% summarise(pscore=get_proper_score(temp_obs, mean, hetSD))
GLM_pscore = glm2 %>% group_by(Depth) %>% summarise(pscore = get_proper_score(temp_obs, GLM_mean, GLM_std))
plot(Surr_BC_pscore$Depth, Surr_BC_pscore$pscore, type = "l",
     ylab = "",main="Proper score (B)", xlab = "", xlim=c(0,9), ylim=c(-11,0), lwd = 3,
     col = "#E66100", cex.lab=3.5, cex.main=3.5, cex.axis=3.5)
lines(Surr_BCphi_pscore$Depth, Surr_BCphi_pscore$pscore , col =  "#E66100", lty = 3, lwd= 3)
lines(Surr_NOBC_pscore$Depth, Surr_NOBC_pscore$pscore , col =  "#E66100", lty = 2, lwd= 3)
lines(0:9, Obs_pscore$pscore, col = "#503A9B", lwd = 3)
mtext("Depth", side=1, line=5, cex = 3)
#text(.5, -1.5, "B", cex=3)
############################
# 90% CI coverage
############################

glm2$SE = glm2$GLM_std 
glm2$Lower = glm2$GLM_mean - 1.645*glm2$SE
glm2$Upper = glm2$GLM_mean + 1.645*glm2$SE
frac_vec = vector(length = 10)
for (i in 0:9){
  tempdf = filter(allRes, Depth == i)
  n = nrow(tempdf)
  head(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$BCUpper) & (tempdf$temp_obs >= tempdf$BCLower)])
  frac_inside = inside / n
  frac_vec[(i+1)] = frac_inside
}

frac_vec2 = vector(length = 10)
for (i in 0:9){
  tempdf = filter(allRes, Depth == i)
  n = nrow(tempdf)
  head(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$HetUpperPI) & (tempdf$temp_obs >= tempdf$HetLowerPI)])
  frac_inside = inside / n
  frac_vec2[(i+1)] = frac_inside
}

frac_vecPhi = vector(length = 10)
for (i in 0:9){
  tempdf = filter(allResphi, Depth == i)
  n = nrow(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$HetUpperPI) & (tempdf$temp_obs >= tempdf$HetLowerPI)])
  frac_inside = inside / n
  frac_vecPhi[(i+1)] = frac_inside
}


# GLM
frac_vecGLM = vector(length = 10)
for (i in 0:9){
  tempdf = filter(glm2, Depth == i)
  n = nrow(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$Upper) & (tempdf$temp_obs >= tempdf$Lower)])
  frac_inside = inside / n
  frac_vecGLM[(i+1)] = frac_inside
}

frac_vecobs = vector(length = 10)
for (i in 0:9){
  tempdf = filter(obs_gp, Depth == i)
  n = nrow(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$Upper) & (tempdf$temp_obs >= tempdf$Lower)])
  frac_inside = inside / n
  frac_vecobs[(i+1)] = frac_inside
}


plot(0:9, frac_vec, type = "l",
     ylab = "", main="90% PI coverage (C)", xlab = "",
     xlim=c(0,9), ylim = c(0, 1), col = "#E66100", lwd = 2, cex.lab = 3.5, cex.main=3.5, cex.axis=3.5)
lines(0:9, frac_vecPhi, lwd = 3, col = "#E66100", lty=3)
lines(0:9, frac_vecobs, col = "#503A9B", lwd = 3)
lines(0:9, frac_vec2, lwd = 3, col = "#E66100", lty=2)
lines(0:9, frac_vecGLM, col = "darkgray", lwd = 3)
mtext("Depth", side=1, line=5, cex = 3)
#text(.5, .3, "C", cex=3)


PIWidthphi = allResphi %>% group_by(Depth) %>% summarise(meanBCwidth = mean(BCUpper-BCLower))
PIWidth = allRes %>% group_by(Depth) %>% summarise(meanBCwidth = mean(BCUpper-BCLower),
                                                     NOBCWidth = mean(HetUpperPI - HetLowerPI))
Obswidth = obs_gp %>% group_by(Depth) %>% summarise(obsGPwidth = mean(Upper - Lower))

GLMWidth = glm2 %>% group_by(Depth) %>% summarise(glmwidth = mean(Upper - Lower))

plot(0:9,  PIWidth$meanBCwidth, type = "l",
     ylab = "", main="90% PI width (D)", xlab = "",
     xlim=c(0,9), ylim = c(.1, 7), col = "#E66100", lwd = 2.5, cex.lab = 3.5, cex.main=3.5, cex.axis=3.5)
lines(0:9, PIWidthphi$meanBCwidth, lwd = 3, col = "#E66100", lty=3)
lines(0:9, Obswidth$obsGPwidth, col = "#503A9B", lwd = 3)
lines(0:9, PIWidth$NOBCWidth, lwd = 3, col = "#E66100", lty=2)
lines(0:9, GLMWidth$glmwidth, col = "darkgray", lwd = 3)
mtext("Depth", side=1, line=5, cex = 3)
dev.off()

###################################################################################################################
## novel visual
d_names <- as_labeller(
  c(`0` = "Surface", `2` = "2m", `4` = "4m", `6` = "6m", `8` = "8m"))
h1 = filter(allRes, Horizon == 30)
h2= h1 %>% group_by(DOY, Depth) %>% summarise(mymean = mean(Mean), 
                                              bcmean = mean(BC_mean)) %>% filter(Depth %in% c(0,2, 4,6,8))
h3= h1 %>% group_by(DOY, Depth) %>% summarise(mymean = mean(temp_obs)) %>% filter(Depth %in% c(0,2, 4,6,8))
p30=ggplot(h2, aes(x = DOY, y = mymean, col = "GPGLM")) +
  geom_line(linewidth=.7, linetype = "solid", alpha=.8) +
  geom_line(data= h2, aes(x = DOY, y = bcmean, col = "GPBC"), linewidth=.7, alpha=.7) + 
  geom_point(data=h3, aes(x = DOY, y = mymean, col = "Obs"), alpha=.5, size=.35) +
  theme_bw() +
  ylab("Temp (°C)") +
  scale_color_manual(breaks = c("GPBC", "GPGLM", "Obs"), 
                     values = c(GPBC = "#D55E00", GPGLM = "#6699CC",
                                Obs = "#503A9B"),
                     labels = c("GPBC", "GPGLM", "Obs"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","solid", "blank"),
                       shape = c(NA, NA, 16), size = c(NA, NA, 1))),
                     name="") +
  ggtitle("Horizon 30") +
  theme(legend.position = c(0.90, 0.85), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        axis.text.x = element_text(angle = 45, hjust=1, size = 14),
        axis.text.y = element_text(size = 14),axis.title = element_text(size=14),
        strip.text.x = element_text(size = 13)) +
  facet_wrap(~Depth, ncol=5, labeller = d_names) 

h1 = filter(allRes, Horizon == 1)
h2= h1 %>% group_by(DOY, Depth) %>% summarise(mymean = mean(Mean),
                                              bcmean = mean(BC_mean)) %>% filter(Depth %in% c(0,2, 4,6,8))
h3= h1 %>% group_by(DOY, Depth) %>% summarise(mymean = mean(temp_obs)) %>% filter(Depth %in% c(0,2, 4,6,8))
p1=ggplot(h2, aes(x = DOY, y = mymean, col = "GPGLM")) +
  geom_line(linewidth=.7, alpha=.8) +
  geom_line(data= h2, aes(x = DOY, y = bcmean, col = "GPBC"), linewidth=.7, alpha=.7) + 
  geom_point(data=h3, aes(x = DOY, y = mymean, col = "Obs"),alpha=.5, size=.35) +
  theme_bw() +
  ylab("Temp (°C)") +
  scale_color_manual(breaks = c("GPBC", "GPGLM", "Obs"), 
                     values = c(GPBC = "#D55E00", GPGLM = "#6699CC",
                                Obs = "#503A9B"),
                     labels = c("GPBC", "GPGLM", "Obs"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","solid", "blank"),
                       shape = c(NA, NA, 16), size = c(NA, NA, 1))),
                     name="")  +
  ggtitle("Horizon 1") +
  theme(legend.position = c(0.90, 0.85), legend.text=element_text(size=16),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        axis.text.x = element_text(angle = 45,  hjust=1, size = 14),
        axis.text.y = element_text(size = 14), axis.title = element_text(size=14),
        strip.text.x = element_text(size = 13)) +
  facet_wrap(~Depth, ncol=5, labeller = d_names)
ggarrange(p1, p30, nrow=2)
ggsave("HorizonDOY.pdf", width = 2500, height= 1900, units = "px")




h1 = filter(allRes, Horizon == 1)
h2= h1 %>% group_by(DOY, Depth) %>% summarise(mymean = mean(BC_mean)) %>% filter(Depth %in% c(0,2, 4,6,8))
h3= h1 %>% group_by(DOY, Depth) %>% summarise(mymean = mean(temp_obs)) %>% filter(Depth %in% c(0,2, 4,6,8))
p1b=ggplot(h2, aes(x = DOY, y = mymean, col = "GPBC")) +
  geom_line(linewidth=1) +
  geom_point(data=h3, aes(x = DOY, y = mymean, col = "Obs"), alpha=.5, size=.5) +
  theme_bw() +
  ylab("Temp C") +
  scale_color_manual(breaks = c("GPBC", "Obs"), 
                     values = c(GPBC = "#D55E00",
                                Obs = "#503A9B"),
                     labels = c("GPBC", "Obs"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "blank"),
                       shape = c(NA, 16),
                       size = c(NA, 1)), alpha=1),
                     name="") +
  ggtitle("Horizon 1") +
  theme(legend.position = c(0.9, 0.9), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        axis.text.x = element_text(angle = 45,  hjust=1)) +
  facet_wrap(~Depth, ncol=5)


ggarrange(p30,p1, ncol=2)
ggsave("TEST.pdf", width = 3000, height= 2000, units = "px")

library(ggpubr)
