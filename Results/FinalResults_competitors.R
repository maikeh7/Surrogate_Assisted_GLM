library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr)

# carry out all anaylses presented in results
# Make figures 7 and 8

# Results (raw) from GPBC
#results_folder = "../Data/RESULTS2.28.2024"
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

# 'raw' GLM forecasts for the test period
#  **data is summarized with mean/sd over reps
glm2 = read.csv("../Data/rawGLM_withObs.csv")
glm2 = filter(glm2, date >= as.Date("2022-06-11") & date <= as.Date("2023-06-10"))
head(glm2)
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
Surr_BCphi_rmse = allResphi %>% group_by(Horizon) %>% summarise(rmse = get_rmse(temp_obs, BC_mean))
Obs_rmse = get_rmse(obs_gp$temp_obs, obs_gp$mean)
GLM_rmse = glm2 %>% group_by(Horizon) %>% summarise(rmse = get_rmse(temp_obs, GLM_mean))

#png("threePanelMetrics.png", res = 300, height = 2000, width = 3700)
pdf("threePanelMetrics.pdf", height=8, width=18)
par(mfrow=c(1,4))
par(mar=c(6,5,4,1)+.1)
oldp <- par(mgp=c(3, 2, 0))
plot(Surr_BC_rmse$Horizon, Surr_BC_rmse$rmse, type = "l", col = "#E66100",
     ylab = "", xlab = "",
     ylim = c(.3,2), xlim=c(0,30), lwd = 3, main = "RMSE (A)", cex.lab=3.5, cex.main = 3.5, cex.axis=3.5)
lines(Surr_BCphi_rmse$Horizon, Surr_BCphi_rmse$rmse, col = "#E66100", lty = 3, lwd = 3)
lines(Surr_NOBC_rmse$Horizon, Surr_NOBC_rmse$rmse, col = "#E66100", lty = 2, lwd = 3)
lines(1:30, rep(Obs_rmse, 30), col = "#503A9B", lwd = 3)
lines(1:30, GLM_rmse$rmse, col = "darkgray", lwd = 3)
#text(2, 1.55, "A", cex=3)
legend("bottomleft", legend = c("GPBC",  expression(paste("GPBC w/o ", phi)), "GPGLM", "OGP", "GLM"),
       col = c("#E66100", "#E66100","#E66100", "#503A9B", "darkgray"), lty = c(1,3,2,1,1), lwd=2, cex=2)
mtext("Horizon", side=1, line=5, cex = 3)
############################
# proper score by horizon
############################
Surr_BC_pscore = allRes %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, BC_mean, BC_sd))

# to get a more reasonable plot, replace horizon 15 sd_sk w/ that of horizon 14
# b/c the score is like -20K so it throws everything off!!
# same with horizon 24 depth 8 DOY 231...
allTemp = allRes
allTemp[DOY == 135 & Depth == 8 & Horizon ==15, ]$SD_SK = 0.016776600
allTemp[DOY == 231 & Depth == 8 & Horizon ==24, ]$SD_SK = 0.0099
Surr_NOBC_pscore = allTemp %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, Mean,
                                                                                        SD_SK))

Surr_BCphi_pscore = allResphi %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, BC_mean,
                                                                                         BC_sd))
Obs_pscore = get_proper_score(obs_gp$temp_obs, obs_gp$mean, obs_gp$hetSD)
GLM_pscore = glm2 %>% group_by(Horizon) %>% summarise(pscore = get_proper_score(temp_obs, GLM_mean, GLM_std))
plot(Surr_BC_pscore$Horizon, Surr_BC_pscore$pscore, type = "l",
     ylab = "",main="Proper score (B)", xlab = "", xlim=c(0,30), ylim = c(-13,0), lwd = 3,
     col = "#E66100", cex.lab=3.5, cex.main=3.5, cex.axis=3.5)
lines(Surr_BCphi_pscore$Horizon, Surr_BCphi_pscore$pscore , col =  "#E66100", lty = 3, lwd= 3)
lines(Surr_NOBC_pscore$Horizon, Surr_NOBC_pscore$pscore , col =  "#E66100", lty = 2, lwd= 3)
lines(1:30, rep(Obs_pscore, 30), col = "#503A9B", lwd = 3)
mtext("Horizon", side=1, line=5, cex = 3)
#text(2, -5, "B", cex=3)
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
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$HetUpperPI) & (tempdf$temp_obs >= tempdf$HetLowerPI)])
  frac_inside = inside / n
  frac_vec2[i] = frac_inside
}

frac_vecPhi = vector(length = 30)
for (i in 1:30){
  tempdf = filter(allResphi, Horizon == i)
  n = nrow(tempdf)
  head(tempdf)
  inside = length(tempdf$temp_obs[(tempdf$temp_obs <= tempdf$HetUpperPI) & (tempdf$temp_obs >= tempdf$HetLowerPI)])
  frac_inside = inside / n
  frac_vecPhi[i] = frac_inside
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
     ylab = "", main="90% PI coverage (C)", xlab = "",
     xlim=c(0,30), ylim = c(.1, 1), col = "#E66100", lwd = 3, cex.lab = 3.5, cex.main = 3.5, cex.axis=3.5)
lines(1:30, frac_vecPhi, lwd = 3, col = "#E66100", lty=3)
lines(1:30, rep(frac_inside,30), col = "#503A9B", lwd = 3)
lines(1:30, frac_vec2, lwd = 3, col = "#E66100", lty=2)
lines(1:30, frac_vecGLM, col = "darkgray", lwd = 3)
mtext("Horizon", side=1, line=5, cex = 3)
#text(2, .4, "C", cex=3)

PIWidthphi = allResphi %>% group_by(Horizon) %>% summarise(meanBCwidth = mean(BCUpper-BCLower))
PIWidth = allRes %>% group_by(Horizon) %>% summarise(meanBCwidth = mean(BCUpper-BCLower),
                                                     NOBCWidth = mean(HetUpperPI - HetLowerPI))
Obswidth = obs_gp %>% summarise(obsGPwidth = mean(Upper - Lower))
Odf = data.frame(Horizon = 1:30, obsGPwidth = Obswidth$obsGPwidth)
GLMWidth = glm2 %>% group_by(Horizon) %>% summarise(glmwidth = mean(Upper - Lower))

plot(1:30,  PIWidth$meanBCwidth, type = "l",
     ylab = "", main="90% PI width (D)", xlab = "",
     xlim=c(0,30), ylim = c(.1, 5), col = "#E66100", lwd = 3, cex.lab = 3.5, cex.main=3.5, cex.axis=3.5)
lines(1:30, PIWidthphi$meanBCwidth, lwd = 3, col = "#E66100", lty=3)
lines(1:30, rep(Obswidth$obsGPwidth,30), col = "#503A9B", lwd = 3)
lines(1:30, PIWidth$NOBCWidth, lwd = 3, col = "#E66100", lty=2)
lines(1:30, GLMWidth$glmwidth, col = "darkgray", lwd = 3)
mtext("Horizon", side=1, line=5, cex = 3)
#text(2, 4, "D", cex=3)

dev.off()
#############################################################################################################
PIWidthphi = allResphi %>% group_by(Horizon) %>% summarise(meanBCwidth = mean(BCUpper-BCLower))


PIWidth = allRes %>% group_by(Horizon) %>% summarise(meanBCwidth = mean(BCUpper-BCLower),
                                                   NOBCWidth = mean(HetUpperPI - HetLowerPI))
Obswidth = obs_gp %>% summarise(obsGPwidth = mean(Upper - Lower))
Odf = data.frame(Horizon = 1:30, obsGPwidth = Obswidth$obsGPwidth)
GLMWidth = glm2 %>% group_by(Horizon) %>% summarise(glmwidth = mean(Upper - Lower))
ggplot(data=PIWidth, aes(x = Horizon, y = meanBCwidth, col = "GPBC")) +
  geom_line(linewidth = 1) +
  geom_line(data = PIWidth, aes(x = Horizon, y = NOBCWidth, col = "GPGLM"), linetype = "dashed", linewidth = 1) +
  geom_line(data = PIWidthphi, aes(x = Horizon, y = meanBCwidth, col = "GPBCphi"), linetype = "dotted", linewidth = 1) +
  geom_line(data = Odf, aes(x = Horizon, y = obsGPwidth, col = "OGP"), linewidth = 1) +
  geom_line(data = GLMWidth, aes(x = Horizon, y = glmwidth, col = "GLM"), linewidth = 1) +
  theme_bw() +
  ylab("PI width C") +
  scale_color_manual(breaks = c("GPBC", "GPBCphi", "GPGLM", "GLM", "OGP"), 
                     values = c(GPBC = "#D55E00", GPBCphi = "#D55E00",
                                GPGLM = "#D55E00", GLM = "darkgray", OGP = "#503A9B"),
                     labels = c("GPBC", expression(paste("GPBC w/o ", phi)), "GPGLM", "GLM", "OGP"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "dotted", "dashed", "solid", "solid"))),
                     name="") +
  theme(legend.position = c(0.85, 0.19), legend.text=element_text(size=18),text = element_text(size = 20),
        legend.background=element_rect(fill = alpha("white", 0.1))) 
ggsave("PIWidth.pdf")




head(allRes)

PIWidth = allRes %>% group_by(Horizon) %>% summarise(meanBCwidth = mean(BCUpper-BCLower),
                                                     NOBCWidth = mean(HetUpperPI - HetLowerPI))
Obswidth = obs_gp %>% summarise(obsGPwidth = mean(Upper - Lower))
Odf = data.frame(Horizon = 1:30, obsGPwidth = Obswidth$obsGPwidth)

GLMWidth = glm2 %>% group_by(Horizon) %>% summarise(glmwidth = mean(Upper - Lower))
ggplot(data=PIWidth, aes(x = Horizon, y = meanBCwidth, col = "GPBC")) +
  geom_line(linewidth = 1) +
  geom_line(data = PIWidth, aes(x = Horizon, y = NOBCWidth, col = "GPGLM"), linetype = "dashed", linewidth = 1) +
  geom_line(data = Odf, aes(x = Horizon, y = obsGPwidth, col = "OGP"), linewidth = 1) +
  geom_line(data = GLMWidth, aes(x = Horizon, y = glmwidth, col = "GLM"), linewidth = 1) +
  theme_bw() +
  ylab("PI width (°C)") +
  scale_color_manual(breaks = c("GPBC", "GPGLM", "GLM", "OGP"), 
                     values = c(GPBC = "#D55E00", GPGLM = "#D55E00", GLM = "darkgray", OGP = "#503A9B"),
                     labels = c("GPBC",  "GPGLM", "GLM", "OGP"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "dotted", "solid", "solid"))),
                     name="") +
  theme(legend.position = c(0.85, 0.15), legend.text=element_text(size=18),text = element_text(size = 20),
        legend.background=element_rect(fill = alpha("white", 0.1))) 
ggsave("PIWidth.pdf")


plot(PIWidth$Horizon, PIWidth$meanBCwidth, ylim = c(2, 4.5), type="l")
points(PIWidth$Horizon, PIWidth$NOBCWidth, col = "red", type = "l")
par(mfrow = c(1,1))
# obsGP to this

d2 = allRes %>% group_by(DOY) %>% summarise(GPBC = get_rmse(temp_obs, BC_mean), GPGLM=get_rmse(temp_obs, Mean))
dglm = glm2 %>% group_by(DOY) %>% summarise(GLM = get_rmse(temp_obs, GLM_mean))
ob2 = obs_gp %>% group_by(DOY) %>% summarise(OGP = get_rmse(temp_obs, mean))
head(ob2)

ggplot(d2, aes(x = DOY, y = GPBC, col = "GPBC"))+
  geom_line(linewidth = 1) +
  theme_bw() +
  ylab("RMSE") +
  geom_line(data=d2, aes(x = DOY, y = GPGLM, col = "GPGLM"), linetype = "dashed", linewidth = 1) +
  geom_line(data = dglm, aes(x = DOY, y = GLM, col = "GLM"), linewidth = 1) +
  geom_line(data = ob2, aes(x = DOY, y = OGP, col = "OGP"), linewidth = 1) +
  scale_color_manual(breaks = c("GPBC", "GPGLM", "GLM", "OGP"), 
                     values = c(GPBC = "#D55E00", GPGLM = "#D55E00", GLM = "darkgray", OGP = "#503A9B"),
                     labels = c("GPBC",  "GPGLM", "GLM", "OGP"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "dashed", "solid", "solid"))),
                     name="")
ggsave("tempPlotDOY.pdf")

ymd=make_ymd()
d2 = allRes %>% group_by(Depth) %>% summarise(GPBC = get_rmse(temp_obs, BC_mean), GPGLM=get_rmse(temp_obs, Mean))
dglm = glm2 %>% group_by(Depth) %>% summarise(GLM = get_rmse(temp_obs, GLM_mean))
ob2 = obs_gp %>% group_by(Depth) %>% summarise(OGP = get_rmse(temp_obs, mean))
ggplot(d2, aes(x = Depth, y = GPBC, col = "GPBC"))+
  geom_line(linewidth = 1.1) +
  theme_bw() +
  ylab("RMSE") +
  geom_line(data=d2, aes(x = Depth, y = GPGLM, col = "GPGLM"), linetype = "dashed", linewidth = 1.1) +
  geom_line(data = dglm, aes(x = Depth, y = GLM, col = "GLM"), linewidth = 1.1) +
  geom_line(data = ob2, aes(x = Depth, y = OGP, col = "OGP"), linewidth = 1) +
  scale_color_manual(breaks = c("GPBC", "GPGLM", "GLM", "OGP"), 
                     values = c(GPBC = "#D55E00", GPGLM = "#D55E00", GLM = "darkgray", OGP = "#503A9B"),
                     labels = c("GPBC",  "GPGLM", "GLM", "OGP"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "dotted", "solid", "solid"))),
                     name="")
mydf = data.frame(Depth = 0:9, GPBC = d2$GPBC, GPGLM = d2$GPGLM, GLM = dglm$GLM, OGP = ob2$OGP)
xtable(mydf)
ggsave("tempPlotDepth.pdf")

res2 = right_join(allRes, ymd, by = "DOY")
dglm2 = right_join(glm2, ymd, by = "DOY")
d2 = res2 %>% group_by(MONTH) %>% summarise(GPBC = get_rmse(temp_obs, BC_mean), GPGLM=get_rmse(temp_obs, Mean))
dglm = dglm2 %>% group_by(MONTH) %>% summarise(GLM = get_rmse(temp_obs, GLM_mean))
obsgp2 = right_join(obs_gp, ymd, by = "DOY")
ob2 = obs_gp %>% group_by(MONTH) %>% summarise(OGP = get_rmse(temp_obs, mean))

ggplot(d2, aes(x = factor(MONTH), y = GPBC, col = "GPBC", group=1))+
  geom_line(linewidth = 1) +
  theme_bw() +
  ylab("RMSE") +
  geom_line(data=d2, aes(x = factor(MONTH), y = GPGLM, col = "GPGLM", group=1), linetype = "dashed", linewidth = 1) +
  geom_line(data = dglm, aes(x = factor(MONTH), y = GLM, col = "GLM", group=1), linewidth = 1) +
  geom_line(data = ob2, aes(x = factor(MONTH), y = OGP, col = "OGP"), linewidth = 1) +
  scale_color_manual(breaks = c("GPBC", "GPGLM", "GLM", "OGP"), 
                     values = c(GPBC = "#D55E00", GPGLM = "#D55E00", GLM = "darkgray", OGP = "#503A9B"),
                     labels = c("GPBC",  "GPGLM", "GLM", "OGP"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "dotted", "solid", "solid"))),
                     name="")
ggsave("tempPlotMONTH.pdf")

res2 = right_join(allRes, ymd, by = "DOY")
res2$bias = res2$temp_obs - res2$Mean  
res2$applied_bias = res2$BC_mean - res2$Mean
glm2$bias = glm2$temp_obs - glm2$GLM_mean

temp = filter(res2, DOY == 222)

dglm = glm2 %>% group_by(DOY) %>% summarise(meanBias = mean(bias, na.rm=TRUE))

d2 = res2 %>% group_by(DOY) %>% summarise(meanBias = mean(bias, na.rm=TRUE))
d2[d2$DOY == 60, ]$meanBias=1
biasfit = readRDS("..DATA/GPBCPHI/SURROGATES/bias_surrogate.Rds")
head(biasfit)
temp = biasfit %>% group_by(DOY) %>% summarise(meanbias = mean(BiasMean))

y1 = biasfit$start_date
y2 = strsplit(as.character(y1), "-")
y2 = do.call("rbind", y2)
y2 = as.data.frame(y2)
idx=which((y2$V1 == "2022"))
idx2= which(y2$V1 == "2023")
bfit = biasfit[c(idx, idx2), ]

d3 = bfit %>% group_by(DOY) %>% summarise(meanAppliedBias = mean(BiasMean, na.rm=TRUE))


ggplot(d2, aes(x = DOY, y = meanBias, col = "bias")) +
  geom_line(linewidth = 1) +
  theme_bw() +
  geom_vline(xintercept = 48, linetype="dotted", 
             color = "black", linewidth=1) +
  geom_vline(xintercept = 171, linetype="dotted", 
             color = "black", linewidth=1) +
  geom_vline(xintercept = 260, linetype="dotted", 
             color = "black", linewidth=1) +
  geom_line(data = d3, aes(x = DOY, y = meanAppliedBias, col = "appliedbias"), linetype = "dotdash",
            linewidth=1.3) +
  geom_line(data = dglm, aes(x = DOY, y = meanBias, col = "GLMbias"), linewidth = 1) +
  scale_color_manual(breaks = c("bias", "appliedbias", "GLMbias"), 
                     values = c(bias = "#CC79A7", appliedbias = "#009E73", GLMbias = "darkgray"),
                     labels = c("actual bias",  "predicted bias", "GLM bias"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "dotdash", "solid"))),
                     name="") +
  ylab("Mean bias (°C)") +
  theme(legend.position = c(0.85, 0.15), legend.text=element_text(size=18),text = element_text(size = 20),
        legend.background=element_rect(fill = alpha("white", 0.1)))
  ggsave("BiasPlotnew.pdf")
  ggsave("BiasPlot.png")


myseq=seq(150,190)
myseq = c(48, 171, 260)
allRes$bias = allRes$temp_obs - allRes$Mean
i=171
head(allRes)
for (i in myseq){
  myday = filter(allRes, DOY == i, Depth %in% c(0,5))
  ggplot(myday, aes(x = Horizon, y = BC_mean, col = "GPBC"))+
    geom_line() + 
    geom_line(data = myday, aes(x = Horizon, y = Mean, col = "GPGLM")) + 
    geom_line(data = myday, aes(x = Horizon, y = BCLower), col = "#D55E00", linetype = "dashed") +
    geom_line(data = myday, aes(x = Horizon, y = BCUpper), col = "#D55E00", linetype = "dashed") +
    geom_point(data = myday, aes(x = Horizon, y = temp_obs, col = "Obs")) +
    facet_wrap(~Depth, nrow=2) +
    theme_bw() +
    xlab("Horizon (days)") +
    ylab("Temperature (°C)") +
    scale_color_manual(breaks = c("GPBC", "GPGLM", "Obs"), 
                       values = c(GPBC = "#D55E00", GPGLM = "#CC79A7", Obs = "#503A9B"),
                       labels = c("GPBC",  "GPGLM", "Obs"),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "solid", "blank"),
                         shape = c(NA,NA, 16)),
                       name="")) +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.78, 0.35), legend.text=element_text(size=16),text = element_text(size = 16),
          legend.background=element_rect(fill = alpha("white", 0.1)), strip.text.x = element_text(size = 16)) +
    ggtitle(paste("DOY", i))
  ggsave(paste0("DOY", i, ".pdf"))
}
d_names <- as_labeller(
  c(`0` = "Surface", `5` = "5m"))
for (i in myseq){
  myday = filter(allRes, DOY == i, Depth %in% c(0,5))
  if (i == 48){
  ggplot(myday, aes(x = Horizon, y = BC_mean, col = "GPBC"))+
    geom_line(linewidth=1) + 
    geom_line(data = myday, aes(x = Horizon, y = Mean, col = "GPGLM"), linetype="dashed", linewidth=1) + 
    geom_point(data = myday, aes(x = Horizon, y = temp_obs, col = "Obs")) +
    facet_wrap(~Depth, nrow=2, labeller = d_names) +
    theme_bw() +
    xlab("Horizon (days)") +
    ylab("Temperature (°C)") +
    scale_color_manual(breaks = c("GPBC", "GPGLM", "Obs"), 
                       values = c(GPBC = "#D55E00", GPGLM = "#D55E00", Obs = "#503A9B"),
                       labels = c("GPBC",  "GPGLM", "Obs"),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "dotted", "blank"),
                         shape = c(NA,NA, 16)),
                         name="")) +
    theme(legend.title=element_blank()) +
    theme(legend.position = c(.22, 0.4), legend.text=element_text(size=14),text = element_text(size = 14),
          legend.background=element_rect(fill = alpha("white", 0.1)), strip.text.x = element_text(size = 16),
          axis.text=element_text(size=14)) +
    annotate(geom="text", x=8, y=.45, label=paste("DOY", i)) 
  ggsave(paste0("DOY", i, ".pdf"), height = 2000, width = 1000, units="px")
  }else{
    ggplot(myday, aes(x = Horizon, y = BC_mean, col = "GPBC"))+
      geom_line(linewidth=1) + 
      geom_line(data = myday, aes(x = Horizon, y = Mean, col = "GPGLM"), linetype="dashed", linewidth=1) + 
      geom_point(data = myday, aes(x = Horizon, y = temp_obs, col = "Obs")) +
      facet_wrap(~Depth, nrow=2, labeller = d_names) +
      theme_bw() +
      xlab("Horizon (days)") +
      ylab("Temperature (°C)") +
      scale_color_manual(breaks = c("GPBC", "GPGLM", "Obs"), 
                         values = c(GPBC = "#D55E00", GPGLM = "#D55E00", Obs = "#503A9B"),
                         labels = c("GPBC",  "GPGLM", "Obs"),
                         guide = guide_legend(override.aes = list(
                           linetype = c("solid", "dotted", "blank"),
                           shape = c(NA,NA, 16)),
                           name="")) +
      annotate(geom="text", x=8, y=.45, label=paste("DOY", i)) +
      theme(legend.position = "none", text = element_text(size = 14), strip.text.x = element_text(size = 16),
            axis.text=element_text(size=14)) 
    ggsave(paste0("DOY", i, ".pdf"), height = 2000, width = 1000, units="px")
  }
}


mydf = glm2 %>% group_by(Horizon) %>% summarise(meanBias = mean(bias, na.rm=TRUE))

df2 = res2 %>% group_by(Horizon) %>% summarise(meanBias = mean(bias, na.rm=TRUE),
                                          meanAppliedBias = mean(applied_bias, na.rm = TRUE))

plot(mydf$meanBias)
head(glm2)

library(xtable)
xtable(d2)
head(d2)
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
head(allRes)

obs_sub = filter(obs_data, DOY == mydoy, Depth == 1)
head(obs_sub)
bias_sub = filter(biasfit, DOY == mydoy, Depth == 1)
head(bias_sub2)

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

get_rmse(obs_sub$temp_obs, pred_sub$Mean)
get_rmse(obs_sub$temp_obs, pred_sub$BC_mean)

###################################################################################################
### version WITH OGP
###################################################################################################
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

###################################################################################################
### version WITH NO OGP
###################################################################################################
plotgpglm =  ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = 0.7) +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = Mean, col = "GPGLM"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetLowerPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetUpperPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations" ))+
  ylim(c(1,13)) + 
  theme_bw() +
  scale_color_manual(breaks = c("GPGLM", "Observations", "GLM"), 
                     values = c(GPGLM = "#D55E00", Observations = "#503A9B",
                                GLM = "darkgray"),
                     labels = c("GPGLM", "Observations", "GLM"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "blank", "solid"),
                       shape = c(NA, 16, NA))),
                     name="") +
  theme(legend.position = c(0.3, 0.92), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1))) +
  labs(tag = "A") 

plotgpbc = ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, group = ensemble_no)) +
  geom_line(alpha = 0.7, col = "darkgray") +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = BC_mean, col = "GPBC"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#E66100") +
  geom_line(data=pred_sub, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#E66100") +
  
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
  theme(legend.position = c(0.19, 0.95), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1))) 
#facet_wrap(~factor(start_date))
#
ggarrange(plotgpglm, plotgpbc, common.legend = FALSE)
#ggsave("BCnoBCTestperiodExample2.pdf")

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


