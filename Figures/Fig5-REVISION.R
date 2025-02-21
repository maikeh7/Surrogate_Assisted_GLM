# Make Figure 5
library(ggplot2)
library(dplyr)
library(ggpubr)
library(data.table)

#plot_dir = "C:/Users/Maike/Box Sync/DEEP_LEARNING/SurrogateModeling/manuscriptPlots"
biglist = list(length=30)
horizon_dir = "../Data/HORIZON_TRAIN"
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

#preds = readRDS("../Data/SURROGATES/preds_all_training3.1.2024.Rds")
preds = readRDS("../Data/SURROGATES_NEW/preds_all_training.Rds")

#preds$BC_sd = (preds$SD_SK / sqrt(30)) + preds$bias_SD
#preds$BCLower = qnorm(0.05, preds$BC_mean, preds$BC_sd)
#preds$BCUpper <- qnorm(0.95, preds$BC_mean, preds$BC_sd)

obs_data = read.csv("../Data/SURROGATES/Bias_dataset_validation.csv")
biasfit = readRDS("../Data/SURROGATES/bias_surrogate.Rds")
ogp = read.csv("../Data/Observed_hetGP_withOb_TRAINONLY.csv")

mydoy=50
smalldf = filter(bigdf, DOY == mydoy, Depth ==1)
smalldf$ensemble_no = rep(1:31, 90)

bias_sub = filter(biasfit, DOY == mydoy, Depth == 1)
pred_sub = filter(preds, DOY == mydoy, Depth == 1)

obs_sub = filter(obs_data, DOY == mydoy, Depth == 1)

# merge w/ observed GP (this is observed gp trained on ALL training data, just as all of these other preds)
ogp = filter(ogp, Depth == 1)
ogp = dplyr::select(ogp, mean, Lower, Upper, date)

colnames(ogp) = c("ObsGP_mean", "ObsGP_Lower", "ObsGP_Upper", "date")
ogp$date = as.character(ogp$date)


bias_sub$s_date = as.POSIXct(bias_sub$start_date)
bias_sub$date = bias_sub$s_date + lubridate::days(bias_sub$Horizon)
bias_sub$date = as.character(bias_sub$date)
bias_sub2 = right_join(bias_sub, ogp, by = "date")
#bias_sub2 = bias_sub2[complete.cases(bias_sub2), ]
obs_sub$ensemble_no = 1
pred_sub$ensemble_no = 1
bias_sub$ensemble_no = 1
bias_sub2$ensemble_no = 1

# NO OGP
plot1 = ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = Mean, col = "GPGLM"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetLowerPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetUpperPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations" ))+

  
  theme_bw() +
  scale_color_manual(breaks = c("GPGLM", "Observations", "GLM"), 
                     values = c(GPGLM = "#D55E00", Observations = "#503A9B", GLM = "darkgray"),
                     labels = c("GPGLM",  "Observations", "GLM"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "blank",  "solid"),
                       shape = c(NA, 16, NA))),
                     name="") +
  theme(legend.position = c(0.11, 0.86), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        strip.text.x = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title=element_text(size=14))+
  facet_wrap(~factor(start_date)) +
  labs(tag = "A")
ggsave("BCnoBCtrainperiodNOOGPA.pdf", width = 2400, height= 1000, units = "px")

plot2 = ggplot(obs_sub, aes(x = Horizon, y = bias, col = "bias")) +
  geom_point() +
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_line(data=bias_sub, aes(x = Horizon, y = Mean, col = "biasGP"),linewidth = .9) +
  geom_line(data=bias_sub, aes(x = Horizon, y = Lower), linetype = "dashed", linewidth = 1.1, col = "#009E73") +
  geom_line(data=bias_sub, aes(x = Horizon, y = Upper), linetype = "dashed", linewidth = 1.1, col = "#009E73") +
  
  theme_bw() +
  scale_color_manual(breaks = c("biasGP", "bias"),
                     values = c(biasGP = "#009E73",
                                bias = "#CC79A7"),
                     labels = c("biasGP", "bias"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "blank"),
                       shape = c(NA, 16))),
                     name="") +

  theme(legend.position = c(0.11, 0.86), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        strip.text.x = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title=element_text(size=14))+
  facet_wrap(~factor(start_date)) +
  labs(tag = "B")
ggsave("BCnoBCtrainperiodNOOGPB.pdf", width = 2400, height= 1000, units = "px")

plot3 = ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) + 
  
  geom_line(data=pred_sub, aes(x = Horizon, y = BC_mean, col = "GPBC"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#E66100") +
  geom_line(data=pred_sub, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#E66100") +
  
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  
  theme_bw() +
  scale_color_manual(breaks = c("GPBC", "Observations", "GLM"), 
                     values = c(Observations = "#503A9B", GLM = "darkgray", GPBC = "#E66100"),
                     labels = c("GPBC", "Observations", "GLM"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "blank", "solid"),
                       shape = c(NA, 16, NA))),
                     name = "") +
  theme(legend.position = c(0.13, 0.84), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        strip.text.x = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title=element_text(size=14)) +
  facet_wrap(~factor(start_date)) +
  labs(tag = "C")
ggsave("BCnoBCtrainperiodNOOGPC.pdf", width = 2400, height= 1000, units = "px")
ggarrange(plot1, plot2, plot3, nrow = 3, ncol=1)
ggsave("BCnoBCtrainperiodNOOGP.pdf", width = 2400, height= 3400, units = "px")
