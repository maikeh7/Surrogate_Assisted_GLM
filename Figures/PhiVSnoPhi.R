# Make Figure 5
library(ggplot2)
library(dplyr)
library(ggpubr)
library(data.table)

phipreds = readRDS("GPBCPHI/preds_all_training.Rds")
trainpreds = readRDS("GPBC-train/preds_all_training.Rds")
obs_data = read.csv("GPBCPHI/Bias_dataset_validation.csv")
obs_data = obs_data %>% dplyr::select(DOY, Depth, Horizon, start_date, temp_obs)

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

mydoy = 61
smalldf = filter(bigdf, DOY == mydoy, Depth ==1)
smalldf$ensemble_no = rep(1:31, 90)


smalldf1 = filter(smalldf, start_date == "2021-03-01")
smalldf2 = filter(smalldf, start_date == "2022-03-01")
smalldf3 = filter(smalldf, start_date == "2023-03-01")

obs = filter(obs_data, DOY == mydoy, Depth == 1, start_date == "2023-03-01")

phi1 = filter(phipreds, DOY == mydoy, Depth == 1, start_date == "2023-03-01")
phi1 = right_join(phi1, obs, by = c("DOY","Depth",  "Horizon", "start_date"))
pred1 = filter(trainpreds, DOY == mydoy, Depth == 1, start_date == "2023-03-01")
pred1 = right_join(pred1, obs, by = c("DOY","Depth",  "Horizon", "start_date"))
phi1$ensemble_no = 1
pred1$ensemble_no = 1

plot1=ggplot(smalldf3, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  geom_line(data = smalldf1, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data = smalldf2, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data=phi1, aes(x = Horizon, y = BC_mean, col = "without"),linetype = "dotted", linewidth = 1.1) +
  geom_line(data=phi1, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=phi1, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_point(data = phi1, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  theme_bw() +
  scale_color_manual(breaks = c("without", "GLM", "Observations"), 
                     values = c(without = "#D55E00",
                                GLM = "gray21",
                                Observations = "#503A9B"),
                     labels = c(expression(paste("without ", phi)),  "GLM", "Observations"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("dotted", "solid", "blank"),
                       shape = c(NA, NA, 16))),
                     name="") +
  ggtitle("Bias-corrected") +
  theme(legend.position = c(0.20, 0.86), legend.text=element_text(size=14),text = element_text(size = 14),
        legend.background=element_rect(fill = alpha("white", 0.1)))



plot2=ggplot(smalldf3, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  geom_line(data = smalldf1, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data = smalldf2, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data=pred1, aes(x = Horizon, y = BC_mean, col = "with"), linewidth = 1.1) +
  geom_line(data=pred1, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred1, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_point(data = pred1, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  theme_bw() +
  scale_color_manual(breaks = c("with", "GLM", "Observations"), 
                     values = c(with = "#D55E00",
                                GLM = "gray21",
                                Observations = "#503A9B"),
                     labels = c(expression(paste("with ", phi)),  "GLM", "Observations"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "blank"),
                       shape = c(NA, NA, 16))),
                     name="") +
  ggtitle("Bias-corrected") +
  theme(legend.position = c(0.20, 0.86), legend.text=element_text(size=14),text = element_text(size = 14),
        legend.background=element_rect(fill = alpha("white", 0.1)))
ggarrange(plot1, plot2, nrow = 2, ncol=1)
ggsave("CORRECTED.pdf", width = 2000, height= 2800, units = "px")
####################################################################################################################

plot3=ggplot(smalldf3, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  geom_line(data = smalldf1, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data = smalldf2, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data=phi1, aes(x = Horizon, y = Mean, col = "without"), linetype = "dotted", linewidth = 1.1) +
  geom_line(data=phi1, aes(x = Horizon, y = HetLowerPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=phi1, aes(x = Horizon, y = HetUpperPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_point(data = phi1, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  theme_bw() +
  ylim(c(2,17)) +
  scale_color_manual(breaks = c("without", "GLM", "Observations"), 
                     values = c(without = "#D55E00",
                                GLM = "gray21",
                                Observations = "#503A9B"),
                     labels = c(expression(paste("without ", phi)),  "GLM", "Observations"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("dotted", "solid", "blank"),
                       shape = c(NA, NA, 16))),
                     name="") +
  ggtitle("Not bias-corrected") +
  theme(legend.position = c(0.20, 0.9), 
        legend.text=element_text(size=14),
        text = element_text(size = 14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



plot4=ggplot(smalldf3, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .9) +
  geom_line(data = smalldf1, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data = smalldf2, aes(x = Horizon, y = Temp_C_00UTC), col = "lightgray", alpha = .55) +
  geom_line(data=pred1, aes(x = Horizon, y = Mean, col = "with"), linewidth = 1.1) +
  geom_line(data=pred1, aes(x = Horizon, y = HetLowerPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred1, aes(x = Horizon, y = HetUpperPI), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_point(data = pred1, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  theme_bw() +
  ylim(c(2,17)) +
  scale_color_manual(breaks = c("with", "GLM", "Observations"), 
                     values = c(with = "#D55E00",
                                GLM = "gray21",
                                Observations = "#503A9B"),
                     labels = c(expression(paste("with ", phi)),  "GLM", "Observations"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "blank"),
                       shape = c(NA, NA, 16))),
                     name="") +
  ggtitle("Not bias-corrected") +
  theme(legend.position = c(0.20, 0.92), 
        legend.text=element_text(size=14),
        text = element_text(size = 14),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggarrange(plot1, plot2, nrow = 2, ncol=1)
ggarrange(plot3, plot4, plot1, plot2, nrow=2, ncol=2)
ggsave("PhivsNophi.pdf", width = 3000, height= 3000, units = "px")
ggsave("NOCORRECTION.pdf", width = 2000, height= 2800, units = "px")


#####################################################################################################################

plot1=ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  geom_line(data=phi1, aes(x = Horizon, y = BC_mean, col = "GPBCphi"), linewidth = 1.1) +
  geom_line(data=phi1, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=phi1, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_point(data = phi1, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  theme_bw() +
  scale_color_manual(breaks = c("GPBCphi", "GLM", "Observations"), 
                     values = c(GPBCphi = "#D55E00",
                                GLM = "darkgray",
                                Observations = "#503A9B"),
                     labels = c("GPBCphi",  "GLM", "Observations"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "blank"),
                       shape = c(NA, NA, 16))),
                     name="") +
  theme(legend.position = c(0.15, 0.86), legend.text=element_text(size=14),text = element_text(size = 14),
        legend.background=element_rect(fill = alpha("white", 0.1)))+
  facet_wrap(~factor(start_date)) +
  labs(tag = "A")



plot2=ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  geom_line(data=pred1, aes(x = Horizon, y = BC_mean, col = "GPBC"), linewidth = 1.1) +
  geom_line(data=pred1, aes(x = Horizon, y = BCLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred1, aes(x = Horizon, y = BCUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_point(data = pred1, aes(x = Horizon, y = temp_obs, col = "Observations")) +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  theme_bw() +
  scale_color_manual(breaks = c("GPBC", "GLM", "Observations"), 
                     values = c(GPBC = "#D55E00",
                                GLM = "darkgray",
                                Observations = "#503A9B"),
                     labels = c("GPBC",  "GLM", "Observations"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "blank"),
                       shape = c(NA, NA, 16))),
                     name="") +
  theme(legend.position = c(0.15, 0.86), legend.text=element_text(size=14),text = element_text(size = 14),
        legend.background=element_rect(fill = alpha("white", 0.1)))+
  facet_wrap(~factor(start_date)) +
  labs(tag = "B")
ggarrange(plot1, plot2, nrow = 2, ncol=1)
ggsave("GPBCphivsGPBC.pdf", width = 2000, height= 2800, units = "px")
