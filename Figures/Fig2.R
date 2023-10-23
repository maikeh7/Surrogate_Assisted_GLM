# Make Figure 2
library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr)

biglist = list(length=30)
horizon_dir = "../Data/HORIZON_TRAIN"
method = "Average"
lookback = 4
obs_depth = 1

# read in (processed) GLM forecast data
for (i in 1:30){
  temp = data.table::fread(file.path(horizon_dir, paste0("BigDF_H", i, "_TRAIN", method, lookback,
                                                         "D", obs_depth, ".csv")))
  biglist[[i]] = temp
}

bigdf = data.table::rbindlist(biglist)
bigdf = as.data.frame(bigdf)
colnames(bigdf)[which(colnames(bigdf) == "depth_int")] = "Depth"
colnames(bigdf)[which(colnames(bigdf) == "horizon")] = "Horizon"

# names for facet labels
d_names <- as_labeller(
  c(`0` = "Surface", `1` = "1m",`2` = "2m", 
    `3` = "3m",`4` = "4m", `5` = "5m",
    `6` = "6m", `7`= "7m", `8` = "8m", `9` = "9m"))

smalldf = filter(bigdf, start_date == "2021-01-01")
smalldf$ensemble_no = rep(1:31, 300)
p1=ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = factor(ensemble_no))) +
  geom_line() +
  scale_color_manual(values = mycols) +
  theme_bw() +
  theme(legend.position="none") +
  ylab("Temperature (°C)") +
  xlab("Horizon (days)") +
  ylim(c(0,15)) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~Depth, nrow=2, ncol=5, labeller = d_names) 

smalldf = filter(bigdf, start_date == "2020-10-30")
smalldf$ensemble_no = rep(1:31, 300)
p2=ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = factor(ensemble_no))) +
  geom_line() +
  scale_color_manual(values = mycols) +
  theme_bw() +
  theme(legend.position="none") +
  ylab("Temperature (°C)") +
  xlab("Horizon (days)") +
  ylim(c(0,15)) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~Depth, nrow=2, ncol=5, labeller = d_names) 


ggarrange(p2, p1, nrow = 2, ncol=1)
#ggsave(file.path(plot_dir, "HetPlots.png"))