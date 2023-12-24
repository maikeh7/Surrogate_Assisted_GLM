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

preds = readRDS("../Data/SURROGATES/preds_all_training.Rds")
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
  geom_line(data=pred_sub, aes(x = Horizon, y = HetLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations" ))+
  
  geom_point(data=obs_sub, aes(x = Horizon, y = bias, col = "bias")) +
  
  geom_line(data=bias_sub, aes(x = Horizon, y = Mean, col = "biasGP"), linewidth = 1.1) +
  geom_line(data=bias_sub, aes(x = Horizon, y = Lower), linetype = "dashed", linewidth = 1.1, col = "#009E73") +
  geom_line(data=bias_sub, aes(x = Horizon, y = Upper), linetype = "dashed", linewidth = 1.1, col = "#009E73") +
  
  theme_bw() +
  scale_color_manual(breaks = c("GPGLM", "biasGP", "Observations", "bias", "GLM"), 
                     values = c(GPGLM = "#D55E00",
                                biasGP = "#009E73", Observations = "#503A9B",
                                bias = "#CC79A7", GLM = "darkgray"),
                     labels = c("GPGLM",  "biasGP", "Observations", "bias", "GLM"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "blank", "blank", "solid"),
                       shape = c(NA, NA, 16, 16, NA))),
                     name="") +
  theme(legend.position = c(0.11, 0.86), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)))+
  facet_wrap(~factor(start_date)) +
  labs(tag = "A")

# WITH OGP (do not use)
plot1 = ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = Mean, col = "GPGLM"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations" ))+
  
  geom_point(data=obs_sub, aes(x = Horizon, y = bias, col = "bias")) +
  
  geom_line(data=bias_sub, aes(x = Horizon, y = Mean, col = "biasGP"), linewidth = 1.1) +
  geom_line(data=bias_sub, aes(x = Horizon, y = Lower), linetype = "dashed", linewidth = 1.1, col = "#009E73") +
  geom_line(data=bias_sub, aes(x = Horizon, y = Upper), linetype = "dashed", linewidth = 1.1, col = "#009E73") +
  
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_mean, col = "OGP"), linewidth = 1.1) +
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_Lower), linetype = "dashed", linewidth = 1.1, col = "#999933") +
  geom_line(data=bias_sub2, aes(x = Horizon, y = ObsGP_Upper), linetype = "dashed", linewidth = 1.1, col = "#999933") +
  
  theme_bw() +
  scale_color_manual(breaks = c("GPGLM", "OGP", "biasGP", "Observations", "bias", "GLM"), 
                     values = c(GPGLM = "#D55E00", OGP = "#999933",
                                biasGP = "#009E73", Observations = "#503A9B",
                                bias = "#CC79A7", GLM = "darkgray"),
                     labels = c("GPGLM",  "OGP", "biasGP", "Observations", "bias", "GLM"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "solid", "solid", "blank", "blank", "blank"),
                       shape = c(NA, NA, NA, 16, 16, 16))),
                     name="") +
  theme(legend.position = c(0.11, 0.86), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1)))+
  facet_wrap(~factor(start_date)) +
  labs(tag = "A")
#ggsave(file.path(plot_dir, paste0("GPGLM", mydoy, ".png")))


plot2 = ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
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
  theme(legend.position = c(0.10, 0.92), legend.text=element_text(size=14),
        legend.background=element_rect(fill = alpha("white", 0.1))) +
  facet_wrap(~factor(start_date)) +
  labs(tag = "B")

ggarrange(plot1, plot2, nrow = 2, ncol=1)
ggsave("BCnoBCtrainperiodNOOGP.png", width = 2400, height= 3200, units = "px")

###########################################################################################
# Fit 2 NOAA plot
###########################################################################################
preds = readRDS("../Data/SURROGATES/preds_all_training.Rds")
obs_data = read.csv("../Data/SURROGATES/Bias_dataset_validation.csv")

mydoy=50
smalldf = filter(bigdf, DOY == mydoy, Depth ==1)
smalldf$ensemble_no = rep(1:31, 90)
smalldf = filter(smalldf, start_date == "2022-02-19")

pred_sub = filter(preds, DOY == mydoy, Depth == 1)

obs_sub = filter(obs_data, DOY == mydoy, Depth == 1)

obs_sub$ensemble_no = 1
pred_sub$ensemble_no = 1

obs_sub = filter(obs_sub, start_date == "2022-02-19")
pred_sub = filter(pred_sub, start_date == "2022-02-19")


ggplot(smalldf, aes(x = Horizon, y = Temp_C_00UTC, col = "GLM", group = ensemble_no)) +
  geom_line(alpha = .7) +
  
  geom_line(data=pred_sub, aes(x = Horizon, y = Mean, col = "GPGLM"), linewidth = 1.1) +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetLower), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  geom_line(data=pred_sub, aes(x = Horizon, y = HetUpper), linetype = "dashed", linewidth = 1.1, col = "#D55E00") +
  #facet_wrap(~factor(start_date)) + 
  ylab("Temp (°C)") +
  xlab("Horizon (days)") +
  geom_point(data=obs_sub, aes(x = Horizon, y = temp_obs, col = "Observations" ))+

  theme_bw() +
  scale_color_manual(breaks = c("GPGLM", "Observations", "GLM"), 
                     values = c(GPGLM = "#D55E00",
                                Observations = "#503A9B",
                                GLM = "darkgray"),
                     labels = c("GLM surrogate", "Observations", "GLM"),  
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid", "blank", "solid"),
                       shape = c(NA, 16, NA))),
                     name="") +
  theme(legend.position = c(0.13, 0.90), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1)),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18))
ggsave("Fit2NOAA-GLM.png", width = 3000, height= 2000, units = "px")
#######################################################################################
# 3D plots
#######################################################################################
preds = readRDS("../Data/SURROGATES/preds_all_training.Rds")
obs_data = read.csv("../Data/SURROGATES/Bias_dataset_validation.csv")

mydoy=50

pred_sub = filter(preds, DOY == mydoy)

obs_sub = filter(obs_data, DOY == mydoy)

obs_sub = filter(obs_sub, start_date == "2022-02-19")
pred_sub = filter(pred_sub, start_date == "2022-02-19")
head(pred_sub)

df2 = pred_sub
preds=df2$Mean
predsVar = df2$Var_SK

depth = rep(0:9, each = 30)
mydepths = 0:9

df_mean =data.frame(dummy=rep(0,30))
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = preds[idx]
  df_mean=cbind(df_mean, temps)
}

df_mean$dummy=NULL
colnames(df_mean) = paste("d", seq(0:9), sep = "")

depth = rep(0:9, each = 30)
mydepths = 0:9

df_var =data.frame(dummy=rep(0,30))
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = predsVar[idx]
  df_var=cbind(df_var, temps)
}

df_var$dummy=NULL
colnames(df_var) = paste("d", seq(0:9), sep = "")


library(viridis)
library(rgl)

map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

max(df_var)
meancol= map2color(as.matrix(df_mean), viridis(100))
varcol= map2color(as.matrix(df_var), viridis(100), limits = c(0.001, 5))

par3d(cex=2.0)
persp3d(x = 1:30, y = 0:9, as.matrix(df_mean), color = meancol, xlab = "Horizon", ylab = "Depth", 
        zlab = "Mean (C)", alpha=0.95)
rgl.snapshot('3dplotMean.png', fmt = 'png')

par3d(cex=2.0)
persp3d(x = 1:30, y = 0:9, as.matrix(df_var), color = varcol, xlab = "Horizon", ylab = "Depth", 
        zlab = "Var (C)", alpha=0.95)
rgl.snapshot('3dplotVar.png', fmt = 'png')


head(df)
df2 = filter(df, Temp_covar == 25.63134)
x = df2$Depth
y = df2$Horizon
z = df2$BC_mean
z2 = df2$SD_SK

range(preds)
preds=df2$Mean
mysd = df2$SD_SK

depth = rep(0:9, each = 30)
mydepths = 0:9

df=data.frame(dummy=rep(0,30))
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = preds[idx]
  df=cbind(df, temps)
}
head(df)
df$dummy=NULL
colnames(df) = paste("d", seq(0:9), sep = "")