# make Figure 1 (Right) GLM bias/surrogate schematic
library(ggplot2)
library(dplyr)

library(rgl)
library(viridis)

# grab bias data for a HetGP fitted to 2021 GLM sims only
biasdat = read.csv("../Data/bias_dat_forHetGP.csv")
biasdat$X.1=NULL
biasdat$X = NULL
bdat = filter(biasdat, YEAR == 2021)

# function to make MONTH/DAY/DOY dataframe for merging w/ other datasets
make_ymd = function(){
  month_table = data.frame(MONTH = 1:12, freq = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  MONTH = rep(1:12, month_table$freq)
  DAY = unlist(Map(function(start_num, stop_num) seq(start_num,stop_num ),1,month_table$freq ))
  DOY = 1:366
  ymd = data.frame(MONTH = MONTH, DAY = DAY, DOY = DOY)
}
ymd = make_ymd()

# preds from a HetGP fitted to 2021 GLM sims
preds = readRDS("../Data/Surrogate_2021OnlyPreds.Rds")

pred_times = cbind(rep(1:366, 10), rep(0:9, each=366))
mean_trend = preds$mean
temp_sd = sqrt(preds$sd2 + preds$nugs)
sdnonug = sqrt(preds$sd2)
hetDF = data.frame(cbind(pred_times, mean_trend, temp_sd, sdnonug))
colnames(hetDF) = c("DOY", "depth_int", "Mean", "SD", "SD_nonug")

hetDF$Lower = qnorm(0.05, hetDF$Mean, hetDF$SD)
hetDF$Upper = qnorm(0.95, hetDF$Mean, hetDF$SD)

# read in bias surrogate for 2021 (fit to observed - surrogate)
bias_2021 = readRDS("../Data/Surrogate_bias_2021_Preds.Rds")

pred_times = cbind(rep(1:366, 10), rep(0:9, each=366))
mean_trend = bias_2021$mean
temp_sd = sqrt(bias_2021$sd2 + bias_2021$nugs)
BiasDF = data.frame(cbind(pred_times, mean_trend, temp_sd))
head(BiasDF)
colnames(BiasDF) = c("DOY", "depth_int", "Mean", "SD")

# construct BC GP surrogate
BiasDF$Lower = qnorm(0.05, BiasDF$Mean, BiasDF$SD)
BiasDF$Upper = qnorm(0.95, BiasDF$Mean, BiasDF$SD)
hetDF$BC_mean = hetDF$Mean + BiasDF$Mean

# add variances--then take square root, as variances, nugget would be on the diagonal of 
# Sigma(surroate) + Sigma(bias)
hetDF$BC_sd = sqrt(preds$sd2 + bias_2021$sd2 + bias_2021$nugs)
hetDF$BC_Lower = qnorm(0.05, hetDF$BC_mean, hetDF$BC_sd)
hetDF$BC_Upper = qnorm(0.95, hetDF$BC_mean, hetDF$BC_sd)

head(BiasDF)
bdat$Date = paste(bdat$YEAR, bdat$MONTH, bdat$DAY, sep = "-")
bdat$Date = as.POSIXct(bdat$Date, tz = "UTC")
hetDF = right_join(hetDF, ymd, by = "DOY")
hetDF$YEAR = 2021
hetDF$Date = paste(hetDF$YEAR, hetDF$MONTH, hetDF$DAY, sep = "-")
hetDF$Date = as.POSIXct(as.Date(hetDF$Date), tz = "UTC")


mycols =  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot_cols = mycols[c(1,2 ,6,8)]

ggplot(data = filter(bdat, depth_int == 6), aes(x = Date, y = temp_glm, col = "GLM")) +
  geom_line(linewidth = 1.2) +
  geom_line(data = filter(bdat, depth_int==6), aes(x = Date, y = temp_obs, col = "Observed"), 
            linewidth = 1.2) +
  geom_line(data= filter(hetDF, depth_int == 6), aes(x = Date, y = Lower), col = plot_cols[3],
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data= filter(hetDF, depth_int == 6), aes(x = Date, y = Upper), col = plot_cols[3], 
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data= filter(hetDF, depth_int == 6), aes(x = Date, y = Mean, col = "Surrogate"),
            linewidth = 1) +
  geom_line(data= filter(hetDF, depth_int == 6), aes(x = Date, y = BC_mean, col = "BC_Surrogate"), 
            linewidth = 1) +
  geom_line(data= filter(hetDF, depth_int == 6), aes(x = Date, y = BC_Lower), col = plot_cols[4], 
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data= filter(hetDF, depth_int == 6), aes(x = Date, y = BC_Upper), col = plot_cols[4],
            linewidth = 0.8, linetype = "dashed") +
  theme_bw() +
  annotate("text", x=as.POSIXct("2021-08-13"), y=12, label= "Bias",
           col="black", size=10) +
  theme(text = element_text(size = 20, color = "black"),
        axis.text = element_text(size = 14, color = "black"))  +
  
  scale_color_manual(breaks = c("BC_Surrogate", "Surrogate", "Observed", "GLM"), 
                     values = c(Observed = plot_cols[1], GLM = plot_cols[2], Surrogate = plot_cols[3],
                                BC_Surrogate = plot_cols[4]),
                     labels = c("Bias-corrected surrogate", "Surrogate", "Observed", "GLM"), 
                     guide = guide_legend(override.aes = list(
                       linewidth = c(1, 1, 1.3, 1.3))),
                     name = "") +
  theme(legend.position = c(0.24, 0.89), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1))) +
  ylab("Water temperature C") 
#ggsave("PlotForCayelanannotate.pdf", height = 1800, width = 2400, units="px")
ggsave("PlotForCayelanannotate.png", height = 1800, width = 2400, units="px")

#breaks = c("BC_Surrogate", "Surrogate", "Observed", "GLM")
#labels = c("BC_Surrogate", "Surrogate", "Observed", "GLM")

# Stuff for 2D figure that I will add
ggplot(data = filter(BiasDF, depth_int == 6), aes(x = DOY, y = Mean)) +
  geom_line(linewidth = 1, col = "#009E73") +
  geom_line(data= filter(BiasDF, depth_int == 6), aes(x = DOY, y = Lower), col = "#009E73",
            linewidth = 0.8, linetype = "dashed") +
  geom_line(data= filter(BiasDF, depth_int == 6), aes(x = DOY, y = Upper), col = "#009E73", 
            linewidth = 0.8, linetype = "dashed") +
  theme_bw() +
  theme(text = element_text(size = 26, color = "black"),
        axis.text = element_text(size = 26, color = "black"))  +
  ylab("Bias C") 
ggsave("Bias1D.png", height = 1800, width = 2400, units="px")

## Make figure 5 (2D surface)

df2 = hetDF
df3 = BiasDF

x = df2$depth_int
y = df2$DOY
z = df2$BC_mean
z2 = df2$SD_SK

head(df2)
range(preds)
preds=df2$Mean
predsbias = df3$Mean
upperPI = df3$Upper
lowerPI = df3$Lower

depth = rep(0:9, each = 366)
mydepths = 0:9

df_mean =data.frame(dummy=rep(0,366))
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = preds[idx]
  df_mean=cbind(df_mean, temps)
}
head(df_mean)
df_mean$dummy=NULL
colnames(df_mean) = paste("d", seq(0:9), sep = "")


df_biasmean =data.frame(dummy=rep(0,366))
depth = rep(0:9, each = 366)
mydepths = 0:9
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = predsbias[idx]
  df_biasmean=cbind(df_biasmean, temps)
}
head(df_biasmean)
df_biasmean$dummy=NULL
colnames(df_biasmean) = paste("d", seq(0:9), sep = "")


df_upper =data.frame(dummy=rep(0,366))
depth = rep(0:9, each = 366)
mydepths = 0:9
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = upperPI[idx]
  df_upper=cbind(df_upper, temps)
}
head(df_upper)
head(df_mean)
df_upper$dummy=NULL
colnames(df_upper) = paste("d", seq(0:9), sep = "")

df_lower =data.frame(dummy=rep(0,366))
depth = rep(0:9, each = 366)
mydepths = 0:9
for (i in 1:length(mydepths)){
  idx = which(depth == mydepths[i])
  temps = lowerPI[idx]
  df_lower=cbind(df_lower, temps)
}
head(df_lower)
df_lower$dummy=NULL
colnames(df_lower) = paste("d", seq(0:9), sep = "")

map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

biascol= map2color(as.matrix(df_biasmean), viridis(100), limits = c(-6,3))
meancol= map2color(as.matrix(df_mean), viridis(100), limits = c(12,25))

par3d(cex=2.0)
persp3d(x = 1:366, y = 0:9, as.matrix(df_mean), color = meancol, xlab = "DOY", ylab = "Depth", 
        zlab = "Mean (C)", alpha=0.95)
rgl.snapshot('3dplotMean.png', fmt = 'png')

par3d(cex=2.0)
persp3d(x = 1:366, y = 0:9, as.matrix(df_biasmean), color = biascol, xlab = "DOY", ylab = "Depth", 
        zlab = "Bias(C)", alpha = 0.95)
persp3d(x = 1:366, y = 0:9, as.matrix(df_upper), color = "red", add = TRUE, alpha = .5)
persp3d(x = 1:366, y = 0:9, as.matrix(df_lower), color = "red", add = TRUE, alpha =.5)
rgl.snapshot('3dplotBiasNOPI.png', fmt = 'png')
rgl.snapshot('3dSD.png', fmt = 'png')

