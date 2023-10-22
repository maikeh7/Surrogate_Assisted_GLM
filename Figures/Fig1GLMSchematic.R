# make Figure 1 (Right) GLM bias/surrogate schematic
library(ggplot2)
library(viridisLite)

# grab bias data for a HetGP fitted to 2021 GLM sims only
biasdat = read.csv("..Data/bias_dat_forHetGP.csv")
biasdat$X.1=NULL
biasdat$X = NULL
mycols = viridis(22)

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
preds = readRDS("Data/Surrogate_2021OnlyPreds.Rds")
pred_times = cbind(rep(1:366, 10), rep(0:9, each=366))
mean_trend = preds$mean
temp_sd = sqrt(preds$sd2 + preds$nugs)
hetDF = data.frame(cbind(pred_times, mean_trend, temp_sd))
colnames(hetDF) = c("DOY", "depth_int", "Mean", "SD")

hetDF$Lower = qnorm(0.05, hetDF$Mean, hetDF$SD)
hetDF$Upper = qnorm(0.95, hetDF$Mean, hetDF$SD)
bdat = filter(biasdat, YEAR == 2021)

# read in bias surrogate for 2021 (fit to observed - surrogate)
bias_2021 = readRDS("Data/Surrogate_bias_2021_Preds.Rds")
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
hetDF$BC_sd = sqrt(preds$sd2 + preds$nugs + bias_2021$sd2 + bias_2021$nugs)
hetDF$BC_Lower = qnorm(0.05, hetDF$BC_mean, hetDF$BC_sd)
hetDF$BC_Upper = qnorm(0.95, hetDF$BC_mean, hetDF$BC_sd)

bdat$Date = paste(bdat$YEAR, bdat$MONTH, bdat$DAY, sep = "-")
bdat$Date = as.POSIXct(bdat$Date, tz = "UTC")
hetDF = right_join(hetDF, ymd, by = "DOY")
hetDF$YEAR = 2021
hetDF$Date = paste(hetDF$YEAR, hetDF$MONTH, hetDF$DAY, sep = "-")
hetDF$Date = as.POSIXct(as.Date(hetDF$Date), tz = "UTC")


mycols =  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_cols = mycols[c(1,2 ,6,8)]
library(ggplot2)
ggplot(data = filter(bdat, depth_int == 6), aes(x = Date, y = temp_glm, col = "GLM")) +
  geom_line(linewidth = 1) +
  geom_line(data = filter(bdat, depth_int==6), aes(x = Date, y = temp_obs, col = "Observed"),
            linewidth = 1) +
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
  theme(text = element_text(size = 20, color = "black"),
        axis.text = element_text(size = 14, color = "black"))  +
  
  scale_color_manual(breaks = c("BC_Surrogate", "Surrogate", "Observed", "GLM"), 
                     values = c(Observed = plot_cols[1], GLM = plot_cols[2], Surrogate = plot_cols[3],
                                BC_Surrogate = plot_cols[4]),
                     labels = c("Bias-corrected surrogate", "Surrogate", "Observed", "GLM"), 
                     name = "") +
  theme(legend.position = c(0.24, 0.89), legend.text=element_text(size=18),
        legend.background=element_rect(fill = alpha("white", 0.1))) +
  ylab("Water temperature C") 
ggsave("PlotForCayelan.png", height = 1800, width = 2400, units="px")
breaks = c("BC_Surrogate", "Surrogate", "Observed", "GLM")
labels = c("BC_Surrogate", "Surrogate", "Observed", "GLM")