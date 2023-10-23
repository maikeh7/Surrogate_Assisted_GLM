library(hetGP)
library(ggplot2)
# code to construct HetGP climatolgical model 

# Get observed data 
# update_observed_data() 
obs_dir = "../Data/Observed_data.csv"
read_obs_data = function(obs_dir){
  obs_data = read.csv(obs_dir)
  obs_data$date = as.POSIXct(obs_data$datetime, tz = "UTC")
  obs_data$X = NULL
  return(obs_data)
}
obs = read_obs_data(obs_dir)
head(obs)

# filter to only train on train data! We will predict on test dates ONLY
obs = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2023-06-11"))
colnames(obs)[4] = "Depth"

# set up model matrix
ModelMat = cbind(obs$DOY, obs$depth_int)
Z = obs$temp_obs

# fit HetGP model using Gaussian cov function
start=Sys.time()
het_depth <- mleHetGP(ModelMat, Z, covtype =  "Gaussian")
stop = Sys.time()
print(stop-start)

het_depth_rebuild = rebuild(het_depth, robust = TRUE)

# construct prediction matrix/make predictions
pred_times = cbind(rep(1:366, 10), rep(0:9, each=366))

preds = predict(x = pred_times, object = het_depth_rebuild)

# get posterior means/sd
mean_trend = preds$mean
temp_sd = sqrt(preds$sd2 + preds$nugs)
pred_times = data.frame(pred_times)
colnames(pred_times) = c("DOY", "Depth")
pred_times$mean = mean_trend
pred_times$hetSD = temp_sd
pred_times$Lower = qnorm(0.05, pred_times$mean, pred_times$hetSD)
pred_times$Upper = qnorm(0.95, pred_times$mean, pred_times$hetSD)

# make a plot to make sure it looks ok
ggplot(pred_times, aes(x = DOY, y = mean)) +
  geom_point(data = obs, aes(x = DOY, y = temp_obs), col = "gray", size = .6) +
  geom_line(aes(x = DOY, y = mean)) +
  geom_line(aes(x = DOY, y = Lower), linetype = "dashed") +
  geom_line(aes(x = DOY, y = Upper), linetype = "dashed") +
  theme_bw() +
  ylab("Water temperature °C") +
  facet_wrap(~factor(Depth), nrow = 2, ncol = 5)
#ggsave(file.path(plot_dir, "ObservedGP.png"))

# merge observed data TEST PERIOD ONLY and preds from hetGP model (used in Results)
obs = read_obs_data(obs_dir)
obs2 = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2022-06-11"))
colnames(obs2)[4] = "Depth"
obs2 = obs2[, c("YEAR", "MONTH", "DAY", "Depth", "temp_obs", "DOY")]
pred_times2 = right_join(pred_times, obs2, by = c("DOY", "Depth"))
pred_times2 = pred_times2[complete.cases(pred_times2), ]
#write.csv(pred_times2, "../Data/Observed_hetGP_withOb_TESTONLY.csv")

# Now save HetGP preds for TRAIN PERIOD ONLY (for making plots)
obs=read_obs_data()
obs2 = filter(obs, date >= as.Date("2020-10-03") & date <= as.Date("2023-06-11"))
colnames(obs2)[4] = "Depth"
obs2 = obs2[, c("YEAR", "MONTH", "DAY", "Depth", "temp_obs", "DOY", "date")]
head(obs2)
pred_times2= right_join(pred_times, obs2, by = c("DOY", "Depth"))
pred_times2 = pred_times2[complete.cases(pred_times2), ]
#write.csv(pred_times2, "../Data/Observed_hetGP_withOb_TRAINONLY.csv")


