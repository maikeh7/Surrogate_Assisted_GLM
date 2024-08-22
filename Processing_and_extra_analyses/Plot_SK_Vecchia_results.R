# Process and plot results for the comparison between 'true SK" and Vecchia approximation
library(ggplot2)
library(dplyr)
library(ggpubr)
library(data.table)
# difference in max/min temps
tempdiff= 29.637001-1.101086 
myfiles = list.files("../Data/RESULTS4_7_2024_SK_checkSK") #SK results
myfiles = list.files("../Data/RESULTS4_7_2024_VECCHIA_checkSK") #Vecchia results
resdf = data.frame()
for (i in 2:length(myfiles)){
  temp = readRDS(myfiles[i])
  resdf = rbind(resdf, temp)
}

myfiles = list.files()
resdfsk = data.frame()
for (i in 2:length(myfiles)){
  temp = readRDS(myfiles[i])
  resdfsk = rbind(resdfsk, temp)
}

library(ggplot2)
myresults = resdf %>% group_by(DOY) %>% summarise(pi95 = mean(VecchiaPI95), ci95 = mean(VecchiaCI95),
                                                  myRMSE = mean(VecchiaRMSE))
myresultssk = resdfsk %>% group_by(DOY) %>% summarise(pi95SK = mean(SKPI95), ci95SK = mean(SKCI95),
                                                      SK_RMSE = mean(SKRMSE))
head(myresultssk)
myresults2 = right_join(myresults, myresultssk, by = "DOY")
myresults2$ratio = (myresults2$SK_RMSE / myresults2$myRMSE) / tempdiff

mean(myresults2$pi95)

ggplot(myresults2, aes(x = DOY, y = ratio, col = "RMSE_ratio")) +
  geom_line(linewidth=1) +
  geom_line(data = myresults2, aes(x = DOY, y = ci95, col = "Vecchia_SK_CI"), linewidth=1) +
  geom_line(data = myresults2, aes(x = DOY, y = pi95, col = "Vecchia_SK_PI"), linewidth=1) +
  geom_line(data = myresults2, aes(x = DOY, y = ci95SK, col = "SK_CI"), linewidth=1) +
  geom_line(data = myresults2, aes(x = DOY, y = pi95SK, col = "SK_PI"), linewidth=1) +
  theme_bw() +
  xlim(c(0,365)) +
  ylab("Proportion") +
  theme(legend.position=c(0.78,0.35)) +
  
  scale_color_manual(breaks = c("RMSE_ratio", "Vecchia_SK_CI", "Vecchia_SK_PI", "SK_CI", "SK_PI"), 
                     values = c(RMSE_ratio = plot_cols[1], Vecchia_SK_CI = plot_cols[2], Vecchia_SK_PI = "#D55E00", 
                                SK_CI =  plot_cols[3], SK_PI = "#0072B2"),
                     labels = c("RMSE normalized ratio", "Vecchia SK CI coverage", "Vecchia SK PI coverage",
                                "True SK CI coverage", "True SK PI coverage"), 
                     # guide = guide_legend(override.aes = list(
                     #   linetype = c("solid", "solid", "dotted", "solid", "dotted"))),
                     name = "") 
ggsave("SK_resultsNEW.pdf", width = 14, height=9 , units="cm")

head(res3)
ggplot(res2, aes(x = DOY, y = ratio, col = "RMSE_ratio")) +
  geom_line(linewidth=1) +
  geom_line(data = res3, aes(x = DOY, y = mean_My_CI, col = "Approx_CI"), linewidth=1) +
  geom_line(data = res3, aes(x = DOY, y = mean_My_PI , col = "Approx_PI"), linewidth=1) +
  geom_line(data = res3, aes(x = DOY, y = mean_True_CI , col = "True_SK_CI"), linewidth=1) +
  geom_line(data = res3, aes(x = DOY, y = mean_True_PI , col = "True_SK_PI"), linewidth=1) +
  theme_bw() +
  xlim(c(0,130)) +
  ylab("Proportion") +
  theme(legend.position=c(0.78,0.35)) +
  
  scale_color_manual(breaks = c("RMSE_ratio", "Vecchia_CI", "Vecchia_PI", "True_SK_CI", "True_SK_PI"), 
                     values = c(RMSE_ratio = plot_cols[1], Vecchia_CI = plot_cols[2], Vecchia_PI = "#D55E00",
                                True_SK_CI = plot_cols[3], True_SK_PI = "#0072B2"),
                     labels = c("RMSE normalized ratio", "Approximation CI coverage", "Approximation PI coverage", 
                                "True SK CI coverage", "True SK PI coverage"), 
                     # guide = guide_legend(override.aes = list(
                     #   linetype = c("solid", "solid", "dotted", "solid", "dotted"))),
                     name = "") 
ggsave("SK_resultsNEW.pdf", width = 14, height=9 , units="cm")


plot_cols =  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mean(res3$mean_True_CI)
mean(res3$mean_True_PI)
mean(res3$mean_My_CI)
test=readRDS("AllFracDF.Rds")