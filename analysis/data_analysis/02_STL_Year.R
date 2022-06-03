# This script analyses the changes in the STLs over time (calendar year).
# Results generate Figure 1 A.

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(ggeffects)
library(patchwork)

# load data
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen.RData")

# LME model N ~ QMD and Year with 75th percentile
Fit_Year = lmer(logDensity ~ scale(logQMD) + scale(Year) + (1|PlotID) + (1|Species), data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(Fit_Year)
r.squaredGLMM(Fit_Year)
plot(allEffects(Fit_Year))
plot_model(Fit_Year,type = "pred",terms = c("logQMD","Year"))

hist(aggData_QMDbinsDen$Year)
hist_Year <- ggplot(aggData_QMDbinsDen, aes(x=Year)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Year", limits = c(1960,2021), breaks = c(1962,1990,2019)) +
  scale_y_continuous("Frequency", limits = c(0,100), breaks = seq(0,100,20))
hist_Year

# type = "pred" to plot predicted values (marginal effects) for specific model terms.
plot75Year <- plot_model(Fit_Year, type = "pred",show.data=F, dot.size=1.5,line.size=.5,
                         terms = c("logQMD","Year[1962,1990,2019]"),title = "STL changes as a function of calendar year",
                         axis.title = c("Ln QMD","Ln N"),legend.title = "Year",
                         colors = c("#FC4E07", "#00AFBB", "#E7B800")) + 
  geom_point(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity), alpha=0.3, size = 1,col="black", shape = 18, inherit.aes = FALSE) + 
  geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = 1,col="grey",shape = 18, inherit.aes = FALSE) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                     plot.title = element_text(size = 10),
                     legend.position = c(.11, .20),
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.key.size = unit(.6, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(2,4.5),breaks = seq(2,4.5,0.5))+
  scale_y_continuous(limits = c(4.5,9.2)) 
plot75Year

pred <- ggpredict(Fit_Year, terms = c("logQMD","Year[1962,1990,2019]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plot75Year <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity), alpha=0.3, size = .8,col="black", shape = 16, inherit.aes = FALSE) + 
  geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = .8,col="grey",shape = 16, inherit.aes = FALSE) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of calendar year",color  = "Year") + 
  scale_color_manual("Year", 
                        breaks = c("1962","1990", "2019"), 
                        values = c("#FC4E07", "#00AFBB", "#E7B800")) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.11, .20),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.6, 'cm'),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(2,4.5),breaks = seq(2,4.5,0.5))+
  scale_y_continuous(limits = c(4.5,9.2))
plot75Year

gg1 <- plot75Year + inset_element(hist_Year, left = 0.65, bottom = 0.5, right = 0.99, top = 0.99)
gg1

# Analysis of residuals
ResG <- residuals(Fit_Year)
FittedG <- fitted(Fit_Year)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(aggData_QMDbinsDen$logDensity ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals") ## Normality
boxplot(ResG,ylab = "Residuals")
