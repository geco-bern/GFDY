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
plot_model(Fit_Year,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Year[1962,1990,2019]"))
min(aggData_QMDbinsDen$Year)
max(aggData_QMDbinsDen$Year)

pred <- ggpredict(Fit_Year, terms = c("logQMD","Year[1962,1990,2019]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

preddata <- preddata %>% group_by(x) %>% 
  mutate(STL_Plus1=predicted-lag(predicted)) %>%
  mutate(STL_Plus2=predicted-lag(lag(predicted)))

NPlus1 <- preddata %>%
  filter(group==1991) %>%
  ungroup(x) %>%
  summarise(STL_Plus1=mean(STL_Plus1,na.rm=T)) %>% pull()

NPlus2 <- preddata %>%
  filter(group==1992) %>%
  ungroup(x) %>%
  summarise(STL_Plus2=mean(STL_Plus2)) %>% pull()

# Get upward shift
# predict y for a given x
pred <- ggpredict(Fit_Year, terms = c("logQMD[3]","Year[1990,1991]"), full.data = TRUE)
preddata <- as.data.frame(pred)
preddata <- preddata %>%
  mutate(upSTL=predicted-lag(predicted),
         e_upSTL=exp(predicted-lag(predicted))) 
preddata
upSTL75 <- preddata$e_upSTL[2]
upSTL55 <- preddata$e_upSTL[2]
upSTL90 <- preddata$e_upSTL[2]

# Get rightward shift
# predict x for a given y
Fit_Year = lmer(logDensity ~ logQMD + Year + (1|PlotID) + (1|Species), data = aggData_QMDbinsDen, na.action = "na.exclude")
intercept <- coef(summary(Fit_Year))[,"Estimate"][[1]]
a <- coef(summary(Fit_Year))[,"Estimate"][[2]]
b <- coef(summary(Fit_Year))[,"Estimate"][[3]]
rightSTL75 <- exp((intercept - 7 + b*1991)/abs(a) - (intercept - 7 + b*1990)/abs(a))
rightSTL55 <- exp((intercept - 7 + b*1991)/abs(a) - (intercept - 7 + b*1990)/abs(a))
rightSTL90 <- exp((intercept - 7 + b*1991)/abs(a) - (intercept - 7 + b*1990)/abs(a))

shifts <- data.frame(change=c("up55","up75","up90"),up=c(upSTL55,upSTL75,upSTL90),right=c(rightSTL55,rightSTL75,rightSTL90))
ggplot() + geom_col(data = shifts, aes(change,up))
ggplot() + geom_col(data = shifts, aes(change,right))

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

hist(aggData_QMDbinsDen$Year)
hist_Year <- ggplot(aggData_QMDbinsDen, aes(x=Year)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Year", limits = c(1960,2021), breaks = c(1962,1990,2019)) +
  scale_y_continuous("Frequency", limits = c(0,100), breaks = seq(0,100,20))
hist_Year

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
