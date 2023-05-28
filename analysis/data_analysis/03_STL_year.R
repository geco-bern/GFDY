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
library(lattice)

# load data
load("~/GFDY/data/inputs_obs/aggData_analysis75.RData")
#load("~/GFDY/data/inputs_obs/aggData_analysis55.RData")
#load("~/GFDY/data/inputs_obs/aggData_analysis90.RData")
length(unique(aggData_analysis$PlotID))

# Run the model ####
# LMM model N ~ QMD and Year with 75th percentile
Fit_Year = lmer(logDensity ~ scale(logQMD) + scale(Year) + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                data = aggData_analysis, na.action = "na.exclude")
summary(Fit_Year)
r.squaredGLMM(Fit_Year)
out <- summary(Fit_Year)
out$coefficients
confint(Fit_Year)
upperCI <-  out$coefficients["scale(Year)","Estimate"] + out$coefficient["scale(Year)","Std. Error"]*1.96
lowerCI <-  out$coefficients["scale(Year)","Estimate"] - out$coefficient["scale(Year)","Std. Error"]*1.96
upperCI_unscaled <- out$coefficients["scale(Year)","Estimate"]/ sd(aggData_analysis$Year) +
  out$coefficient["scale(Year)","Std. Error"]/ sd(aggData_analysis$Year)*1.96
lowerCI_unscaled <- out$coefficients["scale(Year)","Estimate"]/ sd(aggData_analysis$Year) -
  out$coefficient["scale(Year)","Std. Error"]/ sd(aggData_analysis$Year)*1.96
plot(allEffects(Fit_Year))
plot_model(Fit_Year,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Year"))
plot_model(Fit_Year,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Year[1950,1985,2019]"))

# Figure 1a ####
pred <- ggpredict(Fit_Year, terms = c("logQMD","Year[1950,1985,2019]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plot75Year <- ggplot() + 
  geom_point(data = aggData_analysis, aes(x = logQMD, y = logDensity), alpha=0.5, size = 1,col="black", shape = 16, inherit.aes = FALSE) + 
  #geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = .8,col="grey",shape = 16, inherit.aes = FALSE) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high,fill=group),alpha=.2,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = F,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of time",color  = "Year") + 
  scale_color_manual("Year", #expression(paste(italic("Year"))), 
                     breaks = c("1950","1985", "2019"), 
                     values = c("#FC4E07", "#00AFBB", "#E7B800")) +
  scale_fill_manual("Year", #expression(paste(italic("Year"))), 
                    breaks = c("1950","1985", "2019"), 
                    values = c("#FC4E07", "#00AFBB", "#E7B800")) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.13, .20),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.6, 'cm'),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(2,4.6),breaks = seq(2.5,4.5,1))+
  scale_y_continuous(limits = c(4.5,9.2),breaks = seq(5,9,2))
plot75Year

hist_Year <- ggplot(aggData_analysis, aes(x=Year)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Year", breaks = c(1950,1985,2019)) +
  scale_y_continuous("Frequency", limits = c(0,113), breaks = seq(0,100,50))
hist_Year

# STL shifts ####
# Upward shift
# predict y for a given x
pred <- ggpredict(Fit_Year, terms = c("logQMD","Year[1994,1995]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
preddata <- preddata %>% group_by(x) %>% 
  mutate(upSTL=predicted-lag(predicted)) %>%
  mutate(increment=predicted/lag(predicted)) %>%
  mutate(percent=upSTL*100/lag(predicted))
preddata
change_STL <- preddata %>%
  filter(group=="1995") %>%
  ungroup(x) %>%
  summarise(percent=mean(percent)) %>% pull()
change_STL

# Model diagnostic ####

# Checking assumptions for linear models
# plot_model(Fit_Year, type='diag') 

# 1) Normality of residuals: The residuals of the model are normally distributed.
# Q-Q plots: Dots should be plotted along the line
plot_model(Fit_Year, type='diag')[[1]] 
qqnorm(residuals(Fit_Year));qqline(residuals(Fit_Year),col="darkblue",lwd=2)
qqmath(Fit_Year)
plot_normality <- ggplot(data.frame(residuals=residuals(Fit_Year,type="pearson")), aes(sample=residuals)) + 
  stat_qq(alpha=.5,stroke=0,size=2) + stat_qq_line(color="#377EB8") +
  xlab("Theoretical quantiles") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) 
plot_normality
# Histogram: Distribution should look like normal curve
plot_model(Fit_Year, type='diag')[[3]] 
hist(residuals(Fit_Year), main = "",ylab="Density", xlab="Residuals",breaks=50) 
plot_histogram <- ggplot(data.frame(residuals=residuals(Fit_Year,type="pearson")), aes(x=residuals)) + 
  geom_histogram(aes(y = ..density..),color="#377EB8", fill="white") + 
  xlab("Residuals") + ylab("Density") + theme_bw() +
  geom_density(linetype = 1,colour = 2) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10))
plot_histogram
shapiro.test(residuals(Fit_Year)) # no normality (P<0.05!)

# Figure S5a,b,c ####
# Homoscedasticity (constant variance of residuals)
# Amount and distance of points scattered above/below line is equal or randomly spread
plot_model(Fit_Year, type='diag')[[4]] 
plot(Fit_Year)
plot_homocedasticity1 <- ggplot(data.frame(fitted=fitted(Fit_Year),residuals=residuals(Fit_Year,type="pearson")),
                                aes(x=fitted,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("Fitted (ln N)") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(5,9), breaks = seq(5,9,2)) +
  scale_y_continuous(limits = c(-0.53,0.53), breaks = seq(-0.5,0.5,0.5))
plot_homocedasticity1

# Linearity in each variable: Models are assumed to be linear in each of the independent variables. 
# This assumption can be checked with plots of the residuals versus each of the variables.
plot_linearity1_var_qmd <- ggplot(data.frame(logQMD=aggData_analysis$logQMD,residuals=residuals(Fit_Year,type="pearson")),
                                  aes(x=logQMD,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("ln QMD") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(2,4), breaks = seq(2,4,1)) +
  scale_y_continuous(limits = c(-0.53,0.53), breaks = seq(-0.5,0.5,0.5))
plot_linearity1_var_qmd

plot_linearity1_var_yr <- ggplot(data.frame(Year=aggData_analysis$Year,residuals=residuals(Fit_Year,type="pearson")),
                                 aes(x=Year,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("Year") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(breaks = c(1940,1980,2020)) +
  scale_y_continuous(limits = c(-0.53,0.53), breaks = seq(-0.5,0.5,0.5))
plot_linearity1_var_yr

# More tests ####
# 1. Exclude residuals from model fit
Res_Fit_Year <- residuals(Fit_Year, type = "pearson")
rowindex_outliers <- as.integer(names(boxplot.stats(Res_Fit_Year, coef = 1.5)$out))
aggData_analysis <- aggData_analysis |> 
  mutate(rowindex = dplyr::row_number()) |>
  mutate(outlier = rowindex %in% rowindex_outliers) 
aggData_analysis_out <- aggData_analysis |>
  filter(outlier==FALSE)

Fit_Year_out = lmer(logDensity ~ scale(logQMD) + scale(Year) + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                data = aggData_analysis_out, na.action = "na.exclude")
summary(Fit_Year_out)
plot_model(Fit_Year_out,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Year[1950,1985,2019]"))

aggData_analysis |> 
  ggplot(aes(x = logQMD, y = logDensity, color = outlier)) + 
  geom_point() + 
  scale_color_manual("Outlier?",                    # Set title of legend
                     values = c("black", "red"),    # Highlight in red
                     labels = c("No", "Yes")        # Add labels to the legend
  ) + theme_classic()

# 2. Weighted model with PlotArea_ha
Fit_Year_PlotArea = lmer(logDensity ~ scale(logQMD) + scale(Year) + scale(PlotArea_ha) + 
                      (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                    data = aggData_analysis, na.action = "na.exclude")
summary(Fit_Year_PlotArea)
AICc(Fit_Year,Fit_Year_PlotArea)

# 3. Interactions
Fit_YearInter = lmer(logDensity ~ scale(logQMD) * scale(Year) + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                data = aggData_analysis, na.action = "na.exclude")
summary(Fit_YearInter)
AICc(Fit_Year,Fit_YearInter)

# 4. Exclude 10% (25%) smallest and largest stands (by their QMD).
aggData_analysis_sub <- aggData_analysis %>% filter(between(logQMD, quantile(logQMD, .10), quantile(logQMD, .90)))
Fit_Year_sub = lmer(logDensity ~ scale(logQMD) + scale(Year) + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                data = aggData_analysis_sub, na.action = "na.exclude")
summary(Fit_Year_sub)
plot_model(Fit_Year_sub,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Year[1950,1985,2019]"))

# Figure S10a ####
pred <- ggpredict(Fit_Year_sub, terms = c("logQMD","Year[1950,1985,2019]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plot75Year_sub <- ggplot() + 
  geom_point(data = aggData_analysis_sub, aes(x = logQMD, y = logDensity), alpha=0.5, size = 1,col="black", shape = 16, inherit.aes = FALSE) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high,fill=group),alpha=.2,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = F,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of time",color  = "Year") + 
  scale_color_manual("Year", #expression(paste(italic("Year"))), 
                     breaks = c("1950","1985", "2019"), 
                     values = c("#FC4E07", "#00AFBB", "#E7B800")) +
  scale_fill_manual("Year", #expression(paste(italic("Year"))), 
                    breaks = c("1950","1985", "2019"), 
                    values = c("#FC4E07", "#00AFBB", "#E7B800")) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.13, .20),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.6, 'cm'),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(2.5,3.9),breaks = seq(2.75,4,.5))+
  scale_y_continuous(limits = c(5.6,8.2),breaks = seq(6,8,1))
plot75Year_sub

hist_Year_sub <- ggplot(aggData_analysis_sub, aes(x=Year)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Year", breaks = c(1950,1985,2019)) +
  scale_y_continuous("Frequency", limits = c(0,113), breaks = seq(0,100,50))
hist_Year_sub

# Figure S10c ####
# Homoscedasticity (constant variance of residuals)
# Amount and distance of points scattered above/below line is equal or randomly spread
plot(Fit_Year_sub)
plot_homocedasticity1_sub <- ggplot(data.frame(fitted=fitted(Fit_Year_sub),residuals=residuals(Fit_Year_sub,type="pearson")),
                                    aes(x=fitted,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("Fitted (ln N)") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(5.5,8), breaks = seq(6,8,1)) +
  scale_y_continuous(limits = c(-0.33,0.33), breaks = seq(-0.3,0.3,0.3))
plot_homocedasticity1_sub

# Linearity in each variable: Models are assumed to be linear in each of the independent variables. 
# This assumption can be checked with plots of the residuals versus each of the variables.
plot_linearity1_var_qmd_sub <- ggplot(data.frame(logQMD=aggData_analysis_sub$logQMD,residuals=residuals(Fit_Year_sub,type="pearson")),
                                      aes(x=logQMD,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("ln QMD") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(2.5,3.9), breaks = seq(2.5,3.5,.5)) +
  scale_y_continuous(limits = c(-0.33,0.33), breaks = seq(-0.3,0.3,0.3))
plot_linearity1_var_qmd_sub

plot_linearity1_var_yr_sub <- ggplot(data.frame(Year=aggData_analysis_sub$Year,residuals=residuals(Fit_Year_sub,type="pearson")),
                                     aes(x=Year,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("Year") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(breaks = c(1940,1980,2020)) +
  scale_y_continuous(limits = c(-0.33,0.33), breaks = seq(-0.3,0.3,0.3))
plot_linearity1_var_yr_sub
