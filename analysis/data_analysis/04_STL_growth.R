# This script analyses the changes in the STLs w.r.t growth (growth anomalies, i.e., residuals).
# Results generate Figure 1 B.

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(patchwork)
library(ggeffects)
library(patchwork)

# load data
load("~/GFDY/data/inputs_obs/aggData_analysis75.RData")
load("~/GFDY/data/inputs_obs/aggData_analysis55.RData")
load("~/GFDY/data/inputs_obs/aggData_analysis90.RData")
length(unique(aggData_analysis$PlotID))
str(aggData_analysis)
summary(aggData_analysis$Year)

# Model at stand level ####
# LMM model N ~ QMD and Res_Growth0 with 75th percentile
Fit_ResBio = lmer(logDensity ~ scale(logQMD) + scale(Res_Growth0) + (1|PlotID) + (1|Species) + (1|years_since_management_bins), 
                  data = aggData_analysis, na.action = "na.exclude")
summary(Fit_ResBio)

# Weighted model with PlotArea_ha
Fit_ResBio_PlotArea = lmer(logDensity ~ scale(logQMD) + scale(Res_Growth0) + scale(PlotArea_ha) +
                             (1|PlotID) + (1|Species) + (1|years_since_management_bins), 
                  data = aggData_analysis, na.action = "na.exclude")
summary(Fit_ResBio_PlotArea)
#anova(Fit_ResBio,Fit_ResBio_PlotArea)
AICc(Fit_ResBio,Fit_ResBio_PlotArea)

# Interactions
Fit_ResBioInter = lmer(logDensity ~ scale(logQMD) * scale(Res_Growth0) + (1|PlotID) + (1|Species) + (1|years_since_management_bins), 
                  data = aggData_analysis, na.action = "na.exclude")
summary(Fit_ResBioInter)
#anova(Fit_ResBio,Fit_ResBioInter)
AICc(Fit_ResBio,Fit_ResBioInter)

r.squaredGLMM(Fit_ResBio)
out <- summary(Fit_ResBio)
out$coefficients
plot(allEffects(Fit_ResBio))
plot_model(Fit_ResBio, type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Res_Growth0"))
plot_model(Fit_ResBio, type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Res_Growth0[-1,0,1]"))
summary(aggData_analysis$Res_Growth0)

pred <- ggpredict(Fit_ResBio, terms = c("logQMD","Res_Growth0[-1,0,1]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plot75Res <- ggplot() + 
  geom_point(data = aggData_analysis, aes(x = logQMD, y = logDensity), alpha=0.5, size = 1,col="black", shape = 16, inherit.aes = FALSE) + 
  #geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = .8,col="grey",shape = 16, inherit.aes = FALSE) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high,fill=group),alpha=.2,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = F,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of growth",color  = "Growth anomalies") + 
  scale_color_manual("Growth \nanomalies",#expression(paste(italic("Growth \nanomalies"))), 
                     breaks = c("-1","0", "1"), 
                     labels = c("-1","0", "1"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_fill_manual("Growth \nanomalies",#expression(paste(italic("Growth \nanomalies"))), 
                     breaks = c("-1","0", "1"), 
                     labels = c("-1","0", "1"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
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
plot75Res

hist(aggData_analysis$Res_Growth0)
hist_Res <- ggplot(aggData_analysis, aes(x=Res_Growth0)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Growth anomalies", breaks = c(-1,0,1)) +
  scale_y_continuous("Frequency", limits = c(0,113), breaks = seq(0,100,50))
hist_Res

gg2 <- plot75Res + inset_element(hist_Res, left = 0.65, bottom = 0.5, right = 0.99, top = 0.99)
gg2

# Figure 1 ms ####
ff1 <- plot75Year + inset_element(hist_Year, left = 0.6, bottom = 0.6, right = 0.99, top = 0.99, ignore_tag = TRUE) +
  plot75Res + inset_element(hist_Res, left = 0.6, bottom = 0.6, right = 0.99, top = 0.99, ignore_tag = TRUE) + 
  plot_layout(ncol = 2) + 
  #plot_annotation(tag_levels = list(c('a)', 'b)')))
  plot_annotation(tag_levels = 'A', tag_suffix = ")") #& 
  #theme(plot.margin = unit(rep(0.13,4), "cm"))#+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff1
ggsave("~/GFDY/manuscript/figures/fig_1.png", width = 8.5, height = 4.2, dpi=300)

# STL shifts ####
# Upward shift
# predict y for a given x
pred <- ggpredict(Fit_ResBio, terms = c("logQMD","Res_Growth0[0,1]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
preddata <- preddata %>% group_by(x) %>% 
  mutate(upSTL=predicted-lag(predicted)) %>%
  mutate(increment=predicted/lag(predicted)) %>%
  mutate(percent=upSTL*100/lag(predicted))
preddata
change_STL <- preddata %>%
  filter(group=="1") %>%
  ungroup(x) %>%
  summarise(percent=mean(percent)) %>% pull()
change_STL

# Model diagnostic ####

# Analysis of residuals
ResG <- residuals(Fit_ResBio)
FittedG <- fitted(Fit_ResBio)
par(mfrow=c(2,2))
plot(ResG ~ FittedG, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) ## Homocedasticity
plot(aggData_analysis$logDensity ~ FittedG, xlab="FittedG", ylab="TreesPerHectareAHC1_2", main = "xyplot")
abline(0, 1) 
hist(ResG, main="Histogram of residuals", xlab="Residuals",breaks=50) ## Normality
boxplot(ResG,ylab = "Residuals")

# Checking assumptions for linear models
plot_model(Fit_ResBio, type='diag') 

# 1) Normality of residuals: The residuals of the model are normally distributed.
# Q-Q plots: Dots should be plotted along the line
plot_model(Fit_ResBio, type='diag')[[1]] 
qqnorm(residuals(Fit_ResBio));qqline(residuals(Fit_ResBio),col="darkblue",lwd=2)
qqmath(Fit_ResBio)
plot_normality <- ggplot(data.frame(residuals=residuals(Fit_ResBio,type="pearson")), aes(sample=residuals)) + 
  stat_qq(alpha=.5,stroke=0,size=2) + stat_qq_line(color="#377EB8") +
  xlab("Theoretical quantiles") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(-3.5,3.5),breaks = seq(-3,3,1))
plot_normality
# Histogram: Distribution should look like normal curve
plot_model(Fit_ResBio, type='diag')[[3]] 
hist(residuals(Fit_ResBio), main = "",ylab="Density", xlab="Residuals",breaks=50) 
plot_histogram <- ggplot(data.frame(residuals=residuals(Fit_ResBio,type="pearson")), aes(x=residuals)) + 
  geom_histogram(aes(y = ..density..),color="#377EB8", fill="white") + 
  xlab("Residuals") + ylab("Density") + theme_bw() +
  geom_density(linetype = 1,colour = 2) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10))
plot_histogram
shapiro.test(residuals(Fit_ResBio)) # no normality (P<0.05!)

# Homoscedasticity (constant variance of residuals)
# Amount and distance of points scattered above/below line is equal or randomly spread
plot_model(Fit_ResBio, type='diag')[[4]] 
plot(Fit_ResBio)
plot_homocedasticity2 <- ggplot(data.frame(fitted=fitted(Fit_ResBio),residuals=residuals(Fit_ResBio,type="pearson")),
                               aes(x=fitted,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("Fitted Values") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10))
plot_homocedasticity2

# Linearity in each variable: Models are assumed to be linear in each of the independent variables. 
# This assumption can be checked with plots of the residuals versus each of the variables.
plot_linearity_var_qmd2 <- ggplot(data.frame(logQMD=aggData_analysis$logQMD,residuals=residuals(Fit_ResBio,type="pearson")),
                             aes(x=logQMD,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("ln QMD") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10))
plot_linearity_var_qmd2

plot_linearity_var_bio <- ggplot(data.frame(Res_Growth0=aggData_analysis$Res_Growth0,residuals=residuals(Fit_ResBio,type="pearson")),
                             aes(x=Res_Growth0,y=residuals)) + geom_point(alpha=.5,stroke=0,size=1.5,shape=16) + geom_hline(color="#377EB8",yintercept = 0, linetype = 1) +
  xlab("Growth anomalies") + ylab("Residuals") + theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),axis.title = element_text(size = 10))
plot_linearity_var_bio

# Figure S4 ####
ffS4 <- plot_homocedasticity1 + plot_linearity_var_qmd1 + plot_linearity_var_yr +
  plot_homocedasticity2 + plot_linearity_var_qmd2 + plot_linearity_var_bio +
  plot_layout(ncol = 3) + 
  #plot_annotation(tag_levels = list(c('a)', 'b)')))
  plot_annotation(tag_levels = 'A', tag_suffix = ")") #& 
#theme(plot.margin = unit(rep(0.13,4), "cm"))#+
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffS4
ggsave("~/GFDY/manuscript/figures/fig_S4.png", width = 9, height = 5.5, dpi=300)

# Model at tree level ####
# LMM model N ~ QMD and Res_Growth0 with 75th percentile
Fit_ResBioTree = lmer(logDensity ~ scale(logQMD) + scale(Res_GrowthTree0) + (1|PlotID) + (1|Species) + (1|years_since_management_bins), 
                  data = aggData_analysis, na.action = "na.exclude")
summary(Fit_ResBioTree)
out <- summary(Fit_ResBioTree)
out$coefficients
r.squaredGLMM(Fit_ResBioTree)
plot(allEffects(Fit_ResBioTree))
plot_model(Fit_ResBioTree, type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Res_GrowthTree0"))
plot_model(Fit_ResBioTree, type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Res_GrowthTree0[-0.04,0,0.04]"))
summary(aggData_analysis$Res_Growth0)

pred <- ggpredict(Fit_ResBioTree, terms = c("logQMD","Res_GrowthTree0[-0.04,0,0.04]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plotTreeRes <- ggplot() + 
  geom_point(data = aggData_analysis, aes(x = logQMD, y = logDensity), alpha=0.3, size = .8,col="black", shape = 16, inherit.aes = FALSE) + 
  #geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = .8,col="grey",shape = 16, inherit.aes = FALSE) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high,fill=group),alpha=.2,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = F,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of growth",color  = "Growth anomalies") + 
  scale_color_manual("Mean tree growth \nanomalies", 
                     breaks = c("-0.05","0", "0.05"), 
                     labels = c("-0.05","0.0", "0.05"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_fill_manual("Mean tree growth \nanomalies", 
                     breaks = c("-0.05","0", "0.05"), 
                     labels = c("-0.05","0.0", "0.05"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
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
  scale_y_continuous(limits = c(3.6,9.2))
plotTreeRes

hist(aggData_analysis$Res_GrowthTree0)
hist_TreeRes <- ggplot(aggData_analysis, aes(x=Res_GrowthTree0)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Growth anomalies") +
  scale_y_continuous("Frequency", limits = c(0,150), breaks = seq(0,150,50))
hist_TreeRes

gg3 <- plotTreeRes + inset_element(hist_TreeRes, left = 0.65, bottom = 0.5, right = 0.99, top = 0.99)
gg3

