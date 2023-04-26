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
#library(nlme)
library(DHARMa)
library(lattice)

# load data
load("~/GFDY/data/inputs_obs/aggtreeData.RData")

str(aggtreeData)
sort(unique(aggtreeData$Species))

aggtreeData <- aggtreeData %>% mutate(lnDBH_growth=log(DBH_growth+1))
summary(aggtreeData$Year)

aggtreeData1 <- aggtreeData %>% filter(Year<=1990)
aggtreeData2 <- aggtreeData %>% filter(Year>1985|Year<=2000)
aggtreeData3 <- aggtreeData %>% filter(Year>2000)

aggtreeDataSps <- aggtreeData %>% filter(Species=="Abies alba"|Species=="Picea abies"|Species=="Pinus cembra"|Species=="Larix decidua"|Species=="Fagus sylvatica")
sort(unique(aggtreeData$Species))

# LMM model 
Fit_tree = lmer(Biomass_growth_kg ~ scale(Year) + (1|PlotID) + (1|Species),
                data = aggtreeData, na.action = "na.exclude")

Fit_tree = lmer(Biomass_growth_kg ~ scale(Year)*scale(DBH) + (1|PlotID) + (1|Species),
                data = aggtreeData, na.action = "na.exclude")

Fit_tree = lmer(DBH_growth ~ scale(DBH) + scale(Year)*scale(elevation) + (1|PlotID) + (1|Species),
                data = aggtreeData, na.action = "na.exclude")

summary(Fit_tree)
plot(allEffects(Fit_tree))
plot_model(Fit_tree,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year","DBH"))
plot_model(Fit_tree,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("DBH"))
plot_model(Fit_tree,type = "pred",show.data=T, dot.size=1.5, terms = c("Year"))

# Weighted model with PlotArea_ha (without including PlotID as random)
Fit_Year_wei = lmer(logDensity ~ scale(logQMD) + scale(Year) + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                    weights = PlotArea_ha, data = aggData_QMDbinsDen, na.action = "na.exclude")
Fit_Year_wei = lmer(logDensity ~ scale(logQMD) + scale(Year) + scale(PlotArea_ha) + 
                      (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                    data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(Fit_Year_wei)
# No year model
Fit = lmer(logDensity ~ scale(logQMD) + (1|PlotID) + (1|Species) + (1|years_since_management_bins), 
           data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(Fit)
AICc(Fit, Fit_Year)
out_anova <- anova(Fit, Fit_Year)
out <- summary(Fit_Year)
out$coefficients
confint(Fit_Year)
upperCI <-  out$coefficients["scale(Year)","Estimate"] + out$coefficient["scale(Year)","Std. Error"]*1.96
lowerCI <-  out$coefficients["scale(Year)","Estimate"] - out$coefficient["scale(Year)","Std. Error"]*1.96
upperCI_unscaled <- out$coefficients["scale(Year)","Estimate"]/ sd(aggData_QMDbinsDen$Year) +
  out$coefficient["scale(Year)","Std. Error"]/ sd(aggData_QMDbinsDen$Year)*1.96
lowerCI_unscaled <- out$coefficients["scale(Year)","Estimate"]/ sd(aggData_QMDbinsDen$Year) -
  out$coefficient["scale(Year)","Std. Error"]/ sd(aggData_QMDbinsDen$Year)*1.96
r.squaredGLMM(Fit_Year)
plot(allEffects(Fit_tree))
plot_model(Fit_Year,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Year[1946,1985,2019]"))
summary(aggData_QMDbinsDen$Year)

pred <- ggpredict(Fit_Year, terms = c("logQMD","Year[1946,1985,2019]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plot75Year <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity), alpha=0.3, size = .8,col="black", shape = 16, inherit.aes = FALSE) + 
  geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = .8,col="grey",shape = 16, inherit.aes = FALSE) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of time",color  = "Year") + 
  scale_color_manual("Year", #expression(paste(italic("Year"))), 
                     breaks = c("1946","1985", "2019"), 
                     values = c("#FC4E07", "#00AFBB", "#E7B800")) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.11, .20),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.6, 'cm'),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(1.95,4.7),breaks = seq(2.5,4.5,1))+
  scale_y_continuous(limits = c(3.6,9.2))
plot75Year
