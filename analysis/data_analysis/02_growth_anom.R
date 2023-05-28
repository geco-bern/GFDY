# This script calculates growth anomalies from stand and tree level data

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggridges)
library(gamm4)
library(sjPlot)

# read selected data at stand level
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75out.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen55out.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen90out.RData")
length(unique(aggData_QMDbinsDen_out$PlotID))

# read selected data at tree level
load("~/GFDY/data/inputs_obs/aggTreeData75.RData")
load("~/GFDY/data/inputs_obs/aggTreeData55.RData")
load("~/GFDY/data/inputs_obs/aggTreeData90.RData")
length(unique(aggTreeData$PlotID))

aggData_analysis <- aggData_QMDbinsDen_out %>% left_join(aggTreeData)
summary(aggData_analysis$BiomassIncrement_Kg_m2_year)
length(unique(aggData_analysis$PlotID))

# Residuals Stand Growth ~ QMD or growth anomalies ####
# To analyse the effect of growth enhancement (growth anomalies, i.e., residuals) in STL relationships.
FitResStand=gamm4(BiomassIncrement_Kg_m2_year~s(QMD),random=~(1|PlotID),# + (1|Year),
                  data=aggData_analysis,na.action = "na.omit")
summary(FitResStand$gam)
plot.gam(FitResStand$gam,pages = 1)
plot_model(FitResStand$gam,type = "pred",terms = c("QMD"),show.data=T)
predict0 <- predict(FitResStand$gam,aggData_analysis,allow.new.levels = TRUE) 
Res_Growth0 <- aggData_analysis$BiomassIncrement_Kg_m2_year - predict0
plot(Res_Growth0 ~ predict0, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
plot(aggData_analysis$BiomassIncrement_Kg_m2_year ~ predict0, xlab="Predicted", ylab="Observed", main="Observed vs. fitted")
aggData_analysis <- aggData_analysis %>% mutate(Res_Growth0=Res_Growth0) 
summary(aggData_analysis$Res_Growth0)

# Figure S4 ####
plot_model(FitResStand$gam,type = "pred",terms = c("QMD"),show.data=T)
pred <- ggpredict(FitResStand$gam, terms = c("QMD"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig_S4 <- ggplot() + 
  geom_point(data = aggData_analysis, aes(x = QMD, y = BiomassIncrement_Kg_m2_year), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "QMD (cm)", y = expression(paste("Net biomass change (kg C ", m^-2, " ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     plot.title = element_text(size = 8)) 
fig_S4
ggsave(paste0(here::here(), "/manuscript/figures/fig_S4.png"), width = 5, height = 4.5, dpi=300)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S4.pdf"), width = 5, height = 4.5, dpi=300)

# Compare model simulations and observations  ####
# Increase Growth (BiomassIncrement_Kg_m2_year) +15% and +30% to mimic model simulations
aggData_analysis <- aggData_analysis %>% 
  mutate(GrowthPlus15 = BiomassIncrement_Kg_m2_year*1.15) %>% 
  mutate(GrowthPlus30 = BiomassIncrement_Kg_m2_year*1.30)
# Residuals from Growth +15% ~ s(QMD) and +30% ~ s(QMD)
aggData_analysis <- aggData_analysis %>% 
  mutate(Res_Growth15 = GrowthPlus15 - predict0,
         Res_Growth30 = GrowthPlus30 - predict0)

# Residuals Tree Growth ~ QMD or growth anomalies ####
# To analyse the effect of growth enhancement (growth anomalies, i.e., residuals) in STL relationships.
summary(aggData_analysis$mean_tree_biomass_growth_kgperm2peryear)

FitResTree=gamm4(mean_tree_biomass_growth_kgperm2peryear~s(mean_qmd),random=~(1|PlotID),# + (1|Year),
                 data=aggData_analysis,na.action = "na.omit")
summary(FitResTree$gam)
plot.gam(FitResTree$gam,pages = 1)
plot_model(FitResTree$gam,type = "pred",terms = c("mean_qmd"),show.data=T)
predictTree0 <- predict(FitResTree$gam,aggData_analysis,allow.new.levels = TRUE) 
Res_GrowthTree0 <- aggData_analysis$mean_tree_biomass_growth_kgperm2peryear - predictTree0
plot(Res_GrowthTree0 ~ predictTree0, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
plot(aggData_analysis$mean_tree_biomass_growth_kgperm2peryear ~ predictTree0, xlab="Predicted", ylab="Observed", main="Observed vs. fitted")
aggData_analysis <- aggData_analysis %>% mutate(Res_GrowthTree0=Res_GrowthTree0) 
summary(aggData_analysis$Res_GrowthTree0)

# Figure tree growth ####
plot_model(FitResTree$gam,type = "pred",terms = c("mean_qmd"),show.data=T)
pred <- ggpredict(FitResTree$gam, terms = c("mean_qmd"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

figResTree <- ggplot() + 
  geom_point(data = aggData_analysis, aes(x = mean_qmd, y = mean_tree_biomass_growth_kgperm2peryear), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "QMD (cm)", y = expression(paste("Mean tree biomass change (kg C ", m^-2, " ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) 
figResTree

save(aggData_analysis, file = "~/GFDY/data/inputs_obs/aggData_analysis75.RData")
save(aggData_analysis, file = "~/GFDY/data/inputs_obs/aggData_analysis55.RData")
save(aggData_analysis, file = "~/GFDY/data/inputs_obs/aggData_analysis90.RData")
length(unique(aggData_analysis$PlotID))

# Trends in forest attributes ####
load("~/GFDY/data/inputs_obs/aggData_analysis.RData")
fit_biomass_res = lmer(Res_Growth0 ~ Year + (1|PlotID) + (1|Species),
                   data = aggData_analysis, na.action = "na.exclude")
summary(fit_biomass_res)
plot_model(fit_biomass_res,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx #
# Residuals Tree Growth ~ QMD or growth anomalies ####
# To analyse the effect of growth enhancement (growth anomalies, i.e., residuals) in STL relationships.
FitResmeanTree=gamm4(mean_tree_biomass_growth_kgperyear~s(mean_qmd),random=~(1|PlotID),
                     data=aggData_analysis,na.action = "na.omit")
summary(FitResmeanTree$gam)
plot.gam(FitResmeanTree$gam,pages = 1)
plot_model(FitResmeanTree$gam,type = "pred",terms = c("mean_qmd"),show.data=T)
predictmeanTree0 <- predict(FitResmeanTree$gam,aggData_analysis,allow.new.levels = TRUE) 
Res_GrowthmeanTree0 <- aggData_analysis$mean_tree_biomass_growth_kgperyear - predictmeanTree0
plot(Res_GrowthmeanTree0 ~ predictmeanTree0, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
plot(aggData_analysis$mean_tree_biomass_growth_kgperyear ~ predictmeanTree0, xlab="Predicted", ylab="Observed", main="Observed vs. fitted")
aggData_analysis <- aggData_analysis %>% mutate(Res_GrowthmeanTree0=Res_GrowthmeanTree0) 
summary(aggData_analysis$Res_GrowthmeanTree0)
summary(predictmeanTree0)
summary(Res_GrowthmeanTree0)
