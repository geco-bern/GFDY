# This script calculates growth anomalies for stand and tree level growth data
# This script creates Figure S4

# load packages
library(gamm4)
library(sjPlot)
library(dplyr)
library(ggeffects)
library(ggplot2)

# Read data ####

# read data at stand level
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen55out.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen90out.RData"))
length(unique(aggData_QMDbinsDen_out$PlotID))

# read data at tree level
load(paste0(here::here(), "/data/inputs_obs/aggTreeData75.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggTreeData55.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggTreeData90.RData"))
length(unique(aggTreeData$PlotID))

# Growth anomalies ####

## Stand growth  ####
# To analyse the effect of growth enhancement (growth anomalies, i.e., residuals) in STL relationships.
FitResStand=gamm4(BiomassIncrement_Kg_m2_year~s(QMD),random=~(1|PlotID),# + (1|Year),
                  data=aggData_QMDbinsDen_out,na.action = "na.omit")
summary(FitResStand$gam)
plot.gam(FitResStand$gam,pages = 1)
plot_model(FitResStand$gam,type = "pred",terms = c("QMD"),show.data=T)
predict0 <- predict(FitResStand$gam,aggData_QMDbinsDen_out,allow.new.levels = TRUE) 
Res_Growth0 <- aggData_QMDbinsDen_out$BiomassIncrement_Kg_m2_year - predict0
plot(Res_Growth0 ~ predict0, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
plot(aggData_QMDbinsDen_out$BiomassIncrement_Kg_m2_year ~ predict0, xlab="Predicted", ylab="Observed", main="Observed vs. fitted")
aggData_QMDbinsDen_out <- aggData_QMDbinsDen_out %>% mutate(Res_Growth0=Res_Growth0) 
summary(aggData_QMDbinsDen_out$Res_Growth0)

### Figure S4 ####
plot_model(FitResStand$gam,type = "pred",terms = c("QMD"),show.data=T)
pred <- ggpredict(FitResStand$gam, terms = c("QMD"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

fig_S4 <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen_out, aes(x = QMD, y = BiomassIncrement_Kg_m2_year), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "QMD (cm)", y = expression(paste("Net biomass change (kg C ", m^-2, " ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     plot.title = element_text(size = 8)) 
fig_S4
ggsave(paste0(here::here(), "/manuscript/figures/fig_S4.png"), width = 5, height = 4.5, dpi=300)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S4.pdf"), width = 5, height = 4.5, dpi=300)

# Compare model simulations and observations
# Increase Growth (BiomassIncrement_Kg_m2_year) +15% and +30% to mimic model simulations
aggData_QMDbinsDen_out <- aggData_QMDbinsDen_out %>% 
  mutate(GrowthPlus15 = BiomassIncrement_Kg_m2_year*1.15) %>% 
  mutate(GrowthPlus30 = BiomassIncrement_Kg_m2_year*1.30)
# Residuals from Growth +15% ~ s(QMD) and +30% ~ s(QMD)
aggData_QMDbinsDen_out <- aggData_QMDbinsDen_out %>% 
  mutate(Res_Growth15 = GrowthPlus15 - predict0,
         Res_Growth30 = GrowthPlus30 - predict0)

save(aggData_QMDbinsDen_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
#save(aggData_QMDbinsDen_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen55out.RData"))
#save(aggData_QMDbinsDen_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen90out.RData"))

## Tree growth  ####
# To analyse the effect of growth enhancement (growth anomalies, i.e., residuals) in STL relationships.
FitResTree=gamm4(mean_tree_biomass_growth_kgperm2peryear~s(mean_qmd),random=~(1|PlotID),# + (1|Year),
                 data=aggTreeData,na.action = "na.omit")
summary(FitResTree$gam)
plot.gam(FitResTree$gam,pages = 1)
plot_model(FitResTree$gam,type = "pred",terms = c("mean_qmd"),show.data=T)
predictTree0 <- predict(FitResTree$gam,aggTreeData,allow.new.levels = TRUE) 
Res_GrowthTree0 <- aggTreeData$mean_tree_biomass_growth_kgperm2peryear - predictTree0
plot(Res_GrowthTree0 ~ predictTree0, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
plot(aggTreeData$mean_tree_biomass_growth_kgperm2peryear ~ predictTree0, xlab="Predicted", ylab="Observed", main="Observed vs. fitted")
aggTreeData <- aggTreeData %>% mutate(Res_GrowthTree0=Res_GrowthTree0) 
summary(aggTreeData$Res_GrowthTree0)

### Figure tree growth
plot_model(FitResTree$gam,type = "pred",terms = c("mean_qmd"),show.data=T)
pred <- ggpredict(FitResTree$gam, terms = c("mean_qmd"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

figResTree <- ggplot() + 
  geom_point(data = aggTreeData, aes(x = mean_qmd, y = mean_tree_biomass_growth_kgperm2peryear), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "QMD (cm)", y = expression(paste("Mean tree biomass change (kg C ", m^-2, " ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) 
figResTree

save(aggTreeData, file = paste0(here::here(), "/data/inputs_obs/aggTreeData75.RData"))
#save(aggTreeData, file = paste0(here::here(), "/data/inputs_obs/aggTreeData55.RData"))
#save(aggTreeData, file = paste0(here::here(), "/data/inputs_obs/aggTreeData90.RData"))
