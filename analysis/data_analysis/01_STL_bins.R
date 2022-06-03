# This script calculates the QMD bins for the STL analyses

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggridges)
library(gamm4)
library(sjPlot)

# QMD bins
load("~/GFDY/data/inputs_obs/aggData.RData")
aggData_QMDbins <- aggData %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(aggData_QMDbins$QMD_bins))

# Highlight the tails of the distributions
ggplot(aggData_QMDbins, aes(x = Density, y = QMD_bins, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.25, 0.5, 0.75, 0.95)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#0000FFA0", "#A0A0A0A0","#0000FFA0","#FF0000A0"),
                    labels = c("(0, 0.25]", "(0.25, 0.5]", "(0.5, 0.75]", "(0.75, 0.95]", "(0.95, 1]")) + labs(x="Tree density")

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles from 0.5 to 0.9
valueQuantile = 0.75
quantileX <- aggData_QMDbins %>% group_by(QMD_bins) %>% summarise(quantile(Density, c(valueQuantile))) 
max(quantileX$`quantile(Density, c(valueQuantile))`)
aggData_QMDbins <- aggData_QMDbins %>% left_join(quantileX)
aggData_QMDbinsDen <- aggData_QMDbins %>% filter(Density>=`quantile(Density, c(valueQuantile))`)
aggData_QMDbinsRest <- aggData_QMDbins %>% filter(Density<`quantile(Density, c(valueQuantile))`)

ggplot(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity)) + geom_point() +
  geom_smooth(data=aggData_QMDbinsDen, method='lm',se=F,fullrange=TRUE) +
  geom_point(data = aggData, aes(x = logQMD, y = logDensity), alpha=0.2) +
  theme_bw() + labs(x = "Stand size", y = "Tree Density",color = "Time period") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom")+ 
  scale_y_continuous(limits = c(2.5,10),breaks = seq(2.5,10,1)) + scale_x_continuous(limits = c(2,4.6),breaks = seq(2,4.5,.5))

ggplot(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity)) + geom_point() +
  geom_smooth(data=aggData_QMDbinsRest, method='lm',se=F,fullrange=TRUE) +
  geom_point(data = aggData, aes(x = logQMD, y = logDensity), alpha=0.2) +
  theme_bw() + labs(x = "Stand size", y = "Tree Density",color = "Time period") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom")

# Residuals Growth ~ QMD or growth anomalies ####
# To analyse the effect of growth enhancement (growth anomalies, i.e., residuals) in STL relationships.
FitRes=gamm4(BiomassIncrement_Kg_m2_year~s(QMD),random=~(1|PlotID)+ (1|Year),data=aggData_QMDbinsDen,na.action = "na.omit")
summary(FitRes$gam)
plot.gam(FitRes$gam,pages = 1)
plot_model(FitRes$gam,type = "pred",terms = c("QMD"),show.data=T)
predict0 <- predict(FitRes$gam,aggData_QMDbinsDen,allow.new.levels = TRUE) 
Res_Growth0 <- aggData_QMDbinsDen$BiomassIncrement_Kg_m2_year - predict0
plot(Res_Growth0 ~ predict0, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
plot(aggData_QMDbinsDen$BiomassIncrement_Kg_m2_year ~ predict0, xlab="Predicted", ylab="Observed", main="Observed vs. fitted")
aggData_QMDbinsDen <- aggData_QMDbinsDen %>% mutate(Res_Growth0=Res_Growth0) 

figRes <- plot_model(FitRes$gam,type = "pred",terms = c("QMD"),show.data=T) + 
  labs(x = "QMD (cm)", y = expression(paste("Biomass increment (Kg C ", m^-2, " ", yr^-1, ") ")),title="") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  guides(color = "none") 
figRes
ggsave("~/GFDY/manuscript/figures/fig_S1.png", width = 8, height = 6, dpi=300)

# Compare model simulations and observations  ####
# Increase Growth (BiomassIncrement_Kg_m2_year) +15% and +30% to mimic model simulations
aggData_QMDbinsDen <- aggData_QMDbinsDen %>% 
  mutate(GrowthPlus15 = BiomassIncrement_Kg_m2_year*1.15) %>% 
  mutate(GrowthPlus30 = BiomassIncrement_Kg_m2_year*1.30)

# Residals from Growth +15% ~ s(QMD) and +30% ~ s(QMD)
aggData_QMDbinsDen <- aggData_QMDbinsDen %>% 
  mutate(Res_Growth15 = GrowthPlus15 - predict0,
         Res_Growth30 = GrowthPlus30 - predict0)

save(aggData_QMDbinsDen, file = "~/GFDY/data/inputs_obs/aggData_QMDbinsDen.RData")
