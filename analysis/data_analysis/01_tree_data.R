# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gamm4)
library(sjPlot)
library(ggeffects)
library(patchwork)
library(lmerTest) 
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)

# read selected data at stand level
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen55.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen90.RData")
length(unique(aggData_QMDbinsDen$PlotID))
# Filter selected plots for EFM
EFM_sel_plots <- aggData_QMDbinsDen %>% filter(dataset=="EFM")
NFI_sel_plots <- aggData_QMDbinsDen %>% filter(dataset=="NFI")
NFR_sel_plots <- aggData_QMDbinsDen %>% filter(dataset=="NFR")
length(unique(NFR_sel_plots$PlotID))

# EFM - Experimental Forest Management ####

# Plot area
EFM_plot_area <- read.csv("~/GFDY/data/raw_obs/efm/EFM_plot_area.csv")
str(EFM_plot_area)

## Tree-level data from EFM ####
EFM_tree_pre <- readRDS("~/GFDY/data/raw_obs/efm/EFM_tree_data.RDS") # 18 plots
EFM_tree_6003 <- readRDS("~/GFDY/data/raw_obs/efm/EFM_tree_data6003.RDS") # 44 plots
EFM_tree <- EFM_tree_pre %>% bind_rows(EFM_tree_6003) %>% filter(FNUM!=6003000) %>% 
  rename(TreeID=BNR,TreeStatus=AHC) %>% left_join(EFM_plot_area)
str(EFM_tree)
length(unique(EFM_tree$FNUM))
length(unique(EFM_tree$TreeID))
# Select trees alive (1) and marked for thinning (2) (see pdf EFM for classes)
EFM_tree <- EFM_tree %>% #filter(TreeStatus==1|TreeStatus==2) %>%  
  mutate(Biomass_kg = Abovegroundmass_kg + Rootmassmass_kg)
# Calculate growth in terms of biomass at tree level
EFM_tree <- EFM_tree %>% arrange(FNUM,TreeID) %>% group_by(FNUM,TreeID) %>%
  mutate(PeriodLength_years=AJ-lag(AJ)) %>% relocate(PeriodLength_years, .after=AJ) %>% 
  mutate(Biomass_growth_kgperyear=(Biomass_kg-lag(Biomass_kg))/PeriodLength_years) %>% 
  ungroup() %>% 
  mutate(Biomass_growth_kgperhaperyear=Biomass_growth_kgperyear/PlotArea_ha) %>%
  mutate(Biomass_growth_kgperm2peryear=Biomass_growth_kgperhaperyear/10000) %>%
  group_by(FNUM,AJ) %>% mutate(nTrees=n()) %>% ungroup()
# Select same plots selected at stand level
EFM_tree <- EFM_tree %>% filter(FNUM %in% EFM_sel_plots$PlotID)
# Calculate mean tree biomass growth
EFM_tree_agg <- EFM_tree %>% group_by(FNUM,AJ) %>% 
  summarise(mean_tree_biomass_growth_kgperyear=mean(Biomass_growth_kgperyear,na.rm=T),
            mean_tree_biomass_growth_kgperm2peryear=mean(Biomass_growth_kgperm2peryear,na.rm=T),
            mean_tree_biomass_kg=mean(Biomass_kg,na.rm=T),
            mean_dbh=mean(DBH,na.rm=T),
            mean_qmd=sqrt(sum(DBH**2)/nTrees)) %>% distinct(FNUM,AJ, .keep_all = TRUE) %>%
  ungroup() %>% 
  rename(PlotID=FNUM, Year=AJ) %>% mutate(logmean_qmd=log(mean_qmd)) %>%
  mutate(PlotID=as.character(PlotID),dataset="EFM")

# NFI - Swiss National Forest Inventories ####

# Species names
NFI_species_constant <- read.csv("~/GFDY/data/raw_obs/nfi/Species_names.csv")
str(NFI_species_constant)

# Stand data to get plot size
NFI_plot_census <- read.csv("~/GFDY/data/raw_obs/nfi/MIND_plot_census_20210121.csv")
str(NFI_plot_census)

## Tree-level data from NFI ####
NFI_tree <- read.csv("~/GFDY/data/raw_obs/nfi/NFI_tree_census.csv")
str(NFI_tree)
length(unique(NFI_tree$PLOTID)) 
length(unique(NFI_tree$TREEID)) 
# Create Year variable from CENSUS_DATE
NFI_tree <- NFI_tree %>% mutate(Year=str_sub(CENSUS_DATE, 7, 10)) %>% 
  mutate(Year=as.numeric(Year)) %>% relocate(Year, .after=CENSUS_DATE)
# Add species names
NFI_tree <- NFI_tree %>% left_join(NFI_species_constant) %>% left_join(NFI_plot_census[,c(1,2,4)]) %>% 
  relocate(SPECIES_NAME, .after=SPECIES_ID) %>%
  mutate(PlotArea_ha = PLOT_AREA_LARGE/10000)
# Filter tree with status = 1 (alive)
NFI_tree <- NFI_tree %>% filter(TREE_STATUS==1)
# Calculate growth in terms of DBH at tree level
NFI_tree <- NFI_tree %>% arrange(TREEID,PLOTID) %>% group_by(PLOTID,TREEID) %>%
  mutate(PeriodLength_years=Year-lag(Year)) %>% relocate(PeriodLength_years, .after=Year) %>% 
  mutate(Biomass_growth_kgperyear=(BIOMASS-lag(BIOMASS))/PeriodLength_years) %>% 
  ungroup() %>% 
  mutate(Biomass_growth_kgperhaperyear=Biomass_growth_kgperyear/PlotArea_ha) %>%
  mutate(Biomass_growth_kgperm2peryear=Biomass_growth_kgperhaperyear/10000) %>%
  group_by(PLOTID,Year) %>% mutate(nTrees=n()) %>% ungroup()
# Select same plots selected at stand level
NFI_tree <- NFI_tree %>% filter(PLOTID %in% NFI_sel_plots$PlotID)
# Calculate mean tree biomass growth
NFI_tree_agg <- NFI_tree %>% group_by(PLOTID,Year) %>% 
  summarise(mean_tree_biomass_growth_kgperyear=mean(Biomass_growth_kgperyear,na.rm=T),
            mean_tree_biomass_growth_kgperm2peryear=mean(Biomass_growth_kgperm2peryear,na.rm=T),
            mean_tree_biomass_kg=mean(BIOMASS,na.rm=T),
            mean_dbh=mean(DBH,na.rm=T),
            mean_qmd=sqrt(sum(DBH**2)/nTrees)) %>% distinct(PLOTID,Year, .keep_all = TRUE) %>%
  ungroup() %>% 
  rename(PlotID=PLOTID) %>% mutate(logmean_qmd=log(mean_qmd)) %>%
  mutate(PlotID=as.character(PlotID),dataset="NFI")

# NFR - Natural Forests Reserves NFR ####

# Plot area
NFR_plot_area <- read.csv("~/GFDY/data/raw_obs/nfr/NFR_plot_area.csv",sep=",")
str(NFR_plot_area)
# Join reserve (fg) and plot (tf) id in a new variable called PlotID as character
NFR_plot_area <- NFR_plot_area %>% mutate(fg = as.character(fg),FNUM = as.character(FNUM)) 
length(unique(NFR_plot_area$FNUM)) 

## Tree-level data from NFR ####
NFR_tree <- readRDS("~/GFDY/data/raw_obs/nfr/NFR_tree_data.RDS")
NFR_tree <- NFR_tree %>% rename(TreeID=BNR,TreeStatus=AHC,fg=FG) %>% mutate(FNUM = as.character(FNUM)) %>%
  left_join(NFR_plot_area[,c(1,6)])
str(NFR_tree)
length(unique(NFR_tree$FNUM))
length(unique(NFR_tree$TreeID))
# Select trees alive (1) and marked for thinning (2) (see pdf EFM for classes)
NFR_tree <- NFR_tree %>% #filter(TreeStatus==1|TreeStatus==2) %>%  
  mutate(Biomass_kg = Abovegroundmass_kg + Rootmassmass_kg)
# Calculate growth in terms of DBH at tree level
NFR_tree <- NFR_tree %>% arrange(TreeID,FNUM) %>% group_by(FNUM,TreeID) %>%
  mutate(PeriodLength_years=AJ-lag(AJ)) %>% relocate(PeriodLength_years, .after=AJ) %>% 
  mutate(Biomass_growth_kgperyear=(Biomass_kg-lag(Biomass_kg))/PeriodLength_years) %>%
  ungroup() %>% 
  mutate(Biomass_growth_kgperhaperyear=Biomass_growth_kgperyear/PlotArea_ha) %>%
  mutate(Biomass_growth_kgperm2peryear=Biomass_growth_kgperhaperyear/10000) %>%
  group_by(FNUM,AJ) %>% mutate(nTrees=n()) %>% ungroup()
# Select same plots selected at stand level
NFR_tree <- NFR_tree %>% filter(FNUM %in% NFR_sel_plots$PlotID)
# Calculate mean tree biomass growth
NFR_tree_agg <- NFR_tree %>% group_by(FNUM,AJ) %>% 
  summarise(mean_tree_biomass_growth_kgperyear=mean(Biomass_growth_kgperyear,na.rm=T),
            mean_tree_biomass_growth_kgperm2peryear=mean(Biomass_growth_kgperm2peryear,na.rm=T),
            mean_tree_biomass_kg=mean(Biomass_kg,na.rm=T),
            mean_dbh=mean(DBH,na.rm=T),
            mean_qmd=sqrt(sum(DBH**2)/nTrees)) %>% distinct(FNUM,AJ, .keep_all = TRUE)  %>%
  ungroup() %>% 
  rename(PlotID=FNUM, Year=AJ) %>% mutate(logmean_qmd=log(mean_qmd)) %>%
  mutate(PlotID=as.character(PlotID),dataset="NFR")

# Aggregated tree level data from EFM, NFI and NFR ####
aggTreeData <- bind_rows(EFM_tree_agg,NFI_tree_agg,NFR_tree_agg)
length(unique(aggTreeData$PlotID))

# save aggTreeData
save(aggTreeData, file = "~/GFDY/data/inputs_obs/aggTreeData75.RData")
save(aggTreeData, file = "~/GFDY/data/inputs_obs/aggTreeData55.RData")
save(aggTreeData, file = "~/GFDY/data/inputs_obs/aggTreeData90.RData")

# Trends in forest attributes ####
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75.RData")
load("~/GFDY/data/inputs_obs/aggTreeData75.RData")

# QMD
fit_qmd = lmer(QMD ~ Year + (1|PlotID) + (1|Species),
                data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(fit_qmd)
out <- summary(fit_qmd)
out$coefficients
r.squaredGLMM(fit_qmd)
plot(allEffects(fit_qmd))
plot_model(fit_qmd,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_qmd, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_qmd <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = Year, y = QMD), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,linewidth = .6, se=F) + 
  labs(x = "Year", y = "Quadratic Mean Diameter (QMD, cm)",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40))
fig_fit_qmd

# Stand density
fit_stden = lmer(Density ~ Year + (1|PlotID) + (1|Species),
               data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(fit_stden)
out <- summary(fit_stden)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_stden)
plot(allEffects(fit_stden))
plot_model(fit_stden,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_stden, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_stden <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = Year, y = Density), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Stand density (N, indiv ", ha^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(limits = c(0,4000),breaks = seq(0,4000,2000))
fig_fit_stden

# Stand biomass
fit_biomass = lmer(Biomass_Kg_m2 ~ Year + (1|PlotID) + (1|Species),
                 data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(fit_biomass)
out <- summary(fit_biomass)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_biomass)
plot(allEffects(fit_biomass))
plot_model(fit_biomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_biomass, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_biomass <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = Year, y = Biomass_Kg_m2), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Stand biomass (kg C ", m^-2, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(limits = c(0,101),breaks = seq(0,100,50))
fig_fit_biomass

# Stand net biomass change
fit_biomass_change = lmer(BiomassIncrement_Kg_m2_year ~ Year + (1|PlotID) + (1|Species),
                   data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(fit_biomass_change)
out <- summary(fit_biomass_change)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_biomass_change)
plot(allEffects(fit_biomass_change))
plot_model(fit_biomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_biomass_change, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_biomass_change <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = Year, y = BiomassIncrement_Kg_m2_year), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Net biomass change (kg C ", m^-2, " ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1960,2020),breaks = seq(1960,2020,30))
fig_fit_biomass_change

# Mean tree biomass
fit_treebiomass = lmer(mean_tree_biomass_kg ~ Year + (1|PlotID),
                          data = aggTreeData, na.action = "na.exclude")
summary(fit_treebiomass)
out <- summary(fit_treebiomass)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_treebiomass)
plot(allEffects(fit_treebiomass))
plot_model(fit_treebiomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_treebiomass, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_treebiomass <- ggplot() + 
  geom_point(data = aggTreeData, aes(x = Year, y = mean_tree_biomass_kg), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Mean tree biomass (kg C)")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(limits = c(0,4000),breaks = seq(0,4000,2000))
fig_fit_treebiomass

# Mean tree biomass change
fit_treebiomass_change = lmer(mean_tree_biomass_growth_kgperyear ~ Year + (1|PlotID),
                       data = aggTreeData, na.action = "na.exclude")
summary(fit_treebiomass_change)
out <- summary(fit_treebiomass_change)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_treebiomass_change)
plot(allEffects(fit_treebiomass_change))
plot_model(fit_treebiomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_treebiomass_change, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_treebiomass_change <- ggplot() + 
  geom_point(data = aggTreeData, aes(x = Year, y = mean_tree_biomass_growth_kgperyear), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Mean tree biomass change (kg C ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1960,2020),breaks = seq(1960,2020,30)) +
  scale_y_continuous(limits = c(-9,60),breaks = seq(0,60,30))
fig_fit_treebiomass_change

# Figure S2 ####
ffS2 <- fig_fit_qmd + fig_fit_stden + fig_fit_biomass +
  fig_fit_biomass_change + fig_fit_treebiomass + fig_fit_treebiomass_change +
  plot_layout(ncol = 3) + 
  #plot_annotation(tag_levels = list(c('a)', 'b)')))
  plot_annotation(tag_levels = 'A', tag_suffix = ")") #& 
#theme(plot.margin = unit(rep(0.13,4), "cm"))#+
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffS2
ggsave("~/GFDY/manuscript/figures/fig_S2.png", width = 10, height = 6, dpi=300)

# Table S3 - tree ####
load("~/GFDY/data/inputs_obs/aggTreeData75.RData")

aggTreeData %>% group_by(PlotID) %>% arrange(Year) %>% 
  filter(row_number()==1 | row_number()==n()) %>% arrange(PlotID) %>%
  mutate(change_mean_tree_biomass_kg = (mean_tree_biomass_kg - lag(mean_tree_biomass_kg))/lag(mean_tree_biomass_kg)*100) %>% 
  ungroup() %>% #group_by(dataset) %>%
  summarise(change_mean_tree_biomass_kg=mean(change_mean_tree_biomass_kg,na.rm=T))

aggTreeData %>% group_by(PlotID) %>% arrange(Year) %>% 
  filter(row_number()==2 | row_number()==n()) %>% arrange(PlotID) %>%
  mutate(change_mean_tree_biomass_growth_kgperyear = 
           (mean_tree_biomass_growth_kgperyear - lag(mean_tree_biomass_growth_kgperyear))/
           lag(mean_tree_biomass_growth_kgperyear)*100) %>% 
  mutate(change_mean_tree_biomass_growth_kgperm2peryear = 
           (mean_tree_biomass_growth_kgperm2peryear - lag(mean_tree_biomass_growth_kgperm2peryear))/
           lag(mean_tree_biomass_growth_kgperm2peryear)*100) %>% 
  ungroup() %>% #group_by(dataset) %>%
  summarise(change_mean_tree_biomass_growth_kgperyear=mean(change_mean_tree_biomass_growth_kgperyear,na.rm=T),
            change_mean_tree_biomass_growth_kgperm2peryear=mean(change_mean_tree_biomass_growth_kgperm2peryear,na.rm=T))
