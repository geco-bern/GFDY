# This script reads the tree-level data from the Swiss EFM, NFI and NFR plots
# selects only unmanaged plots and calculates the needed variables for the analysis to create the aggregated dataset
# This script generates the aggreagated tre-level dataset (summarizing) and the tree-level dataset
# This script calculates trends in forest attributes and creates Figure S2

# load packages
library(dplyr)
library(stringr)
library(lme4) 
library(lmerTest) 
library(MuMIn)
library(effects) 
library(ggeffects)
library(sjPlot)
library(ggplot2)
library(robustlmm)
library(patchwork)

# Read data ####

# read data at stand level
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen55out.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen90out.RData"))
length(unique(aggData_QMDbinsDen_out$PlotID))

## EFM ####
## EFM - Experimental Forest Management 

# Filter selected plots for EFM
EFM_sel_plots <- aggData_QMDbinsDen_out %>% filter(dataset=="EFM")
NFI_sel_plots <- aggData_QMDbinsDen_out %>% filter(dataset=="NFI")
NFR_sel_plots <- aggData_QMDbinsDen_out %>% filter(dataset=="NFR")

# Plot area
EFM_plot_area <- read.csv(paste0(here::here(), "/data/raw_obs/efm/EFM_plot_area.csv"))
str(EFM_plot_area)

# Tree-level data from EFM
EFM_tree_pre <- readRDS(paste0(here::here(), "/data/raw_obs/efm/EFM_tree_data.RDS"))
EFM_tree_6003 <- readRDS(paste0(here::here(), "/data/raw_obs/efm/EFM_tree_data6003.RDS"))
EFM_tree <- EFM_tree_pre %>% bind_rows(EFM_tree_6003) %>% filter(FNUM!=6003000) %>% 
  rename(TreeID=BNR,TreeStatus=AHC) %>% left_join(EFM_plot_area)
str(EFM_tree)
length(unique(EFM_tree$FNUM))
length(unique(EFM_tree$TreeID))
# Select trees alive (1) and marked for thinning (2) (see pdf EFM for classes)
EFM_tree <- EFM_tree %>% filter(TreeStatus==1) %>%  
  mutate(Biomass_kg = Abovegroundmass_kg + Rootmassmass_kg)
# Calculate growth in terms of biomass at tree level
EFM_tree <- EFM_tree %>% arrange(FNUM,TreeID) %>% group_by(FNUM,TreeID) %>%
  mutate(PeriodLength_years=AJ-lag(AJ)) %>% relocate(PeriodLength_years, .after=AJ) %>% 
  mutate(Biomass_growth_kgperyear=(Biomass_kg-lag(Biomass_kg))/PeriodLength_years) %>%
  mutate(Biomass_growth_kgperyear=ifelse(Biomass_growth_kgperyear>=0,Biomass_growth_kgperyear,0)) %>%
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

EFM_tree <- EFM_tree %>% select(FNUM, AJ, TreeID, Biomass_kg, DBH,
                                Biomass_growth_kgperyear, Biomass_growth_kgperm2peryear,nTrees) %>% 
  rename(PlotID=FNUM, Year=AJ) %>% mutate(PlotID=as.character(PlotID),dataset="EFM")

## NFI ####
## NFI - Swiss National Forest Inventories

# Species names
NFI_species_constant <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/Species_names.csv"))
str(NFI_species_constant)

# Stand data to get plot size
NFI_plot_census <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/MIND_plot_census_20210121.csv"))
str(NFI_plot_census)

## Tree-level data from NFI
NFI_tree <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/NFI_tree_census.csv"))
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
  mutate(Biomass_growth_kgperyear=ifelse(Biomass_growth_kgperyear>=0,Biomass_growth_kgperyear,0)) %>%
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

NFI_tree <- NFI_tree %>% select(PLOTID, Year, TREEID,BIOMASS, DBH,
                                Biomass_growth_kgperyear, Biomass_growth_kgperm2peryear) %>% 
  rename(PlotID=PLOTID, Biomass_kg=BIOMASS, TreeID=TREEID) %>% mutate(PlotID=as.character(PlotID),dataset="NFI")

## NFR ####
## NFR - Natural Forests Reserves

# Plot area
NFR_plot_area <- read.csv(paste0(here::here(), "/data/raw_obs/nfr/NFR_plot_area.csv"),sep=",")
str(NFR_plot_area)
# Join reserve (fg) and plot (tf) id in a new variable called PlotID as character
NFR_plot_area <- NFR_plot_area %>% mutate(fg = as.character(fg),FNUM = as.character(FNUM)) 
length(unique(NFR_plot_area$FNUM)) 

## Tree-level data from NFR
NFR_tree <- readRDS(paste0(here::here(), "/data/raw_obs/nfr/NFR_tree_data.RDS"))
NFR_tree <- NFR_tree %>% rename(TreeID=BNR,TreeStatus=AHC,fg=FG) %>% mutate(FNUM = as.character(FNUM)) %>%
  left_join(NFR_plot_area[,c(1,6)])
str(NFR_tree)
length(unique(NFR_tree$FNUM))
length(unique(NFR_tree$TreeID))
# Select trees alive (1) and marked for thinning (2) (see pdf EFM for classes)
NFR_tree <- NFR_tree %>% filter(TreeStatus==1) %>%  
  mutate(Biomass_kg = Abovegroundmass_kg + Rootmassmass_kg) %>%
  filter(FNUM!=7026001|TreeID!=416) # remove for measure error
# Calculate growth in terms of DBH at tree level
NFR_tree <- NFR_tree %>% arrange(TreeID,FNUM) %>% group_by(FNUM,TreeID) %>%
  mutate(PeriodLength_years=AJ-lag(AJ)) %>% relocate(PeriodLength_years, .after=AJ) %>% 
  mutate(Biomass_growth_kgperyear=(Biomass_kg-lag(Biomass_kg))/PeriodLength_years) %>%
  mutate(Biomass_growth_kgperyear=ifelse(Biomass_growth_kgperyear>=0,Biomass_growth_kgperyear,0)) %>%
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

NFR_tree <- NFR_tree %>% select(FNUM, AJ, TreeID, Biomass_kg, DBH,
                                Biomass_growth_kgperyear, Biomass_growth_kgperm2peryear) %>% 
  rename(PlotID=FNUM, Year=AJ) %>% mutate(PlotID=as.character(PlotID),dataset="NFR") 

# Create datasets ####

## Aggregated ####
# 1) Aggregated tree level data from EFM, NFI and NFR
aggTreeData <- bind_rows(EFM_tree_agg,NFI_tree_agg,NFR_tree_agg)
aggTreeData <- aggTreeData %>% group_by(PlotID) %>%
  mutate(mean_dbh_firstcensus = first(mean_dbh)) %>% 
  relocate(mean_dbh_firstcensus, .after=mean_dbh) %>% ungroup() 
length(unique(aggTreeData$PlotID))

# save aggTreeData
save(aggTreeData, file = paste0(here::here(), "/data/inputs_obs/aggTreeData75.RData"))
#save(aggTreeData, file = paste0(here::here(), "/data/inputs_obs/aggTreeData55.RData"))
#save(aggTreeData, file = paste0(here::here(), "/data/inputs_obs/aggTreeData90.RData"))

## Tree-level ####
# 2) Tree-level data from EFM, NFI and NFR
TreeData <- bind_rows(EFM_tree,NFI_tree,NFR_tree)
TreeData <- TreeData %>% group_by(PlotID, TreeID) %>%
  mutate(DBH_firstcensus = first(DBH)) %>% relocate(DBH_firstcensus, .after=DBH) %>% ungroup()
length(unique(TreeData$PlotID))

# save TreeData
save(TreeData, file = paste0(here::here(), "/data/inputs_obs/TreeData75.RData"))
#save(TreeData, file = paste0(here::here(), "/data/inputs_obs/TreeData55.RData"))
#save(TreeData, file = paste0(here::here(), "/data/inputs_obs/TreeData90.RData"))

# Calculate trends ####
# Trends in forest attributes
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
length(unique(aggData_QMDbinsDen_out$PlotID))
load(paste0(here::here(), "/data/inputs_obs/aggTreeData75.RData"))
length(unique(aggTreeData$PlotID))
load(paste0(here::here(), "/data/inputs_obs/TreeData75.RData"))
length(unique(TreeData$PlotID))

aggData_QMDbinsDen_out <- aggData_QMDbinsDen_out %>% group_by(PlotID) %>%
  mutate(DBH_firstcensus = first(DBH)) %>% relocate(DBH_firstcensus, .after=DBH) %>% ungroup()

# QMD
fit_qmd = lmer(QMD ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                data = aggData_QMDbinsDen_out, na.action = "na.exclude")
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
  geom_point(data = aggData_QMDbinsDen_out, aes(x = Year, y = QMD), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,linewidth = .6, se=F) + 
  labs(x = "Year", y = "Quadratic Mean Diameter (QMD, cm)",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40))
fig_fit_qmd

# Robust regression
fit_qmd_robust = rlmer(QMD ~ Year + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                       data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_qmd_robust)
plot_model(fit_qmd_robust,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(fit_qmd)))
# get coefficients from robust model to extract t-values
coefs.robust <- coef(summary(fit_qmd_robust))
# calculate p-values based on robust t-values and non-robust approx. DFs
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$df, lower=FALSE)
p.values

# Stand density
fit_stden = lmer(Density ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                 data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_stden)
out <- summary(fit_stden)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_stden)
plot(allEffects(fit_stden))
plot_model(fit_stden,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
plot_model(fit_stden,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("DBH_firstcensus"))
pred <- ggpredict(fit_stden, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_stden <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen_out, aes(x = Year, y = Density), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Stand density (N, indiv ", ha^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(limits = c(0,4000),breaks = seq(0,4000,2000))
fig_fit_stden

# Robust regression
fit_stden_robust = rlmer(Density ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                         data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_stden_robust)
plot_model(fit_stden_robust,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(fit_stden)))
# get coefficients from robust model to extract t-values
coefs.robust <- coef(summary(fit_stden_robust))
# calculate p-values based on robust t-values and non-robust approx. DFs
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$df, lower=FALSE)
p.values

# Stand biomass
fit_biomass = lmer(Biomass_Kg_m2 ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                   data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_biomass)
out <- summary(fit_biomass)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_biomass)
plot(allEffects(fit_biomass))
plot_model(fit_biomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
plot_model(fit_biomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("DBH_firstcensus"))
pred <- ggpredict(fit_biomass, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_biomass <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen_out, aes(x = Year, y = Biomass_Kg_m2), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Stand biomass (kg C ", m^-2, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(limits = c(0,101),breaks = seq(0,100,50))
fig_fit_biomass

# Robust regression
fit_biomass_robust = rlmer(Biomass_Kg_m2 ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                           data = aggData_QMDbinsDen_out, na.action = "na.omit")
summary(fit_biomass_robust)
# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(fit_biomass)))
# get coefficients from robust model to extract t-values
coefs.robust <- coef(summary(fit_biomass_robust))
# calculate p-values based on robust t-values and non-robust approx. DFs
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$df, lower=FALSE)
p.values

# Net stand biomass change
fit_biomass_change = lmer(BiomassIncrement_Kg_m2_year ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species)
                          + (1|years_since_management_bins),
                          data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_biomass_change)
out <- summary(fit_biomass_change)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_biomass_change)
plot(allEffects(fit_biomass_change))
plot_model(fit_biomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
plot_model(fit_biomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("DBH_firstcensus"))
pred <- ggpredict(fit_biomass_change, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_biomass_change <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen_out, aes(x = Year, y = BiomassIncrement_Kg_m2_year), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Net biomass change (kg C ", m^-2, " ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1960,2020),breaks = seq(1960,2020,30))
fig_fit_biomass_change

# Robust regression ...
fit_biomass_change_robust = rlmer(BiomassIncrement_Kg_m2_year ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                           data = aggData_QMDbinsDen_out, na.action = "na.omit")
summary(fit_biomass_change_robust)
# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(fit_biomass_change)))
# get coefficients from robust model to extract t-values
coefs.robust <- coef(summary(fit_biomass_change_robust))
# calculate p-values based on robust t-values and non-robust approx. DFs
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$df, lower=FALSE)
p.values
plot_model(fit_biomass_change_robust,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))

# ... or Remove outliers
residGrowth <- residuals(fit_biomass_change)
rowindex_outliers <- as.integer(names(boxplot.stats(residGrowth, coef = 3.5)$out))
aggData_QMDbinsDen_out <- aggData_QMDbinsDen_out %>% 
  mutate(rowindex = dplyr::row_number()) %>%
  mutate(outlier = rowindex %in% rowindex_outliers) 
aggData_QMDbinsDen_out_out <- aggData_QMDbinsDen_out |> filter(outlier==FALSE)
aggData_QMDbinsDen_out |> 
  ggplot(aes(x = Year, y = BiomassIncrement_Kg_m2_year, color = outlier)) + 
  geom_point()
fit_biomass_change_out = lmer(BiomassIncrement_Kg_m2_year ~ Year + DBH_firstcensus + (1|PlotID) + (1|Species) + (1|years_since_management_bins),
                          data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_biomass_change_out)

# Mean tree biomass

# Agg tree
fit_aggtreebiomass = lmer(mean_tree_biomass_kg ~ Year + mean_dbh_firstcensus + (1|PlotID),
                       data = aggTreeData, na.action = "na.exclude")
summary(fit_aggtreebiomass)
out <- summary(fit_aggtreebiomass)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_aggtreebiomass)
plot(allEffects(fit_aggtreebiomass))
plot_model(fit_aggtreebiomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_aggtreebiomass, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_aggtreebiomass <- ggplot() + 
  geom_point(data = aggTreeData, aes(x = Year, y = mean_tree_biomass_kg), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Mean tree biomass (kg C)")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(limits = c(0,4000),breaks = seq(0,4000,2000))
fig_fit_aggtreebiomass

# Tree
fit_treebiomass = lmer(Biomass_kg ~ Year + DBH_firstcensus + (1|PlotID/TreeID),
                       data = TreeData, na.action = "na.exclude")
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
  geom_point(data = TreeData, aes(x = Year, y = Biomass_kg), alpha=0.5, size = 1.5,col="black",shape=16) + 
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = F,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Tree biomass (kg C)")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1930,2020),breaks = seq(1940,2020,40)) +
  scale_y_continuous(breaks = seq(0,15000,7500))
fig_fit_treebiomass

# Mean tree biomass change
#aggTreeData00 <- aggTreeData %>% left_join(aggData_QMDbinsDen_out[,c(1,2,11,13)])

# Agg tree
fit_aggtreebiomass_change = lmer(mean_tree_biomass_growth_kgperyear ~ Year + mean_dbh_firstcensus + 
                                   (1|PlotID),
                              data = aggTreeData, na.action = "na.exclude")
summary(fit_aggtreebiomass_change)
out <- summary(fit_aggtreebiomass_change)
out$coefficients
formatC(out$coefficients, format = "e")
r.squaredGLMM(fit_aggtreebiomass_change)
plot(allEffects(fit_aggtreebiomass_change))
plot_model(fit_aggtreebiomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
pred <- ggpredict(fit_aggtreebiomass_change, terms = c("Year"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
fig_fit_aggtreebiomass_change <- ggplot() + 
  geom_point(data = aggTreeData, aes(x = Year, y = mean_tree_biomass_growth_kgperyear), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Mean tree biomass change (kg C ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1960,2020),breaks = seq(1960,2020,30)) +
  scale_y_continuous(breaks = seq(0,60,30))
fig_fit_aggtreebiomass_change

# Tree
fit_treebiomass_change = lmer(Biomass_growth_kgperyear ~ Year + DBH_firstcensus + (1|PlotID/TreeID),
                              data = subset(TreeData,Biomass_growth_kgperyear>=0), na.action = "na.exclude")
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
  geom_point(data = TreeData, aes(x = Year, y = Biomass_growth_kgperyear), alpha=0.5, size = 1.5,col="black",shape=16) +  
  geom_ribbon(data = preddata, aes(x = x, y = predicted,ymin=conf.low,ymax=conf.high),fill="blue",alpha=.15,show.legend=T) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted), color="blue",fullrange = T,size = .6, se=F) + 
  labs(x = "Year", y = expression(paste("Tree biomass change (kg C ", yr^-1, ") ")),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     plot.title = element_text(size = 8)) +
  scale_x_continuous(limits = c(1960,2020),breaks = seq(1960,2020,30)) +
  scale_y_continuous(limits = c(0,600),breaks = seq(0,600,300))
fig_fit_treebiomass_change

# Figure S2 ####
fig_S2 <- fig_fit_qmd + fig_fit_stden + 
  fig_fit_biomass + fig_fit_biomass_change + 
  fig_fit_aggtreebiomass + fig_fit_aggtreebiomass_change +
  #fig_fit_treebiomass + fig_fit_treebiomass_change +
  plot_layout(ncol = 2) + 
  #plot_annotation(tag_levels = list(c('a)', 'b)')))
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  theme(plot.tag = element_text(size = 12)) #& 
#theme(plot.margin = unit(rep(0.13,4), "cm"))#+
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
fig_S2
ggsave(paste0(here::here(), "/manuscript/figures/fig_S2.png"), width = 7.5, height = 10, dpi=300)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S2.pdf"), width = 7.5, height = 10, dpi=300)
