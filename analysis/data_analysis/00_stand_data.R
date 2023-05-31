# This script reads the data from the Swiss EFM, NFI and NFR plots, selects only unmanaged plots and calculates the needed variables for the analysis
# This script also calculates the QMD bins to select those plots with higher density, considered subjected to self-thinning.

# load packages
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyr)
library(stringr)
library(vegan)
library(viridis)

# EFM - Experimental Forest Management ####

## Stand-level data from EFM ####

# Plot characteristics
EFM_plots_metadata <- read.csv(paste0(here::here(), "/data/raw_obs/efm/VFL_LISTmanual.csv"))
str(EFM_plots_metadata)

# Plot area
EFM_plot_area <- read.csv(paste0(here::here(), "/data/raw_obs/efm/EFM_plot_area.csv"))
str(EFM_plot_area)

# Plot area
EFM_plot_locations <- read.csv(paste0(here::here(), "/data/raw_obs/efm/efm_plot_locations.csv"))
str(EFM_plot_locations)

# Last management intervention
EFM_last_intervention <- read.csv(paste0(here::here(), "/data/raw_obs/efm/EFM_last_intervention.csv"))
str(EFM_last_intervention)

# Climate
EFM_climate <- readRDS(paste0(here::here(), "/data/raw_obs/efm/ClimateData_EFM_Monthly.RDS")) # original prep in cm!
EFM_climate_plot <- EFM_climate %>% mutate(Prcp=Prcp*10) %>% group_by(FNUM,year) %>% 
  summarise(temp=mean(Tave, na.rm=T),prec=sum(Prcp)) %>% ungroup() %>%
  group_by(FNUM) %>% summarise(temp=mean(temp, na.rm=T),prec=mean(prec))

# Stand level data (per plot, year and species)
# All the plots in this dataset are unmanaged.
EFM_stand_pre <- readRDS(paste0(here::here(), "/data/raw_obs/efm/EFM_stand_data.RDS")) 
EFM_stand_6003 <- readRDS(paste0(here::here(), "/data/raw_obs/efm/EFM_stand_data6003.RDS")) 
EFM_stand <- EFM_stand_pre %>% bind_rows(EFM_stand_6003) %>% filter(FNUM!=6003000)
str(EFM_stand)
length(unique(EFM_stand$FNUM))
# Join Stand level data with plot metadata
EFM_stand <- EFM_stand %>% left_join(EFM_last_intervention) %>% 
  left_join(EFM_plot_area) %>% left_join(EFM_plots_metadata[,c(4,8)]) %>%
  left_join(EFM_plot_locations) %>% left_join(EFM_climate_plot) %>%
  rename(elevation=HUM, longitude=lon, latitude=lat)
# Create new variables for linear mixed models at the stand level
EFM_stand <- EFM_stand %>% mutate(QMD=sqrt(BasalAreaAHC1_2_m2perha/(0.0000785*TreesPerHectareAHC1_2))) %>%
  mutate(logQMD=log(QMD)) %>% 
  mutate(BiomassKgperha=(AbovegroundAHC1_2_Mgperha+RootmassAHC1_2_Mgperha)*1000) %>%
  mutate(BiomassIncrement_KgperhaperPeriod=(AbovegroundIncrement_MgperhaperPeriod+RootmassIncrement_MgperhaperPeriod)*1000) %>%
  mutate(BiomassIncrement_KgperhaperYear=BiomassIncrement_KgperhaperPeriod/PeriodLength_years) %>%
  mutate(logTreesPerHectareAHC1_2=log(TreesPerHectareAHC1_2)) %>% group_by(FNUM,BA) %>%
  mutate(interval=AJ-lag(AJ)) %>% ungroup() %>% 
  relocate(first_measurement,.after=AJ) %>% 
  relocate(year_last_intervention,.after=first_measurement) %>% 
  mutate(years_since_management=AJ-year_last_intervention) %>%
  relocate(years_since_management,.after=year_last_intervention)
# Select Stand level data for all species: "All species combined"
EFM_stand_Allsps <- EFM_stand %>% filter(Latin =="All species combined")
length(unique(EFM_stand_Allsps$FNUM))
sort(table(EFM_stand_Allsps$FNUM))
# Select plots with minimum 3 census
EFM_stand_Allsps <- EFM_stand_Allsps %>%
  group_by(FNUM) %>% mutate(n_census=n()) %>% ungroup() %>% filter(n_census>=3)
length(unique(EFM_stand_Allsps$FNUM))
sort(table(EFM_stand_Allsps$FNUM))
# Identify which species dominates in each site (FNUM) and year (AJ)
EFM_stand_Bysps <- EFM_stand %>% filter(Latin !="All species combined")
length(unique(EFM_stand_Bysps$FNUM))
dominant_speciesEFM <- EFM_stand_Bysps %>% group_by(FNUM, AJ) %>% 
  top_n(1, BasalAreaAHC1_2_m2perha) %>% rename(Species=Latin)
# Add dominant species per plot to EFM_stand_Allsps
EFM_stand_Allsps <- EFM_stand_Allsps %>% left_join(dominant_speciesEFM[,c(1,3,4)])
length(unique(EFM_stand_Allsps$FNUM))
sort(table(EFM_stand_Allsps$FNUM))
sort(unique(EFM_stand_Allsps$PlotArea_ha))
EFM_stand_Allsps %>% group_by(FNUM) %>% arrange(AJ) %>% 
  filter(row_number()==n()) %>% ungroup() %>% 
  filter(!grepl('6003', FNUM)) %>% 
  summarise(mean=mean(years_since_management,na.rm=T),sd=sd(years_since_management,na.rm=T))
str(EFM_stand_Allsps)

# NFI - Swiss National Forest Inventories ####

## Stand-level data from NFI ####

# Plot characteristics
NFI_plot_constant <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/NFI_plot_constant.csv"))
str(NFI_plot_constant)

# Species names
NFI_species_constant <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/Species_names.csv"))
str(NFI_species_constant)

# Species names
NFI_climate <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/MIND_plot_annual_temp_precip.csv"),sep=";")
str(NFI_climate)
NFI_climate_plot <- NFI_climate %>% 
  group_by(PLOTID) %>% summarise(temp=mean(TTAVG, na.rm=T),prec=mean(PRSUM,na.rm=T))

# Stand-level data
#The plots included in this dataset are managed and unmanaged.
#The filter of unmanaged plots need to be done using the following variables: 
#LETZTENU variable establishes the number of years since the last management intervention. f it is known that no intervention has been carried out on this sample area for a very long time or that it is almost certain that no intervention has ever been carried out, LETZTENU is given the value=999.
#MANAGEMENT_INDICATOR variable establishes No treatment (kein Eingriff) as ID 1. 
#The variable 'Management_indicator' covers treatments since the last inventory (for example, a plot with 'census_id' == 250 and 'management_indicator' == 1 has not been managed between NFI1 (census_id == 150) and NFI2 (census_id == 250)).
NFI_plot_census <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/MIND_plot_census_20210121.csv"))
str(NFI_plot_census)
length(unique(NFI_plot_census$PLOTID))
# Join with NFI_plot_constant for elevation and coordinates
NFI_plot_census <- NFI_plot_census %>% left_join(NFI_plot_constant[,c(1,2,3,6)]) %>% 
  left_join(NFI_climate_plot) %>% 
  rename(elevation=ELEVATION, longitude=LONGITUDE, latitude=LATITUDE)
# Create Year variable from CENSUS_DATE
NFI_plot_census <- NFI_plot_census %>% mutate(Year=str_sub(CENSUS_DATE, 7, 10)) %>% 
  mutate(Year=as.numeric(Year)) %>% relocate(Year, .after=CENSUS_DATE)
# Remove two plots from NFI with extreme values of biomass increment
NFI_plot_census <- NFI_plot_census %>% filter(!(PLOTID==149566|PLOTID==146688|PLOTID==151587|PLOTID==142296|PLOTID==139723)) 
# Create PeriodLength_years variable 
NFI_plot_census <- NFI_plot_census %>% arrange(PLOTID) %>% group_by(PLOTID) %>%
  mutate(PeriodLength_years=Year-lag(Year)) %>% relocate(PeriodLength_years, .after=Year) #%>% mutate(BiomassIncrement_Kg_ha_period=BIOMASS_VPPS-lag(BIOMASS_VPPS)) %>% relocate(BiomassIncrement_Kg_ha_period, .after=BIOMASS_VPPS)
# Create new variables
NFI_plot_census <- NFI_plot_census %>% mutate(QMD=sqrt(BASAL_AREA_HA/(0.0000785*NPH))) %>%
  mutate(logQMD=log(QMD)) %>% 
  rename(Biomass_Kg_ha=BIOMASS_VPPS) %>% 
  filter(Biomass_Kg_ha>=0) %>%
  filter(NPH>0) %>%
  mutate(BiomassIncrement_Kg_ha_year=BIOMASS_INC_VPPS_TOTAL/PeriodLength_years) %>%
  mutate(logNPH=log(NPH)) %>%
  mutate(PlotArea_ha = PLOT_AREA_LARGE/10000)
# Select 1) plots with years_since_management (LETZTENU) >= 70
# Filter unmanaged plots using the LETZTENU variable: From Poltier et al.: 40 to 70 years since last management
NFI_plot_census <- NFI_plot_census %>% filter(LETZTENU>=70) 
# Select 2) plots with minimum 3 census
# Filter using the MANAGEMENT_INDICATOR variable and ensuring that the plot have at least 3 census 
# (SUM_CENSUSID needs to be 750(=150+250+350) or 1050(=250+350+450) or 1200(=150+250+350+450)). 
# we use this filtering instead of the n_census=n() to get only continuous censuses (since management in the NFI can happen also between censuses, what does not happen in EFM and NFR)
NFI_plot_census <- NFI_plot_census %>% #filter(MANAGEMENT_INDICATOR==1|MANAGEMENT_INDICATOR==-1) %>%
  group_by(PLOTID) %>% mutate(SUM_CENSUSID = sum(CENSUSID)) %>% filter(SUM_CENSUSID==750|SUM_CENSUSID==1050|SUM_CENSUSID==1200)
length(unique(NFI_plot_census$PLOTID))
sort(table(NFI_plot_census$PLOTID))
## calculate stand-level for each species from tree-level data
NFI_tree_census <- read.csv(paste0(here::here(), "/data/raw_obs/nfi/NFI_tree_census.csv"))
# Select same plots selected at stand level
NFI_tree_census <- NFI_tree_census %>% filter(PLOTID %in% NFI_plot_census$PLOTID)
# Create Year variable from CENSUS_DATE
NFI_tree_census <- NFI_tree_census %>% mutate(Year=str_sub(CENSUS_DATE, 7, 10)) %>% 
  mutate(Year=as.numeric(Year)) %>% relocate(Year, .after=CENSUS_DATE)
# Add species names
NFI_tree_census <- NFI_tree_census %>% left_join(NFI_species_constant) 
# Filter tree with status = 1 alive
NFI_tree_census <- NFI_tree_census %>% filter(TREE_STATUS==1)
# Add number of trees per plotID, censusID and SPECIES_ID (n) and multiply by REPRESENTATION to estimate tree density
NFI_tree_census <- NFI_tree_census %>%  group_by(PLOTID,CENSUSID,SPECIES_ID) %>%  
  add_tally() %>% mutate(Tree_density_perha=REPRESENTATION*n) %>% ungroup() %>% mutate(BA=pi*(DBH*0.01)^2/4) %>% 
  mutate(BA_perha=BA*Tree_density_perha)
# Aggregate by plot and species
NFI_tree_agg <- NFI_tree_census %>% group_by(PLOTID,Year,SPECIES_NAME) %>% 
  summarise(Total_BA_perha=sum(BA_perha),Total_Tree_density_perha = sum(Tree_density_perha)) %>%
  ungroup()
# Identify which species dominates in each site (FNUM) and year (AJ)
dominant_speciesNFI <- NFI_tree_agg %>% group_by(PLOTID, Year) %>% 
  top_n(1, Total_BA_perha) %>% rename(Species=SPECIES_NAME) %>%
  ungroup()
# Add dominant species per plot to NFI_plot_census
NFI_plot_census <- NFI_plot_census %>% left_join(dominant_speciesNFI[,c(1,2,3)]) %>%
  ungroup()
length(unique(NFI_plot_census$PLOTID))
sort(table(NFI_plot_census$PLOTID))
str(NFI_plot_census)

# NFR - Natural Forests Reserves NFR ####

## Stand-level data from NFR ####

# Metadata
NFR_Metadata <- read.csv(paste0(here::here(), "/data/raw_obs/nfr/NFR_metadata.csv"))
NFR_Metadata <- NFR_Metadata %>% mutate(fg = as.character(fg)) %>%
  select(fg,lat,long,ele,temp,precip) %>% distinct(fg, .keep_all = TRUE)
str(NFR_Metadata)

# Plot area
NFR_plot_area <- read.csv(paste0(here::here(), "/data/raw_obs/nfr/NFR_plot_area.csv"),sep=",")
str(NFR_plot_area)
NFR_plot_area <- NFR_plot_area %>% mutate(fg = as.character(fg),FNUM = as.character(FNUM)) %>%
  left_join(NFR_Metadata)
length(unique(NFR_plot_area$FNUM)) # Plot
length(unique(NFR_plot_area$fg)) # Plot

# Last management intervention
NFR_last_intervention <- read.csv(paste0(here::here(), "/data/raw_obs/nfr/NFR_last_intervention.csv"))
NFR_last_intervention <- NFR_last_intervention %>% mutate(fg=as.character(fg))
str(NFR_last_intervention)
length(unique(NFR_last_intervention$fg))

# Stand level data (per plot, year and species)
# All the plots in this dataset are unmanaged.
NFR_stand <- readRDS(paste0(here::here(), "/data/raw_obs/nfr/NFR_stand_data.RDS")) # 291 plots From David Forrester data
str(NFR_stand)
length(unique(NFR_stand$FNUM))
NFR_stand <- NFR_stand %>% mutate(FNUM=as.character(FNUM)) %>% select(-FNUM2)
# Join Stand level data with plot are
NFR_stand <- NFR_stand %>% left_join(NFR_plot_area) %>% 
  rename(elevation=ele, longitude=long, latitude=lat)
# Join Stand level data with management intervention
NFR_stand <- NFR_stand %>% left_join(NFR_last_intervention[,c(1,5,6)])
# Remove FNUM with TreesPerHectareAHC1_2 == 0 or NA
NFR_stand <- NFR_stand %>% filter(TreesPerHectareAHC1_2!=0) %>% filter(is.na(TreesPerHectareAHC1_2)==FALSE)
# Remove FNUM because it has a disproportionately high BA and Biomass increment, as suggested by David Forrester
NFR_stand <- NFR_stand %>% filter(FNUM!=7007028,FNUM!=7005006,FNUM!=7005003,FNUM!=7005002,FNUM!=7001001,FNUM!=7001002,FNUM!=7001003,FNUM!=7002003)
# Create new variables for linear mixed models at the stand level
NFR_stand <- NFR_stand %>% mutate(QMD=sqrt(BasalAreaAHC1_2_m2perha/(0.0000785*TreesPerHectareAHC1_2))) %>%
  mutate(logQMD=log(QMD)) %>% 
  mutate(BiomassKgperha=(AbovegroundAHC1_2_Mgperha+RootmassAHC1_2_Mgperha)*1000) %>%
  mutate(BiomassIncrement_KgperhaperPeriod=(AbovegroundIncrement_MgperhaperPeriod+RootmassIncrement_MgperhaperPeriod)*1000) %>%
  mutate(BiomassIncrement_KgperhaperYear=BiomassIncrement_KgperhaperPeriod/PeriodLength_years) %>%
  mutate(logTreesPerHectareAHC1_2=log(TreesPerHectareAHC1_2)) 
NFR_stand <- NFR_stand %>% relocate(first_measurement,.after=AJ) %>% 
  relocate(year_last_intervention,.after=first_measurement) %>% 
  mutate(years_since_management=AJ-year_last_intervention) %>%
  relocate(years_since_management,.after=year_last_intervention) 
# Select Stand level data for all species: "All species combined"
NFR_stand_Allsps <- NFR_stand %>% filter(Latin =="All species combined")
length(unique(NFR_stand_Allsps$FNUM))
sort(table(NFR_stand_Allsps$FNUM))
# Select plots with minimum 3 census
NFR_stand_Allsps <- NFR_stand_Allsps %>%
  group_by(FNUM) %>% mutate(n_census=n()) %>% ungroup() %>% filter(n_census>=3)
length(unique(NFR_stand_Allsps$FNUM))
sort(table(NFR_stand_Allsps$FNUM))
# Identify which species dominates in each site (FNUM) and year (AJ)
NFR_stand_Bysps <- NFR_stand %>% filter(Latin !="All species combined")
sort(table(NFR_stand_Bysps$FNUM))
length(unique(NFR_stand_Bysps$FNUM))
dominant_speciesNFR <- NFR_stand_Bysps %>% group_by(FNUM, AJ) %>% 
  top_n(1, BasalAreaAHC1_2_m2perha) %>% rename(Species=Latin)
# Add dominant species per plot to NFR_stand_Allsps
NFR_stand_Allsps <- NFR_stand_Allsps %>% left_join(dominant_speciesNFR[,c(1,3,4)])
length(unique(NFR_stand_Allsps$Species))
length(unique(NFR_stand_Allsps$FNUM))
sort(table(NFR_stand_Allsps$FNUM))
str(NFR_stand_Allsps)

# Aggregated stand level data from EFM, NFI and NFR ####
EFM_stand_agg <- EFM_stand_Allsps %>% dplyr::select(c(FNUM,AJ,DBHqAHC1_2_cm,QMD,logQMD,BiomassKgperha,TreesPerHectareAHC1_2,
                                               logTreesPerHectareAHC1_2,BiomassIncrement_KgperhaperYear,Species,
                                               PlotArea_ha,years_since_management,elevation,longitude,latitude,temp,prec,PeriodLength_years)) %>% 
  rename(PlotID=FNUM, Year=AJ, DBH =DBHqAHC1_2_cm, Density=TreesPerHectareAHC1_2,logDensity=logTreesPerHectareAHC1_2,
         Biomass_Kg_ha=BiomassKgperha,BiomassIncrement_Kg_ha_year=BiomassIncrement_KgperhaperYear) %>% mutate(dataset="EFM")
length(unique(EFM_stand_agg$PlotID))
sort(table(EFM_stand_agg$PlotID))

NFI_stand_agg <- NFI_plot_census %>% dplyr::select(c(PLOTID,Year,MEAN_DBH_HA, QMD,logQMD,Biomass_Kg_ha,NPH,logNPH,
                                              BiomassIncrement_Kg_ha_year,Species,PlotArea_ha,LETZTENU,elevation,longitude,latitude,temp,prec,PeriodLength_years)) %>% 
  rename(PlotID=PLOTID, DBH=MEAN_DBH_HA, Density=NPH, logDensity=logNPH,years_since_management=LETZTENU) %>% mutate(dataset="NFI")
length(unique(NFI_stand_agg$PlotID))
sort(table(NFI_stand_agg$PlotID))

NFR_stand_agg <- NFR_stand_Allsps %>% dplyr::select(c(FNUM,AJ,DBHqAHC1_2_cm,QMD,logQMD,BiomassKgperha,TreesPerHectareAHC1_2,
                                               logTreesPerHectareAHC1_2,BiomassIncrement_KgperhaperYear,Species,
                                               PlotArea_ha,years_since_management,elevation,longitude,latitude,temp,precip,PeriodLength_years)) %>% 
  rename(PlotID=FNUM, Year=AJ, DBH =DBHqAHC1_2_cm, Density=TreesPerHectareAHC1_2,logDensity=logTreesPerHectareAHC1_2,
         Biomass_Kg_ha=BiomassKgperha,BiomassIncrement_Kg_ha_year=BiomassIncrement_KgperhaperYear,prec=precip) %>% mutate(dataset="NFR")
length(unique(NFR_stand_agg$PlotID))
sort(table(NFR_stand_agg$PlotID))

# Join datasets
aggStandData <- rbind(EFM_stand_agg,NFI_stand_agg,NFR_stand_agg)
# Convert NPP from BiomassIncrement_Kg_ha_year to BiomassIncrement_Kg_m2_year
aggStandData <- aggStandData %>% 
  mutate(BiomassIncrement_Kg_m2_year=BiomassIncrement_Kg_ha_year/10000) %>%
  mutate(Biomass_Kg_m2=Biomass_Kg_ha/10000) %>% 
  mutate(years_since_management_bins = cut(years_since_management, 
        breaks = c(0,25,50,75,100,125,1000),include.lowest = T, right = F)) 
# save
save(aggStandData, file = paste0(here::here(), "/data/inputs_obs/aggStandData.RData"))
length(unique(aggStandData$PlotID))
sort(table(aggStandData$PlotID))
str(aggStandData)

# Select upper quantile by QMD bins ####
# calculate the QMD bins to select those plots with higher density 
load(paste0(here::here(), "/data/inputs_obs/aggStandData.RData"))
aggData_QMDbins <- aggStandData %>% mutate(QMD_bins = cut(QMD, breaks = 30)) 
sort(unique(aggData_QMDbins$QMD_bins))
length(unique(aggData_QMDbins$PlotID))

# Highlight the tails of the distributions
ggplot(aggData_QMDbins, aes(x = Density, y = QMD_bins, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.25, 0.5, 0.75, 0.95)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#0000FFA0", "#A0A0A0A0","#0000FFA0","#FF0000A0"),
                    labels = c("(0, 0.25]", "(0.25, 0.5]", "(0.5, 0.75]", "(0.75, 0.95]", "(0.95, 1]")) + labs(x="Tree density")

# Select from each QMD bins the number of plots with higher density
# Includes a sensitivity analysis using quantiles 0.55, 0.75 and 0.90
valueQuantile = 0.90

quantileX <- aggData_QMDbins %>% group_by(QMD_bins) %>% summarise(quantile(Density, c(valueQuantile))) 
max(quantileX$`quantile(Density, c(valueQuantile))`)
aggData_QMDbins <- aggData_QMDbins %>% left_join(quantileX)
aggData_QMDbinsDen <- aggData_QMDbins %>% filter(Density>=`quantile(Density, c(valueQuantile))`)
aggData_QMDbinsRest <- aggData_QMDbins %>% filter(Density<`quantile(Density, c(valueQuantile))`)

ggplot(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity)) + geom_point() +
  geom_smooth(data=aggData_QMDbinsDen, method='lm',se=F,fullrange=TRUE) +
  geom_point(data = aggStandData, aes(x = logQMD, y = logDensity), alpha=0.2) +
  theme_bw() + labs(x = "Stand size", y = "Tree Density",color = "Time period") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom")

ggplot(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity)) + geom_point() +
  geom_smooth(data=aggData_QMDbinsRest, method='lm',se=F,fullrange=TRUE) +
  geom_point(data = aggStandData, aes(x = logQMD, y = logDensity), alpha=0.2) +
  theme_bw() + labs(x = "Stand size", y = "Tree Density",color = "Time period") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "bottom")

# Select plots with minimum 3 census
#aggData_QMDbinsDen <- aggData_QMDbinsDen %>%
#  group_by(PlotID) %>% mutate(n_census=n()) %>% ungroup() %>% filter(n_census>=3)
length(unique(aggData_QMDbinsDen$PlotID))
sort(table(aggData_QMDbinsDen$PlotID))

ggplot(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity)) + geom_point(alpha=.5) +
  geom_smooth(data=aggData_QMDbinsDen, method='lm',se=F,fullrange=TRUE) 

# rename dataset
save(aggData_QMDbinsDen, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75.RData"))
save(aggData_QMDbinsRest, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest75.RData"))

#save(aggData_QMDbinsDen, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen55.RData"))
#save(aggData_QMDbinsRest, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest55.RData"))

#save(aggData_QMDbinsDen, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen90.RData"))
#save(aggData_QMDbinsRest, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest90.RData"))

# Remove outliers ####
# Filter points that are not in the STL: High and low density and QMD
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75.RData"))
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest75.RData"))

#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen55.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest55.RData"))

#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen90.RData"))
#load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest90.RData"))

quartiles_density <- quantile(aggData_QMDbinsDen$logDensity, probs=c(.25, .75), na.rm = FALSE)
IQR_density <- IQR(aggData_QMDbinsDen$logDensity)
Lower_density <- quartiles_density[1] - 1.5*IQR_density
Upper_density <- quartiles_density[2] + 1.5*IQR_density 
quartiles_QMD <- quantile(aggData_QMDbinsDen$logQMD, probs=c(.25, .75), na.rm = FALSE)
IQR_QMD <- IQR(aggData_QMDbinsDen$logQMD)
Lower_QMD <- quartiles_QMD[1] - 1.5*IQR_QMD
Upper_QMD <- quartiles_QMD[2] + 1.5*IQR_QMD 
aggData_QMDbinsDen_out <- aggData_QMDbinsDen %>% 
  filter(logDensity>Lower_density&logDensity<Upper_density) %>% 
  filter(logQMD>Lower_QMD&logQMD<Upper_QMD)

ggplot(data = aggData_QMDbinsDen_out, aes(x = logQMD, y = logDensity)) + geom_point() +
  geom_smooth(data=aggData_QMDbinsDen, method='lm',se=F,fullrange=TRUE) 

length(unique(aggData_QMDbinsDen_out$PlotID))
sort(table(aggData_QMDbinsDen_out$PlotID))

#vec_outliers <- boxplot.stats(aggData_QMDbinsDen$logDensity)$out
#plot_data <- aggData_QMDbinsDen |> 
#  mutate(outlier = logDensity %in% vec_outliers)
#plot_data |> 
#  ggplot(aes(x = logQMD, y = logDensity, color = outlier)) + 
#  geom_point()
#vec_outliers <- boxplot.stats(aggData_QMDbinsDen$logQMD)$out
#plot_data <- aggData_QMDbinsDen |> 
#  mutate(outlier = logQMD %in% vec_outliers)
#plot_data |> 
#  ggplot(aes(x = logQMD, y = logDensity, color = outlier)) + 
#  geom_point()

# Extract the outliers points to add them to aggData_QMDbinsRest
aggData_QMDbinsDen_rest <- aggData_QMDbinsDen %>% 
  filter(!logDensity %in% aggData_QMDbinsDen_out$logDensity)
aggData_QMDbinsRest_out <- aggData_QMDbinsRest %>% bind_rows(aggData_QMDbinsDen_rest)

# rename dataset
save(aggData_QMDbinsDen_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
save(aggData_QMDbinsRest_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest75out.RData"))

#save(aggData_QMDbinsDen_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen55out.RData"))
#save(aggData_QMDbinsRest_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest55out.RData"))

#save(aggData_QMDbinsDen_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen90out.RData"))
#save(aggData_QMDbinsRest_out, file = paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest90out.RData"))

# Table S1 ####

# Unmanaged plots from different sources
load(paste0(here::here(), "/data/inputs_obs/aggStandData.RData"))
summary(aggStandData)
aggStandData %>% group_by(dataset) %>% summarise(count=n_distinct(PlotID))
aggStandData %>% summarise(count=n_distinct(PlotID))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(PlotArea_ha,na.rm=T),sd=sd(PlotArea_ha,na.rm=T),min=min(PlotArea_ha),max=max(PlotArea_ha))
aggStandData %>% group_by(dataset) %>% summarise(min=min(Year),max=max(Year))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(PeriodLength_years,na.rm=T),sd=sd(PeriodLength_years,na.rm=T))
aggStandData %>% count(dataset,PlotID) %>% group_by(dataset) %>%
  summarise(mean=mean(n,na.rm=T),sd=sd(n,na.rm=T),min=min(n,na.rm=T),max=max(n,na.rm=T))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(elevation,na.rm=T),sd=sd(elevation,na.rm=T))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(DBH,na.rm=T),sd=sd(DBH,na.rm=T))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(Density,na.rm=T),sd=sd(Density,na.rm=T))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(Biomass_Kg_m2,na.rm=T),sd=sd(Biomass_Kg_m2,na.rm=T))
aggStandData %>% group_by(dataset) %>% summarise(mean=mean(BiomassIncrement_Kg_m2_year,na.rm=T),sd=sd(BiomassIncrement_Kg_m2_year,na.rm=T))
aggStandData %>% group_by(PlotID) %>% arrange(Year) %>% 
  filter(row_number()==n()) %>% ungroup() %>% filter(years_since_management!=999) %>% 
  filter(!grepl('6003', PlotID)) %>% 
  group_by(dataset) %>%
  summarise(mean=mean(years_since_management,na.rm=T),sd=sd(years_since_management,na.rm=T))

# Subset of plots from the upper quantiles used in this study
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
summary(aggData_QMDbinsDen_out)
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(count=n_distinct(PlotID))
aggData_QMDbinsDen_out %>% summarise(count=n_distinct(PlotID))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(PlotArea_ha,na.rm=T),sd=sd(PlotArea_ha,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(PlotArea_ha,na.rm=T),sd=sd(PlotArea_ha,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(elevation,na.rm=T),sd=sd(elevation,na.rm=T),max=max(elevation,na.rm=T),min=min(elevation,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(elevation,na.rm=T),sd=sd(elevation,na.rm=T),max=max(elevation,na.rm=T),min=min(elevation,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(latitude,na.rm=T),sd=sd(latitude,na.rm=T),max=max(latitude,na.rm=T),min=min(latitude,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(latitude,na.rm=T),sd=sd(latitude,na.rm=T),max=max(latitude,na.rm=T),min=min(latitude,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(longitude,na.rm=T),sd=sd(longitude,na.rm=T),max=max(longitude,na.rm=T),min=min(longitude,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(longitude,na.rm=T),sd=sd(longitude,na.rm=T),max=max(longitude,na.rm=T),min=min(longitude,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(temp,na.rm=T),sd=sd(temp,na.rm=T),max=max(temp,na.rm=T),min=min(temp,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(temp,na.rm=T),sd=sd(temp,na.rm=T),max=max(temp,na.rm=T),min=min(temp,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(prec,na.rm=T),sd=sd(prec,na.rm=T),max=max(prec,na.rm=T),min=min(prec,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(prec,na.rm=T),sd=sd(prec,na.rm=T),max=max(prec,na.rm=T),min=min(prec,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(min=min(Year),max=max(Year))
aggData_QMDbinsDen_out %>% summarise(min=min(Year),max=max(Year))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(PeriodLength_years,na.rm=T),sd=sd(PeriodLength_years,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(PeriodLength_years,na.rm=T),sd=sd(PeriodLength_years,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(QMD,na.rm=T),sd=sd(QMD,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(QMD,na.rm=T),sd=sd(QMD,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(Density,na.rm=T),sd=sd(Density,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(Density,na.rm=T),sd=sd(Density,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(Biomass_Kg_m2,na.rm=T),sd=sd(Biomass_Kg_m2,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(Biomass_Kg_m2,na.rm=T),sd=sd(Biomass_Kg_m2,na.rm=T))
aggData_QMDbinsDen_out %>% group_by(dataset) %>% summarise(mean=mean(BiomassIncrement_Kg_m2_year,na.rm=T),sd=sd(BiomassIncrement_Kg_m2_year,na.rm=T))
aggData_QMDbinsDen_out %>% summarise(mean=mean(BiomassIncrement_Kg_m2_year,na.rm=T),sd=sd(BiomassIncrement_Kg_m2_year,na.rm=T))

aggData_QMDbinsDen_out %>% group_by(PlotID) %>% arrange(Year) %>% 
  filter(row_number()==n()) %>% ungroup() %>% filter(years_since_management!=999) %>%
  filter(!grepl('6003', PlotID)) %>% 
  group_by(dataset) %>%
  summarise(mean=mean(years_since_management,na.rm=T),sd=sd(years_since_management,na.rm=T))
aggData_QMDbinsDen_out %>% count(dataset,PlotID) %>% group_by(dataset) %>%
  summarise(mean=mean(n,na.rm=T),sd=sd(n,na.rm=T),min=min(n,na.rm=T),max=max(n,na.rm=T))

# Bray-Curtis dissimilarity index
# Need to use the by species data!
# BCij = 1 – (2*Cij) / (Si + Sj)
# Cij: The sum of the lesser values for the species found in each site.
# Si: The total number of specimens counted at site i
# Sj: The total number of specimens counted at site j
# The Bray-Curtis Dissimilarity always ranges between 0 (zero dissimilarity) and 1 (complete dissimilarity)

# Use fc vegdist from vegan pkg

# EFM
df_EFM_bray_curtis <- EFM_stand_Bysps %>% 
  filter(FNUM %in% aggData_QMDbinsDen$PlotID) %>% 
  select(FNUM,Latin,AJ,TreesPerHectareAHC1_2) %>%
  group_by(FNUM) %>% 
  filter(AJ == min(AJ)| AJ == max(AJ)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Latin, values_from = TreesPerHectareAHC1_2) %>% 
  group_by(FNUM) %>% mutate(n=n()) %>% filter(n==2) %>% ungroup() %>%
  select(-c(FNUM, AJ,n)) %>% as.data.frame()
EFM_bray_curtis <- vegdist(df_EFM_bray_curtis, method="bray",na.rm=T,diag=T)
EFM_bray_curtis <- as.matrix(EFM_bray_curtis)
# extracting odd rows 
EFM_bray_curtis_odd <- EFM_bray_curtis[ , seq_len(ncol(EFM_bray_curtis)) %% 2 == 1]
EFM_bray_curtis_odd <- as.data.frame(EFM_bray_curtis_odd)
EFM_bray_curtis_pre_index <- EFM_bray_curtis_odd %>% mutate_all(list(~ifelse(lag(.)==0,.,NA)))
# Check there is only 1 value ! NA per column
EFM_bray_curtis_pre_index %>% summarise_all(list(~sum(!is.na(.))))
# Summarise to get index
EFM_bray_curtis_index <- EFM_bray_curtis_pre_index %>% summarise_all(list(~ mean(., na.rm=T)))
EFM_bray_curtis_index_long <- EFM_bray_curtis_index %>% pivot_longer(cols = everything())
EFM_bray_curtis_index_long %>% summarise(mean=mean(value, na.rm=T),sd=sd(value, na.rm=T))

# Change in percentage of dominant species per plot
percent_speciesEFM <-  EFM_stand_Bysps %>% 
  filter(FNUM %in% aggData_QMDbinsDen$PlotID) %>% 
  select(FNUM, Latin, AJ,BasalAreaAHC1_2_m2perha) %>%
  group_by(FNUM, AJ) %>% 
  mutate(totalBA=sum(BasalAreaAHC1_2_m2perha,na.rm = T),
         percentBA=BasalAreaAHC1_2_m2perha/totalBA*100) %>% ungroup() %>%
  #arrange(AJ,FNUM) %>%
  group_by(FNUM, AJ) %>% top_n(1, percentBA) %>% ungroup() %>%
  group_by(FNUM) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup() %>%
  group_by(FNUM,Latin) %>%
  mutate(changePercentBA = (percentBA - lag(percentBA))/
           lag(percentBA)*100) %>% ungroup()
percent_speciesEFM %>% summarise(mean=mean(changePercentBA, na.rm=T))
percent_speciesEFM %>% distinct(Latin)
summary(percent_speciesEFM)

# NFI
df_NFI_bray_curtis <- NFI_tree_agg %>% 
  filter(PLOTID %in% aggData_QMDbinsDen$PlotID) %>% 
  select(PLOTID,SPECIES_NAME,Year,Total_Tree_density_perha) %>%
  group_by(PLOTID) %>% 
  filter(Year == min(Year)| Year == max(Year)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = SPECIES_NAME, values_from = Total_Tree_density_perha) %>% 
  group_by(PLOTID) %>% mutate(n=n()) %>% filter(n==2) %>% ungroup() %>%
  select(-c(PLOTID, Year,n)) %>% as.data.frame()
NFI_bray_curtis <- vegdist(df_NFI_bray_curtis, method="bray",na.rm=T,diag=T)
NFI_bray_curtis <- NFI_bray_curtis %>% as.matrix() %>%
  replace(., row(.) != col(.)+1, NA) %>% as_tibble()
# extracting odd rows 
NFI_bray_curtis_odd <- NFI_bray_curtis[ , seq_len(ncol(NFI_bray_curtis)) %% 2 == 1]
NFI_bray_curtis_odd <- as.data.frame(NFI_bray_curtis_odd)
NFI_bray_curtis_pre_index <- NFI_bray_curtis_odd #%>% mutate_all(list(~ifelse(lag(.)==0,.,NA)))
# Check there is only 1 value ! NA per column
NFI_bray_curtis_pre_index %>% summarise_all(list(~sum(!is.na(.))))
# Summarise to get index
NFI_bray_curtis_index <- NFI_bray_curtis_pre_index %>% summarise_all(list(~ mean(., na.rm=T)))
NFI_bray_curtis_index_long <- NFI_bray_curtis_index %>% pivot_longer(cols = everything())
NFI_bray_curtis_index_long %>%   summarise(mean=mean(value, na.rm=T),sd=sd(value, na.rm=T))
summary(NFI_bray_curtis_index_long)

# Change in percentage of dominant species per plot
percent_speciesNFI <-  NFI_tree_agg %>% 
  filter(PLOTID %in% aggData_QMDbinsDen$PlotID) %>% 
  select(PLOTID, SPECIES_NAME, Year,Total_BA_perha) %>%
  group_by(PLOTID, Year) %>% 
  mutate(totalBA=sum(Total_BA_perha,na.rm = T),
         percentBA=Total_BA_perha/totalBA*100) %>% ungroup() %>%
  #arrange(Year,PLOTID) %>%
  group_by(PLOTID, Year) %>% top_n(1, percentBA) %>% ungroup() %>%
  group_by(PLOTID) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup() %>%
  group_by(PLOTID,SPECIES_NAME) %>%
  mutate(changePercentBA = (percentBA - lag(percentBA))/
           lag(percentBA)*100) %>% ungroup()
percent_speciesNFI %>% summarise(mean=mean(changePercentBA, na.rm=T))
percent_speciesNFI %>% distinct(SPECIES_NAME)
summary(percent_speciesNFI)

# NFR
df_NFR_bray_curtis <- NFR_stand_Bysps %>% 
  filter(FNUM %in% aggData_QMDbinsDen$PlotID) %>% 
  select(FNUM,Latin,AJ,TreesPerHectareAHC1_2) %>%
  group_by(FNUM) %>% 
  filter(AJ == min(AJ)| AJ == max(AJ)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Latin, values_from = TreesPerHectareAHC1_2) %>% 
  group_by(FNUM) %>% mutate(n=n()) %>% filter(n==2) %>% ungroup() %>%
  select(-c(FNUM, AJ,n)) %>% as.data.frame()
NFR_bray_curtis <- vegdist(df_NFR_bray_curtis, method="bray",na.rm=T,diag=T)
NFR_bray_curtis <- as.matrix(NFR_bray_curtis)
# extracting odd rows 
NFR_bray_curtis_odd <- NFR_bray_curtis[ , seq_len(ncol(NFR_bray_curtis)) %% 2 == 1]
NFR_bray_curtis_odd <- as.data.frame(NFR_bray_curtis_odd)
NFR_bray_curtis_pre_index <- NFR_bray_curtis_odd %>% mutate_all(list(~ifelse(lag(.)==0,.,NA)))
# Check there is only 1 value ! NA per column
NFR_bray_curtis_pre_index %>% summarise_all(list(~sum(!is.na(.))))
# Summarise to get index
NFR_bray_curtis_index <- NFR_bray_curtis_pre_index %>% summarise_all(list(~ mean(., na.rm=T)))
NFR_bray_curtis_index_long <- NFR_bray_curtis_index %>% pivot_longer(cols = everything())
NFR_bray_curtis_index_long %>%   summarise(mean=mean(value, na.rm=T),sd=sd(value, na.rm=T))
summary(NFR_bray_curtis_index_long)

# Change in percentage of dominant species per plot
percent_speciesNFR <-  NFR_stand_Bysps %>% 
  filter(FNUM %in% aggData_QMDbinsDen$PlotID) %>% 
  select(FNUM, Latin, AJ,BasalAreaAHC1_2_m2perha) %>%
  group_by(FNUM, AJ) %>% 
  mutate(totalBA=sum(BasalAreaAHC1_2_m2perha,na.rm = T),
         percentBA=BasalAreaAHC1_2_m2perha/totalBA*100) %>% ungroup() %>%
  #arrange(AJ,FNUM) %>%
  group_by(FNUM, AJ) %>% top_n(1, percentBA) %>% ungroup() %>%
  group_by(FNUM) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup() %>%
  group_by(FNUM,Latin) %>%
  mutate(changePercentBA = (percentBA - lag(percentBA))/
           lag(percentBA)*100) %>% ungroup()
percent_speciesNFR %>% summarise(mean=mean(changePercentBA, na.rm=T))
percent_speciesNFR %>% distinct(Latin)
summary(percent_speciesNFR)

# Full data
df_EFM_stand_Bysps <- EFM_stand_Bysps %>% select(FNUM,Latin,AJ,
  TreesPerHectareAHC1_2,BasalAreaAHC1_2_m2perha) %>%
  mutate(FNUM=as.character(FNUM)) %>%
  rename(PlotID=FNUM, Species=Latin, Year=AJ, StandDensity=TreesPerHectareAHC1_2, BAm2ha=BasalAreaAHC1_2_m2perha)
df_NFI_stand_Bysps <- NFI_tree_agg %>% select(PLOTID,SPECIES_NAME,Year,
   Total_Tree_density_perha,Total_BA_perha) %>%
  mutate(PLOTID=as.character(PLOTID)) %>%
  rename(PlotID=PLOTID, Species=SPECIES_NAME,StandDensity=Total_Tree_density_perha, BAm2ha=Total_BA_perha)
df_NFR_stand_Bysps <- NFR_stand_Bysps %>% select(FNUM,Latin,AJ,
  TreesPerHectareAHC1_2,BasalAreaAHC1_2_m2perha) %>%
  rename(PlotID=FNUM, Species=Latin, Year=AJ, StandDensity=TreesPerHectareAHC1_2, BAm2ha=BasalAreaAHC1_2_m2perha)

df_allData_Bysps <- df_EFM_stand_Bysps %>% bind_rows(df_NFI_stand_Bysps) %>% 
  bind_rows(df_NFR_stand_Bysps)

df_allData_bray_curtis <- df_allData_Bysps %>% 
  filter(PlotID %in% aggData_QMDbinsDen$PlotID) %>% 
  select(PlotID,Species,Year,StandDensity) %>%
  group_by(PlotID) %>% 
  filter(Year == min(Year)| Year == max(Year)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = StandDensity) %>% 
  group_by(PlotID) %>% mutate(n=n()) %>% filter(n==2) %>% ungroup() %>%
  select(-c(PlotID, Year,n)) %>% as.data.frame()
allData_bray_curtis <- vegdist(df_allData_bray_curtis, method="bray",na.rm=T,diag=T)
allData_bray_curtis <- allData_bray_curtis %>% as.matrix() %>%
  replace(., row(.) != col(.)+1, NA) %>% as_tibble()
# extracting odd rows 
allData_bray_curtis_odd <- allData_bray_curtis[ , seq_len(ncol(allData_bray_curtis)) %% 2 == 1]
allData_bray_curtis_odd <- as.data.frame(allData_bray_curtis_odd)
allData_bray_curtis_pre_index <- allData_bray_curtis_odd #%>% mutate_all(list(~ifelse(lag(.)==0,.,NA)))
# Check there is only 1 value ! NA per column
allData_bray_curtis_pre_index %>% summarise_all(list(~sum(!is.na(.))))
# Summarise to get index
allData_bray_curtis_index <- allData_bray_curtis_pre_index %>% summarise_all(list(~ mean(., na.rm=T)))
allData_bray_curtis_index_long <- allData_bray_curtis_index %>% pivot_longer(cols = everything())
allData_bray_curtis_index_long %>%   summarise(mean=mean(value, na.rm=T),sd=sd(value, na.rm=T))
summary(allData_bray_curtis_index_long) 

# Change in percentage of dominant species per plot
percent_species_alData <-  df_allData_Bysps %>% 
  filter(PlotID %in% aggData_QMDbinsDen$PlotID) %>% 
  select(PlotID,Species,Year,BAm2ha) %>%
  group_by(PlotID, Year) %>% 
  mutate(totalBA=sum(BAm2ha,na.rm = T),
         percentBA=BAm2ha/totalBA*100) %>% ungroup() %>%
  #arrange(Year,PlotID) %>%
  group_by(PlotID, Year) %>% top_n(1, percentBA) %>% ungroup() %>%
  group_by(PlotID) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup() %>%
  group_by(PlotID,Species) %>%
  mutate(changePercentBA = (percentBA - lag(percentBA))/
           lag(percentBA)*100) %>% ungroup()
percent_species_alData %>% summarise(mean=mean(changePercentBA, na.rm=T))
percent_species_alData %>% distinct(Species)
summary(percent_species_alData)

# Figure S1 ####
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsDen75out.RData"))
load(paste0(here::here(), "/data/inputs_obs/aggData_QMDbinsRest75out.RData"))

fig_S1 <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen_out, aes(x = logQMD, y = logDensity,col=Year), alpha=0.6, size = 1, shape = 16, inherit.aes = FALSE) + 
  geom_point(data = aggData_QMDbinsRest_out, aes(x = logQMD, y = logDensity,col=Year), alpha=0.6, size = 1,shape = 16, inherit.aes = FALSE) + 
  labs(x = "ln QMD", y = "ln N",color  = "Year") + 
  scale_color_viridis(option="magma",breaks = c(1950, 1975, 2000),trans = 'reverse') +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 8),legend.title = element_text(size = 8),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.18, .20),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.4, 'cm'),
                      legend.box.margin = margin(1, 1, 1, 1)) +
scale_x_continuous(breaks = seq(2,4,1)) +
scale_y_continuous(breaks = seq(4,8,2))
fig_S1
ggsave(paste0(here::here(), "/manuscript/figures/fig_S1.png"), width = 5, height = 4.5, dpi=300)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S1.pdf"), width = 5, height = 4.5, dpi=300)
