# This script reads the data from the Swiss EFR, NFI and NFR plots.

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# EFM - Experimental Forest Management ####

# Plot characteristics
EFM_plots_metadata <- read.csv("~/GFDY/data/raw_obs/efm/VFL_LISTmanual.csv")
str(EFM_plots_metadata)

# Species characteristics
EFM_species_metadata <- read.csv("~/GFDY/data/raw_obs/efm/specieslist.csv")
str(EFM_species_metadata)

# Stand level data (per plot, year and species)
# All the plots in this dataset are unmanaged.
EFM_stand <- readRDS("~/GFDY/data/raw_obs/efm/EFM_stand_data.RDS") # 18 plots
str(EFM_stand)
length(unique(EFM_stand$FNUM))
# Join Stand level data with plot metadata
EFM_stand <- EFM_stand %>% inner_join(EFM_plots_metadata[,c(4,7,8,19)])
# Create new variables for linear mixed models at the stand level
EFM_stand <- EFM_stand %>% mutate(QMD=sqrt(BasalAreaAHC1_2_m2perha/(0.0000785*TreesPerHectareAHC1_2))) %>%
  mutate(logQMD=log(QMD)) %>% 
  mutate(Area_ha=HA*0.0001) %>% 
  mutate(N_indiv=TreesPerHectareAHC1_2*Area_ha) %>% 
  mutate(BiomassKgperha=(AbovegroundAHC1_2_Mgperha+RootmassAHC1_2_Mgperha)*1000) %>%
  mutate(BiomassIncrement_KgperhaperPeriod=(AbovegroundIncrement_MgperhaperPeriod+RootmassIncrement_MgperhaperPeriod)*1000) %>%
  mutate(BiomassIncrement_KgperhaperYear=BiomassIncrement_KgperhaperPeriod/PeriodLength_years) %>%
  mutate(logTreesPerHectareAHC1_2=log(TreesPerHectareAHC1_2))
# Select Stand level data for all species: "All species combined"
EFM_stand_Allsps <- EFM_stand %>% filter(Latin =="All species combined")

# Identify which species dominates in each site (FNUM) and year (AJ)
EFM_stand_Bysps <- EFM_stand %>% filter(Latin !="All species combined")
length(unique(EFM_stand_Bysps$FNUM))
dominant_speciesEFM <- EFM_stand_Bysps %>% group_by(FNUM, AJ) %>% top_n(1, BasalAreaAHC1_2_m2perha) %>% rename(Species=Latin)

percent_speciesEFM <-  EFM_stand_Bysps %>% group_by(FNUM, AJ) %>% mutate(totalBA=sum(BasalAreaAHC1_2_m2perha,na.rm = T)) %>% ungroup() %>%
  group_by(FNUM, AJ, Latin) %>% mutate(percent=BasalAreaAHC1_2_m2perha/totalBA*100) %>% ungroup() %>%
  group_by(FNUM, AJ) %>% top_n(3, BasalAreaAHC1_2_m2perha) %>%
  select(FNUM, Latin, AJ, BasalAreaAHC1_2_m2perha,totalBA,percent) %>%
  arrange(AJ,FNUM)

change_speciesEFM <-  EFM_stand_Bysps %>% group_by(FNUM) %>% filter(AJ == min(AJ)| AJ == max(AJ)) %>% ungroup() %>% 
  group_by(FNUM,AJ) %>% mutate(total=sum(TreesPerHectareAHC1_2,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM) %>% mutate(totalYears=sum(TreesPerHectareAHC1_2,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM,Latin) %>% mutate(mini = min(TreesPerHectareAHC1_2,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM,AJ) %>% mutate(lesser=sum(mini,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM,AJ) %>% mutate(BC = 1-(2*lesser) / (totalYears)) %>% ungroup() %>%
  group_by(FNUM) %>% summarise(BC=mean(BC))
change_speciesEFM
change_speciesEFM <-  change_speciesEFM %>% mutate(BC=ifelse(BC<0,0,BC))
change_speciesEFM %>% mutate(Plot=as.factor(seq(1,n()))) %>% ggplot() + geom_col(aes(x=Plot, y=BC))

# Select plots that are part of the STL analysis (see selection later)
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75.RData")
change_speciesEFM <- change_speciesEFM %>% filter(FNUM %in% aggData_QMDbinsDen$PlotID) 
mean(change_speciesEFM$BC,na.rm=T)
sd(change_speciesEFM$BC,na.rm=T)

# Add dominant species per plot to EFM_stand_Allsps
EFM_stand_Allsps <- EFM_stand_Allsps %>% left_join(dominant_speciesEFM[,c(1,3,4)])

# NFI - Swiss National Forest Inventories ####

# Plot characteristics
NFI_plot_constant <- read.csv("~/GFDY/data/raw_obs/nfi/NFI_plot_constant.csv")

# Species names
NFI_species_constant <- read.csv("~/GFDY/data/raw_obs/nfi/Species_names.csv")

# Stand-level data
#The plots included in this dataset are managed and unmanaged.
#The filter of unmanaged plots need to be done using the following variables: 
#LETZTENU variable establishes the number of years since the last management intervention.
#MANAGEMENT_INDICATOR variable establishes No treatment (kein Eingriff) as ID 1. 
#The variable 'Management_indicator' covers treatments since the last inventory (for example, a plot with 'census_id' == 250 and 'management_indicator' == 1 has not been managed between NFI1 (census_id == 150) and NFI2 (census_id == 250)).
NFI_plot_census <- read.csv("~/GFDY/data/raw_obs/nfi/MIND_plot_census_20210121.csv")
str(NFI_plot_census)
# Join Stand level data with plot metadata
NFI_plot_census <- NFI_plot_census %>% inner_join(NFI_plot_constant[,c(1,6)])
# Create Year variable from CENSUS_DATE
NFI_plot_census <- NFI_plot_census %>% mutate(Year=str_sub(CENSUS_DATE, 7, 10)) %>% 
  mutate(Year=as.numeric(Year)) %>% relocate(Year, .after=CENSUS_DATE)
# Remove two plots from NFI with extreme high values of growth
NFI_plot_census <- NFI_plot_census %>% filter(!(PLOTID==149566|PLOTID==146688)) 
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
  mutate(logNPH=log(NPH))
# Filter unmanaged plots using the LETZTENU variable: Assign different value 40 to 70 years since last management
NFI_plot_census <- NFI_plot_census %>% filter(LETZTENU>=70) 
# Filter using the MANAGEMENT_INDICATOR variable and ensuring that the plot have at least 2 census 
# (SUM_CENSUSID needs to be higher that 450). If we want to include the 4 census, then SUM_CENSUSID needs to be equal to 1200.
NFI_plot_census <- NFI_plot_census %>% filter(MANAGEMENT_INDICATOR==1|MANAGEMENT_INDICATOR==-1) %>%
  group_by(PLOTID) %>% mutate(SUM_CENSUSID = sum(CENSUSID)) %>% filter(SUM_CENSUSID>450)
length(unique(NFI_plot_census$PLOTID)) # 521 plots for 70 yrs ago
summary(NFI_plot_census)

# Tree-level data
NFI_tree_census <- read.csv("~/GFDY/data/raw_obs/nfi/NFI_tree_census.csv")
str(NFI_tree_census)
length(unique(NFI_tree_census$PLOTID)) 
# Select same plots selected at stand level
NFI_tree_census <- NFI_tree_census %>% filter(PLOTID %in% NFI_plot_census$PLOTID)
# Create Year variable from CENSUS_DATE
NFI_tree_census <- NFI_tree_census %>% mutate(Year=str_sub(CENSUS_DATE, 7, 10)) %>% 
  mutate(Year=as.numeric(Year)) %>% relocate(Year, .after=CENSUS_DATE)
# Add species names
NFI_tree_census <- NFI_tree_census %>% right_join(NFI_species_constant) %>% 
  relocate(SPECIES_NAME, .after=SPECIES_ID)
# Filter tree species and tree with status = 1 alive
NFI_tree_census <- NFI_tree_census %>% filter(SPECIES_ID!=-1&SPECIES_ID!=999) %>% filter(TREE_STATUS==1)
# Add number of trees per plotID, censusID and SPECIES_ID (n) and multiply by REPRESENTATION to estimate tree density
NFI_tree_census <- NFI_tree_census %>%  group_by(PLOTID,CENSUSID,SPECIES_ID) %>%  add_tally() %>% mutate(Tree_density_perha=REPRESENTATION*n) 
# Calculate BA per tree and per ha (m2/ha)
NFI_tree_census <- NFI_tree_census %>% mutate(BA=pi*(DBH*0.01)^2/4) %>% mutate(BA_perha=BA*Tree_density_perha) 
# Aggregate by plot and species
NFI_tree_agg <- NFI_tree_census %>% group_by(PLOTID,Year,SPECIES_NAME) %>% 
  summarise(Total_BA_perha=sum(BA_perha),Total_Tree_density_perha = sum(Tree_density_perha)) 
  
# Identify which species dominates in each site (FNUM) and year (AJ)
dominant_speciesNFI <- NFI_tree_agg %>% group_by(PLOTID, Year) %>% top_n(1, Total_BA_perha) %>% rename(Species=SPECIES_NAME)

# Add dominant species per plot to NFI_plot_census
NFI_plot_census <- NFI_plot_census %>% left_join(dominant_speciesNFI[,c(1,2,3)])

# Identify changes in species composition
change_speciesNFI <-  NFI_tree_agg %>% group_by(PLOTID) %>% filter(Year == min(Year)| Year == max(Year)) %>% ungroup() %>% 
  group_by(PLOTID,Year) %>% mutate(total=sum(Total_Tree_density_perha,na.rm=T)) %>% ungroup() %>%
  group_by(PLOTID) %>% mutate(totalYears=sum(Total_Tree_density_perha,na.rm=T)) %>% ungroup() %>%
  group_by(PLOTID,SPECIES_NAME) %>% mutate(mini = min(Total_Tree_density_perha,na.rm=T)) %>% ungroup() %>%
  group_by(PLOTID,Year) %>% mutate(lesser=sum(mini,na.rm=T)) %>% ungroup() %>%
  group_by(PLOTID,Year) %>% mutate(BC = 1-(2*lesser) / (totalYears)) %>% ungroup() %>%
  group_by(PLOTID) %>% summarise(BC=mean(BC))
change_speciesNFI
change_speciesNFI <-  change_speciesNFI %>% mutate(BC=ifelse(BC<0,0,BC))
change_speciesNFI %>% mutate(Plot=as.factor(seq(1,n()))) %>% ggplot() + geom_col(aes(x=Plot, y=BC))

# Select plots that are part of the STL analysis (see selection later)
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75.RData")
change_speciesNFI <- change_speciesNFI %>% filter(PLOTID %in% aggData_QMDbinsDen$PlotID) 
mean(change_speciesNFI$BC,na.rm=T)
sd(change_speciesNFI$BC,na.rm=T)

# Natural Forests Reserves NFR ####

# Metadata
NFR_Metadata <- read.csv("~/GFDY/data/raw_obs/nfr/NFR_metadata.csv")
NFR_Metadata <- NFR_Metadata %>% rename(fg=reserve)

# Plot info
NFR_Reserves_Info <- read.csv("~/GFDY/data/raw_obs/nfr/ReservesInfo.csv")

# Plot area
NFR_plot_area <- read.csv("~/GFDY/data/raw_obs/nfr/plot_area.csv",sep=",")
# Join reserve (fg) and plot (tf) id in a new variable called PlotID as character
NFR_plot_area <- NFR_plot_area %>% mutate(fg = as.character(fg),tf = as.character(tf)) %>%
  unite(FNUM2, fg, tf) %>% dplyr::select(-c(reserve)) %>% mutate(plot_area_ha=plot_area_m2*0.0001)
length(unique(NFR_plot_area$FNUM2)) # Plot

# Stand level data (per plot, year and species)
#All the plots in this dataset are unmanaged.
NFR_stand <- readRDS("~/GFDY/data/raw_obs/nfr/NFR_stand_data.RDS") # 291 plots From David Forrester data
str(NFR_stand)
# Remove FNUM with TreesPerHectareAHC1_2 == 0 or NA
NFR_stand <- NFR_stand %>% filter(TreesPerHectareAHC1_2!=0) %>% filter(is.na(TreesPerHectareAHC1_2)==FALSE)
# Remove FNUM because it has a disproportionally high BA and Biomass increment, as suggested by David Forrester
NFR_stand <- NFR_stand %>% filter(FNUM!=7007028,FNUM!=7005006,FNUM!=7005003,FNUM!=7005002,FNUM!=7001001,FNUM!=7001002,FNUM!=7001003,FNUM!=7002003)
# Join Stand level data with plot metadata
NFR_stand <- NFR_stand %>% mutate(fg=as.integer(str_sub(FNUM, 1, 4))) %>% left_join(NFR_Metadata[,c(1,5,6,7,8,9)]) %>% distinct()
# Join Stand level data with plot are
NFR_stand <- NFR_stand %>% inner_join(NFR_plot_area[,c(1,3)])
# Create new variables for linear mixed models at the stand level
NFR_stand <- NFR_stand %>% mutate(QMD=sqrt(BasalAreaAHC1_2_m2perha/(0.0000785*TreesPerHectareAHC1_2))) %>%
  mutate(logQMD=log(QMD)) %>% 
  mutate(N_indiv=TreesPerHectareAHC1_2*plot_area_ha) %>% 
  mutate(BiomassKgperha=(AbovegroundAHC1_2_Mgperha+RootmassAHC1_2_Mgperha)*1000) %>%
  mutate(BiomassIncrement_KgperhaperPeriod=(AbovegroundIncrement_MgperhaperPeriod+RootmassIncrement_MgperhaperPeriod)*1000) %>%
  mutate(BiomassIncrement_KgperhaperYear=BiomassIncrement_KgperhaperPeriod/PeriodLength_years) %>%
  mutate(logTreesPerHectareAHC1_2=log(TreesPerHectareAHC1_2))
# Select Stand level data for all species: "All species combined"
NFR_stand_Allsps <- NFR_stand %>% filter(Latin =="All species combined")
#NFR_stand_Allsps <- NFR_stand_Allsps[,c(1,56,55,5,49,20,10,57,58,60,61,62,13,65,63,64)] 
length(unique(NFR_stand_Allsps$FNUM))
dim(NFR_stand_Allsps)

# Identify which species dominates in each site (FNUM) and year (AJ)
NFR_stand_Bysps <- NFR_stand %>% filter(Latin !="All species combined")
length(unique(NFR_stand_Bysps$FNUM))
dominant_speciesNFR <- NFR_stand_Bysps %>% group_by(FNUM, AJ) %>% top_n(1, BasalAreaAHC1_2_m2perha) %>% rename(Species=Latin)

change_speciesNFR <-  NFR_stand_Bysps %>% group_by(FNUM) %>% filter(AJ == min(AJ)| AJ == max(AJ)) %>% ungroup() %>% 
  group_by(FNUM,AJ) %>% mutate(total=sum(TreesPerHectareAHC1_2,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM) %>% mutate(totalYears=sum(TreesPerHectareAHC1_2,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM,Latin) %>% mutate(mini = min(TreesPerHectareAHC1_2,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM,AJ) %>% mutate(lesser=sum(mini,na.rm=T)) %>% ungroup() %>%
  group_by(FNUM,AJ) %>% mutate(BC = 1-(2*lesser) / (totalYears)) %>% ungroup() %>%
  group_by(FNUM) %>% summarise(BC=mean(BC))
change_speciesNFR
change_speciesNFR <-  change_speciesNFR %>% mutate(BC=ifelse(BC<0,0,BC))
change_speciesNFR %>% mutate(Plot=as.factor(seq(1,n()))) %>% ggplot() + geom_col(aes(x=Plot, y=BC))

# Select plots that are part of the STL analysis (see selection later)
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75.RData")
change_speciesNFR <- change_speciesNFR %>% filter(FNUM %in% aggData_QMDbinsDen$PlotID) 
mean(change_speciesNFR$BC,na.rm=T)
sd(change_speciesNFR$BC,na.rm=T)

# Add dominant species per plot to NFR_stand_Allsps
NFR_stand_Allsps <- NFR_stand_Allsps %>% left_join(dominant_speciesNFR[,c(1,5,4)])
length(unique(NFR_stand_Allsps$Species))
length(unique(NFR_stand_Allsps$FNUM))

# Aggregated stand level data from EFM, NFI and NFR ####
aggEFM <- EFM_stand_Allsps %>% dplyr::select(c(FNUM,HUM,AJ,DBHqAHC1_2_cm,QMD,logQMD,BiomassKgperha,TreesPerHectareAHC1_2,
                                               logTreesPerHectareAHC1_2,BiomassIncrement_KgperhaperYear,Species)) %>% 
  rename(PlotID=FNUM, Year=AJ, DBH =DBHqAHC1_2_cm,Elevation=HUM, Density=TreesPerHectareAHC1_2,logDensity=logTreesPerHectareAHC1_2,
         Biomass_Kg_ha=BiomassKgperha,BiomassIncrement_Kg_ha_year=BiomassIncrement_KgperhaperYear) %>% mutate(dataset="EFM")
aggEFMpos <- aggEFM %>% filter(is.na(BiomassIncrement_Kg_ha_year)|BiomassIncrement_Kg_ha_year>=0) %>%
  mutate(BiomassIncrement_Kg_ha_year=ifelse(BiomassIncrement_Kg_ha_year==0,0.0001,BiomassIncrement_Kg_ha_year))

aggNFI <- NFI_plot_census %>% dplyr::select(c(PLOTID,ELEVATION,Year,MEAN_DBH_HA, QMD,logQMD,Biomass_Kg_ha,NPH,logNPH,
                                              BiomassIncrement_Kg_ha_year,Species)) %>% 
  rename(PlotID=PLOTID, DBH=MEAN_DBH_HA,Elevation=ELEVATION, Density=NPH, logDensity=logNPH) %>% mutate(dataset="NFI")
aggNFIpos <- aggNFI %>% filter(is.na(BiomassIncrement_Kg_ha_year)|BiomassIncrement_Kg_ha_year>=0) %>%
  mutate(BiomassIncrement_Kg_ha_year=ifelse(BiomassIncrement_Kg_ha_year==0,0.0001,BiomassIncrement_Kg_ha_year))

aggNFR <- NFR_stand_Allsps %>% dplyr::select(c(FNUM,ele,AJ,DBHqAHC1_2_cm,QMD,logQMD,BiomassKgperha,TreesPerHectareAHC1_2,
                                               logTreesPerHectareAHC1_2,BiomassIncrement_KgperhaperYear,Species)) %>% 
  rename(PlotID=FNUM, Year=AJ, DBH =DBHqAHC1_2_cm,Elevation=ele, Density=TreesPerHectareAHC1_2,logDensity=logTreesPerHectareAHC1_2,
         Biomass_Kg_ha=BiomassKgperha,BiomassIncrement_Kg_ha_year=BiomassIncrement_KgperhaperYear) %>% mutate(dataset="NFR")
aggNFRpos <- aggNFR %>% filter(is.na(BiomassIncrement_Kg_ha_year)|BiomassIncrement_Kg_ha_year>=0) %>%
  mutate(BiomassIncrement_Kg_ha_year=ifelse(BiomassIncrement_Kg_ha_year==0,0.0001,BiomassIncrement_Kg_ha_year))

aggEFM <- as_tibble(aggEFMpos)
aggNFI <- as_tibble(aggNFIpos)
aggNFR <- as_tibble(aggNFRpos)

length(unique(aggEFM$PlotID))
length(unique(aggNFI$PlotID))
length(unique(aggNFR$PlotID))

aggData <- rbind(aggEFM,aggNFI,aggNFR)
# Convert NPP from BiomassIncrement_Kg_ha_year to BiomassIncrement_Kg_m2_year
aggData <- aggData %>% mutate(BiomassIncrement_Kg_m2_year=BiomassIncrement_Kg_ha_year/10000)
summary(aggData)
length(unique(aggData$PlotID))
# save
save(aggData, file = "~/GFDY/data/inputs_obs/aggData.RData")
