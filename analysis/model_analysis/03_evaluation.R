# This script evaluates the model to compare the target variables selected
# This script runs all simulations for the different model parameterizations (shapes of mortality)
# and for the different levels of LUE (control, +15% and +30%)

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)
library(patchwork)

load(paste0(here::here(), "/data/inputs_mod/ddf_obs.RData"))

# Evaluation ####
# Compare targets before and after calibration for DBH

#Pre
preDBHp1gl_out_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/previous/preDBHp2gs_output_annual_tile.csv"))
preDBHp1gl_out_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/previous/preDBHp2gs_output_annual_cohorts.csv"))

df_mod <- preDBHp1gl_out_annual_tile %>% 
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- preDBHp1gl_out_annual_cohorts %>%
  dplyr::filter(year>df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins,year) %>% summarise(nTrees=sum(density)) %>% ungroup() %>% 
  group_by(size_bins) %>% summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],
                  df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])) %>% 
  dplyr::left_join(ddf_obs, by = "variables")

precalibDBH <- ggplot(dff) +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed") +
  geom_abline(col="grey") + ggtitle("Before calibration - DBH gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

# Post
# DBH p2=2.5 and LUE control
postDBHp2gs_output_annual_tile <- read.csv(paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp2gl_out_annual_tile.csv"))
postDBHp2gs_output_annual_cohorts <- read.csv(paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp2gl_out_annual_cohorts.csv"))

df_mod <- postDBHp2gs_output_annual_tile %>% 
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- postDBHp2gs_output_annual_cohorts %>%
  dplyr::filter(year>df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins,year) %>% summarise(nTrees=sum(density)) %>% ungroup() %>% 
  group_by(size_bins) %>% summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,
                  df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])
) %>% dplyr::left_join(ddf_obs, by = "variables")

postcalibDBH <- ggplot(dff) +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed")+
  geom_abline(col="grey") + ggtitle("After calibration - DBH gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

# Plotting separately
dff_sel1 <- dff %>% filter(variables=="GPP"|variables=="LAI"|variables=="Biomass") 
ggplot(dff_sel1) +
  geom_abline(col="grey") + 
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  #facet_wrap(~variables,ncol = 3,labeller = labeller(variables = c("Biomass"="Biomass","GPP"="GPP","LAI"="LAI"))) +
  scale_color_viridis(discrete=TRUE,option="viridis",
                    "Variable",breaks = c("Biomass", "GPP", "LAI"),
                    guide = guide_legend(override.aes = list(size=1.6),order=1)) +
  labs(x = "Modeled", y = "Observed") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.85, .20),
                     legend.direction="vertical",
                     legend.box = "horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.7, 'cm'),
                     legend.key.height = unit(.4, 'cm'),
                     legend.spacing.x = unit(0.05, 'cm'),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,45),breaks = seq(0,40,20)) +
  scale_y_continuous(limits = c(0,45),breaks = seq(0,40,20))

dff_sel2 <- dff %>% filter(variables!="GPP"&variables!="LAI"&variables!="Biomass") 
ggplot(dff_sel2) +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  labs(x = "Predicted", y = "Observed") + theme_classic() +
  geom_abline(col="grey") + 
  #facet_wrap(~variables,ncol = 3,labeller = labeller(variables = c("Biomass"="Biomass","GPP"="GPP","LAI"="LAI"))) +
  scale_x_continuous(limits = c(40,85)) +
  scale_y_continuous(limits = c(40,85))

ff <- precalibDBH + postcalibDBH + plot_annotation(tag_levels = 'A')
ff

# Run simulations ####
# DBH mortality has the shape params: p1=1.5, p2=2.5, p3=4.0
# The calibration was done with p2=2.5
load(paste0(here::here(), "/data/inputs_mod/df_drivers_DBH_gs.RData"))
load(paste0(here::here(), "/data/inputs_mod/settings_calib_DBH_gs_uniq_euler.RData"))

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_DBH_gs$par_opt["phiRL"]  
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_DBH_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_DBH_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_DBH_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_DBH_gs$par_opt["par_mort_under"]

df_calib_DBH_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = NPP)) +
  theme_classic()+labs(x = "Year", y = "NPP")

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = c_deadtrees+m_turnover)) +
  theme_classic()+labs(x = "Year", y = "c_deadtrees+m_turnover")

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = n_deadtrees)) +
  theme_classic()+labs(x = "Year", y = "n_deadtrees")

# DBH p1=1.5
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp1gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp1gl_out_annual_cohorts.csv"))
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea2sa1DBHp1gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea2sa1DBHp1gl_out_annual_cohorts.csv"))
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea3sa1DBHp1gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea3sa1DBHp1gl_out_annual_cohorts.csv"))

# DBH p2=2.5
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp2gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp2gl_out_annual_cohorts.csv"))
# LUE +15%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea2sa1DBHp2gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea2sa1DBHp2gl_out_annual_cohorts.csv"))
# LUE +30%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea3sa1DBHp2gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea3sa1DBHp2gl_out_annual_cohorts.csv"))

# DBH p3=4.0
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp3gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea1sa1DBHp3gl_out_annual_cohorts.csv"))
# LUE +15%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea2sa1DBHp3gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea2sa1DBHp3gl_out_annual_cohorts.csv"))
# LUE +30%
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, paste0(here::here(), "/data/outputs_mod/simulations/ea3sa1DBHp3gl_out_annual_tile.csv"))
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts, paste0(here::here(), "/data/outputs_mod/simulations/ea3sa1DBHp3gl_out_annual_cohorts.csv"))

