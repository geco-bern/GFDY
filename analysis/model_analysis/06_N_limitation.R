# This script evaluates the model and compares the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)  # XXX install directly from lauramarques/rsofun, branch GFDY
library(ggplot2)
library(multidplyr)
library(patchwork)

# Calibration discussed with Beni
# N inputs
# 1. From Lamarque et al. 2013, select values of N deposition (N_input) for Swistzerland and Scandinavia
# CH 400-500 mg(N) m−2yr−1 -> N_input = 0.00045 kgN m-2 yr-1
# SW 100-200 mg(N) m−2yr−1 -> N_input = 0.00015 kgN m-2 yr-1
# 2. Parameters controling N deposition (N_input) and initial mineral N (init_Nmineral) 
# init_slow_soil_C    = 0.0,    # initial slow soil C, kg C/m2
# init_Nmineral       = 0.015,  # Mineral nitrogen pool, (kg N/m2)
# 3. Parameters controlling N losses: (see N_loss in vegetation_lm3ppa.mod.f90)
# K_nitrogen   = 8.0,   # mineral Nitrogen turnover rate
# etaN         = 0.025, # loss rate with runoff

# DBH mortality has the shape params: p1=1.5, p2=2.5, p3=4.0
load("~/GFDY/data/inputs_mod/df_drivers_DBH_gs.RData")  # XXX change all paths to paste0(here::here(), "/data/...") or similar
load("~/GFDY/data/inputs_mod/settings_calib_DBH_gs_uniq_euler.RData")

df_drivers$params_siml[[1]]$method_mortality
df_drivers$params_siml[[1]]$method_photosynth

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_DBH_gs$par_opt["phiRL"]  
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_DBH_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_DBH_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_DBH_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_DBH_gs$par_opt["par_mort_under"]

# run the model CH ####
# df_drivers$params_siml[[1]]$do_closedN_run <- FALSE
x <- 1e-7 # set value
a <- 0.5
df_drivers$params_tile[[1]]$K_nitrogen <- x * a
df_drivers$params_tile[[1]]$etaN <- x * (1-a)
df_drivers$init_soil[[1]]$init_Nmineral <- 0.05 # 0.015, 0.5 or 0.1 # kg N/m2 really high
df_drivers$init_soil[[1]]$init_slow_soil_C <- 20 # 20 kg C/m2 total SOC stock is relatively relatively high
df_drivers$init_soil[[1]]$init_fast_soil_C <- 10 # 20 kg C/m2 total SOC stock is relatively relatively high
df_drivers$init_soil[[1]]$N_input <- 800 * 1e-6 # In Europe: 150-800 mg N m-2 yr-1 -> kg N m-2 yr-1 0.00045 (high N) or 0.00015 (low N) # forcing from data
df_drivers$params_species[[1]]$Nfixrate0 = 0.0 # turn off N fixation

## N-fertilisation experiments-------------
get_outputs <- function(df_drivers, nfert_mg_n){
  
  df_drivers$init_soil[[1]]$N_input <- nfert_mg_n * 1e-6 # In Europe: 150-800 mg N m-2 yr-1 -> kg N m-2 yr-1 0.00045 (high N) or 0.00015 (low N) # forcing from data
  
  # turn off N fixation
  df_drivers$params_species[[1]]$Nfixrate0 = 0.0
  
  df_calib_DBH_gs <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  return(df_calib_DBH_gs)
}

out_nfert <- purrr::map(
  as.list(c(150, 300, 500, 800)),
  ~get_outputs(df_drivers, .)
)

get_steadystate <- function(tmp, var){
  tmp$data[[1]]$output_annual_tile |> 
    tail(1000) |> 
    pull(!!var) |> 
    mean()
}

df_nfert <- tibble(
  nfert = c(150, 300, 500, 800),
  plantc = purrr::map_dbl(
    out_nfert,
    ~get_steadystate(., "plantC")
    ),
  plantn = purrr::map_dbl(
    out_nfert,
    ~get_steadystate(., "plantN")
  ),
  nmin = purrr::map_dbl(
    out_nfert,
    ~get_steadystate(., "mineralN")
  ),
  nloss = purrr::map_dbl(
    out_nfert,
    ~get_steadystate(., "N_loss")
  )
  )

df_nfert |> 
  tidyr::pivot_longer(2:5, names_to = "var", values_to = "val") |> 
  ggplot(aes(nfert, val)) +
  geom_point(size = 3) +
  facet_wrap(~var, scales = "free_y")

# simulations with single N-input level
df_calib_DBH_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

# N uptake equilibrates 
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = N_uptk))

# N losses (g N m-2 yr-1)
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  tail(1000) |> 
  ggplot() +
  geom_line(aes(x = year, y = N_loss))

# mineral N
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  tail(1000) |> 
  ggplot() +
  geom_line(aes(x = year, y = mineralN))

# Plant N equilibrates
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantN))

# Total (Plant + Soil) N equilibrates (g N m-2)
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = totN))

# N residence time based on inputs
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  tail(1000) |> 
  ggplot() +
  geom_line(aes(x = year, y = totN/(df_drivers$init_soil[[1]]$N_input * 1e3)))

# soil N
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = slowSoilN + fastSoilN))

# soil C
df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = SlowSOM + fastSOM))

N0_B <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  labs(title = "N 0") + 
  annotate("text", x = 1000, y = 12, 
           label = "K_nitrogen = 0.00001 \netaN = 0.000001 \nN_input = 0 mg N/m2/yr \ninit_Nmineral = 0.015 kg N/m2 \ninit_slow_soil_C = 0 kg C/m2")+
  theme_classic()+labs(x = "Year", y = "plantC") 
N0_B

N0_G <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = NPP)) +
  labs(title = "N 0") + 
  annotate("text", x = 1000, y = 0.7, 
           label = "K_nitrogen = 0.00001 \netaN = 0.000001 \nN_input = 0 mg N/m2/yr \ninit_Nmineral = 0.015 kg N/m2 \ninit_slow_soil_C = 0 kg C/m2")+
  theme_classic()+labs(x = "Year", y = "NPP")
N0_G

NL_B <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  labs(title = "N low") + 
  annotate("text", x = 1000, y = 12, 
           label = "K_nitrogen = 0.00001 \netaN = 0.000001 \nN_input = 150 mg N/m2/yr \ninit_Nmineral = 0.015 kg N/m2 \ninit_slow_soil_C = 0 kg C/m2")+
  theme_classic()+labs(x = "Year", y = "plantC") 
NL_B

NL_G <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = NPP)) +
  labs(title = "N low") + 
  annotate("text", x = 1000, y = 0.7, 
           label = "K_nitrogen = 0.00001 \netaN = 0.000001 \nN_input = 150 mg N/m2/yr \ninit_Nmineral = 0.015 kg N/m2 \ninit_slow_soil_C = 0 kg C/m2")+
  theme_classic()+labs(x = "Year", y = "NPP")
NL_G

NH_B <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  labs(title = "N high") + 
  annotate("text", x = 1000, y = 12, 
           label = "K_nitrogen = 0.00001 \netaN = 0.000001 \nN_input = 450 mg N/m2/yr \ninit_Nmineral = 0.5 kg N/m2 \ninit_slow_soil_C = 40 kg C/m2")+
  theme_classic()+labs(x = "Year", y = "plantC") 
NH_B

NH_G <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = NPP)) +
  labs(title = "N high") + 
  annotate("text", x = 1000, y = 0.7, 
           label = "K_nitrogen = 0.00001 \netaN = 0.000001 \nN_input = 450 mg N/m2/yr \ninit_Nmineral = 0.5 kg N/m2 \ninit_slow_soil_C = 40 kg C/m2")+
  theme_classic()+labs(x = "Year", y = "NPP")
NH_G

N0_B + N0_G + NL_B + NL_G + NH_B + NH_G + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm")) #+
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave("~/GFDY/manuscript/figures/fig_N_inputs_param.png", width = 10, height = 8, dpi=300)

# Post
df_mod <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>%  
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- df_calib_DBH_gs$data[[1]]$output_annual_cohorts %>%
  dplyr::filter(year > df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% 
  mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins, year) %>% 
  summarise(nTrees=sum(density)) %>% 
  ungroup() %>% 
  group_by(size_bins) %>% 
  summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,
                  df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])
) %>% dplyr::left_join(ddf_obs, by = "variables")

ggplot(dff) +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed")+
  geom_abline(col="grey") + ggtitle("After calibration - DBH gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

# Original simulations where run with the following set-up: ####
# 1. turn OFF N limitation: (see vegetation_lm3ppa.mod.f90)
# spdata(i)%LAImax = MAX(LAImin, sp%LAI_light)
# 2. Parameters controlling N losses: (see N_loss in vegetation_lm3ppa.mod.f90)
# K_nitrogen   = 8.0,   # mineral Nitrogen turnover rate
# etaN         = 0.025, # loss rate with runoff
# 3. Parameters controling N deposition (N_input) and initial mineral N (init_Nmineral) 
# and initial soil organic matter (init_slow_soil_C)
# init_slow_soil_C    = 0.0,    # initial slow soil C, kg C/m2
# init_Nmineral       = 0.015,  # Mineral nitrogen pool, (kg N/m2)
# N_input             = 0.0008  # annual N input to soil N pool, kgN m-2 yr-1

# N-unlimited vs. N-limited ####

# (A) if N supply >= N demand, absolutely no N limitation. 
# Set-up:
# 1. turn ON N limitation: (see vegetation_lm3ppa.mod.f90)
# spdata(i)%LAImax = MAX(LAImin, MIN(LAI_nitrogen, sp%LAI_light))
# 2. Parameters controlling N losses need to be set to 0: (see N_loss in vegetation_lm3ppa.mod.f90)
# K_nitrogen   = 0
# etaN         = 0
# 3. Parameters controlling N deposition (N_input) and initial mineral N (init_Nmineral) 
# and initial soil organic matter (init_slow_soil_C) need to be high
# init_slow_soil_C    = 40
# init_Nmineral       = 0.5
# N_input             = 0.0008 

# (B) If N supply <  N demand, then allocation shifts to wood tissues to save N and optimally use C, 
# it does not consequently lead to low biomass, because when N supply is slightly lower than N demand, 
# high allocation to wood can get high biomass compared to the fixed allocation scheme.
# A reference here: https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.12783
# Set-up:
# 1. turn ON N limitation: (see vegetation_lm3ppa.mod.f90)
# spdata(i)%LAImax = MAX(LAImin, MIN(LAI_nitrogen, sp%LAI_light))
# 2. Parameters controlling N losses can be higher than 0: (see N_loss in vegetation_lm3ppa.mod.f90)
# K_nitrogen   = 0.05
# etaN         = 0
# 3. Parameters controlling N deposition (N_input) and initial mineral N (init_Nmineral) 
# and initial soil organic matter (init_slow_soil_C) need to be low
# init_slow_soil_C    = 0
# init_Nmineral       = 0.002
# N_input             = 0.0008 

# (C) If N supply << N demand, low leaves, low NPP, and low biomass
# Set-up:
# 1. turn ON N limitation: (see vegetation_lm3ppa.mod.f90)
# spdata(i)%LAImax = MAX(LAImin, MIN(LAI_nitrogen, sp%LAI_light))
# 2. Parameters controlling N losses can be higher than 0: (see N_loss in vegetation_lm3ppa.mod.f90)
# K_nitrogen   = 8
# etaN         = 0.025
# 3. Parameters controling N deposition (N_input) and initial mineral N (init_Nmineral) 
# and initial soil organic matter (init_slow_soil_C) need to be low
# init_slow_soil_C    = 6
# init_Nmineral       = 0.002
# N_input             = 0.0008 

# N closed runs ####
# with different total N in the ecosystem would be easier to understand for reviewers and readers
# 1. all the input and output are zero 
# K_nitrogen = 0
# etaN = 0
# N_input = 0
# 2. High or low values are given to total N in the ecosystem:
# Unlimited (high values)
# init_slow_soil_C    = 40
# init_Nmineral       = 0.5
# N_input             = 0.0008 
# Limited (low values)
# init_slow_soil_C    = 0
# init_Nmineral       = 0.002
# N_input             = 0.0008 

# DBH mortality ####
# DBH mortality has the shape params: p1=1.5, p2=2.5, p3=4.0
load("~/GFDY/data/inputs_mod/df_drivers_DBH_gs.RData")
load("~/GFDY/data/inputs_mod/settings_calib_DBH_gs_uniq_euler.RData")
df_drivers$params_siml[[1]]$method_mortality
df_drivers$params_siml[[1]]$method_photosynth

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_DBH_gs$par_opt["phiRL"]  
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_DBH_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_DBH_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_DBH_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_DBH_gs$par_opt["par_mort_under"]

# run the model N unlimited ####
# df_drivers$params_siml[[1]]$do_closedN_run <- FALSE
df_drivers$params_tile[[1]]$K_nitrogen <- 0
df_drivers$params_tile[[1]]$etaN <- 0
df_drivers$init_soil[[1]]$init_Nmineral <- 0.5 # kg N/m2 really high
df_drivers$init_soil[[1]]$init_slow_soil_C <- 40 # kg C/m2 really high
df_drivers$init_soil[[1]]$N_input <- 0

# run the model N limited ####
#df_drivers$params_siml[[1]]$do_closedN_run <- TRUE
df_drivers$params_tile[[1]]$K_nitrogen <- 0.01   # 0 for N-closed / 0.01 for no No-closed
df_drivers$params_tile[[1]]$etaN <- 0
df_drivers$init_soil[[1]]$init_Nmineral <- 0.002 # kg N/m2 low
df_drivers$init_soil[[1]]$init_slow_soil_C <- 0.002 # kg C/m2 low
df_drivers$init_soil[[1]]$N_input <- 0

df_calib_DBH_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  labs(title = "N limited N-open") + 
  #annotate("text", x = 1000, y = 12, label = "K_nitrogen = 0.01 \netaN = 0 \ninit_Nmineral = 0.002 \ninit_slow_soil_C = 0.002 \nN_input = 0")+
  theme_classic()+labs(x = "Year", y = "plantC") 

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = NPP)) +
  theme_classic()+labs(x = "Year", y = "NPP")

# DBH p1 = 1.5 ####

# N-closed and N unlimited
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts.csv")

# N-closed and N limited
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nlim_out_annual_cohorts.csv")

# N limited (no N-closed)
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_Nlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_Nlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_Nlim_out_annual_cohorts.csv")

# DBH p2 = 2.5 ####

# N-closed and N unlimited
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts.csv")

# N-closed and N limited
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nlim_out_annual_cohorts.csv")

# N limited (no N-closed)
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_Nlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_Nlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_Nlim_out_annual_cohorts.csv")

# DBH p3 = 4.0 ####

# N-closed and N unlimited
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts.csv")

# N-closed and N limited
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nlim_out_annual_cohorts.csv")

# N limited (no N-closed)
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_Nlim_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_Nlim_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_Nlim_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_Nlim_out_annual_cohorts.csv")

# Figures ####

# N-closed and N unlimited
ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile.csv")
ea1sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts.csv")
ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile.csv")
ea1sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts.csv")
ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile.csv")
ea1sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts.csv")

ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile.csv")
ea2sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts.csv")
ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile.csv")
ea2sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts.csv")
ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile.csv")
ea2sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts.csv")

ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile.csv")
ea3sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nunlim_out_annual_cohorts.csv")
ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile.csv")
ea3sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nunlim_out_annual_cohorts.csv")
ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile.csv")
ea3sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nunlim_out_annual_cohorts.csv")

# N-closed and N limited
ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile.csv")
ea1sa1DBHp1gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_closedN_Nlim_out_annual_cohorts.csv")
ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile.csv")
ea1sa1DBHp2gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_closedN_Nlim_out_annual_cohorts.csv")
ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile.csv")
ea1sa1DBHp3gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_closedN_Nlim_out_annual_cohorts.csv")

ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile.csv")
ea2sa1DBHp1gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_closedN_Nlim_out_annual_cohorts.csv")
ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile.csv")
ea2sa1DBHp2gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_closedN_Nlim_out_annual_cohorts.csv")
ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile.csv")
ea2sa1DBHp3gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_closedN_Nlim_out_annual_cohorts.csv")

ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile.csv")
ea3sa1DBHp1gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_closedN_Nlim_out_annual_cohorts.csv")
ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile.csv")
ea3sa1DBHp2gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_closedN_Nlim_out_annual_cohorts.csv")
ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile.csv")
ea3sa1DBHp3gl_closedN_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_closedN_Nlim_out_annual_cohorts.csv")

# N limited (no N-closed)
ea1sa1DBHp1gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_Nlim_out_annual_tile.csv")
ea1sa1DBHp1gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_Nlim_out_annual_cohorts.csv")
ea1sa1DBHp2gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_Nlim_out_annual_tile.csv")
ea1sa1DBHp2gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_Nlim_out_annual_cohorts.csv")
ea1sa1DBHp3gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_Nlim_out_annual_tile.csv")
ea1sa1DBHp3gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_Nlim_out_annual_cohorts.csv")

ea2sa1DBHp1gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_Nlim_out_annual_tile.csv")
ea2sa1DBHp1gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_Nlim_out_annual_cohorts.csv")
ea2sa1DBHp2gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_Nlim_out_annual_tile.csv")
ea2sa1DBHp2gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_Nlim_out_annual_cohorts.csv")
ea2sa1DBHp3gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_Nlim_out_annual_tile.csv")
ea2sa1DBHp3gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_Nlim_out_annual_cohorts.csv")

ea3sa1DBHp1gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_Nlim_out_annual_tile.csv")
ea3sa1DBHp1gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_Nlim_out_annual_cohorts.csv")
ea3sa1DBHp2gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_Nlim_out_annual_tile.csv")
ea3sa1DBHp2gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_Nlim_out_annual_cohorts.csv")
ea3sa1DBHp3gl_Nlim_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_Nlim_out_annual_tile.csv")
ea3sa1DBHp3gl_Nlim_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_Nlim_out_annual_cohorts.csv")


# Mortality formulations ####

# DBH
fig2a_dbh <- ggplot(data.frame(x = c(0, 1.2)), aes(x)) + 
  stat_function(fun = ~ 0.1*.x ^ 1.5, aes(colour = "r1")) + 
  stat_function(fun = ~ 0.1*.x ^ 2.5, aes(colour = "r2")) + 
  stat_function(fun = ~ 0.1*.x ^ 4.0, aes(colour = "r3")) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#0072B2", "#D55E00", "#009E73"),
                     labels =c(expression(paste(italic("r")[italic("S1")]~ "= 1.5")),
                               expression(paste(italic("r")[italic("S2")]~ "= 2.5")),
                               expression(paste(italic("r")[italic("S3")]~ "= 4.0")))) +
  labs(x='Diameter (m)', y='m',title=expression(paste("Mortality rate (", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.22, .76),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1.2), breaks=seq(0,1.2,0.5)) +
  scale_y_continuous(limits=c(0,0.15), breaks=c(0,0.06,0.12))  
fig2a_dbh

# Stand develop: Stand biomass vs. time ####

## N unlimited ####
fig2b_dbh_closedN_Nunlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) +  
  geom_line(data=ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) +  
  geom_line(data=ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) + 
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "B",title=expression(paste("Biomass (kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.23, .79),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.4, 'cm'),
                     legend.key.width = unit(1.5, "line"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,70),breaks=seq(0,60,30))
fig2b_dbh_closedN_Nunlim

## N limited ####
fig2b_dbh_closedN_Nlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) +  
  geom_line(data=ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) +  
  geom_line(data=ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) + 
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "B",title=expression(paste("Biomass (kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.23, .79),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.4, 'cm'),
                     legend.key.width = unit(1.5, "line"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,70),breaks=seq(0,60,30))
fig2b_dbh_closedN_Nlim

fig2b_dbh_Nlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) +  
  geom_line(data=ea1sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) +  
  geom_line(data=ea2sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) + 
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "B",title=expression(paste("Biomass (kg C ", m^-2, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.23, .79),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.4, 'cm'),
                     legend.key.width = unit(1.5, "line"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,70),breaks=seq(0,60,30))
fig2b_dbh_Nlim

# Growth (NPP) ####

## N unlimited #### 
fig2c_dbh_closedN_Nunlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) +
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "G",title=expression(paste("Growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,1))
fig2c_dbh_closedN_Nunlim

## N limited #### 
fig2c_dbh_closedN_Nlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) +
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "G",title=expression(paste("Growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,1))
fig2c_dbh_closedN_Nlim

fig2c_dbh_Nlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea1sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=NPP, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) +
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "G",title=expression(paste("Growth (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,1))
fig2c_dbh_Nlim

# Mortality (Both mortality and biomass turnover) ####

## N unlimited #### 
fig2d_dbh_closedN_Nunlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) +
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "M",title=expression(paste("Mortality (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1515),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,1))
fig2d_dbh_closedN_Nunlim

## N limited #### 
fig2d_dbh_closedN_Nlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) +
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "M",title=expression(paste("Mortality (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1515),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,1))
fig2d_dbh_closedN_Nlim

fig2d_dbh_Nlim <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea1sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea1sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='Control'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#0072B2", alpha=.8,linewidth=.6) + 
  geom_line(data=ea2sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea2sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+15%'), col="#009E73",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp1gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#0072B2",alpha=.8,linewidth=.6) + 
  geom_line(data=ea3sa1DBHp2gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#D55E00",alpha=.8,linewidth=.6) +
  geom_line(data=ea3sa1DBHp3gl_Nlim_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, linetype='+30%'), col="#009E73",alpha=.8,linewidth=.6) +
  scale_linetype_manual("Level of LUE", breaks = c('Control',"+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "M",title=expression(paste("Mortality (kg C ", m^-2, " ", yr^-1, ") "))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1515),breaks=seq(0,1500,750)) +
  scale_y_continuous(limits=c(0,2.5),breaks=seq(0,2.5,1))
fig2d_dbh_Nlim

# Relative change biomass (plantC) vs. NPP ####

## N unlimited #### 
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig2e_dbh_closedN_Nunlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#009E73",size=3) + 
  scale_shape_manual("Change in LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.25, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
fig2e_dbh_closedN_Nunlim

## N unlimited ####
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig2e_dbh_closedN_Nlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#009E73",size=3) + 
  scale_shape_manual("Change in LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.25, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
fig2e_dbh_closedN_Nlim

# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig2e_dbh_Nlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='0-15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='0-30%'),col="#009E73",size=3) + 
  scale_shape_manual("Change in LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.25, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
fig2e_dbh_Nlim

# Relative change mortality vs. NPP ####

## N unlimited #### 
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig2f_dbh_closedN_Nunlim <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dM, M)),title = "Changes in tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +  
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25))
fig2f_dbh_closedN_Nunlim

## N limited #### 
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig2f_dbh_closedN_Nlim <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dM, M)),title = "Changes in tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +  
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25))
fig2f_dbh_closedN_Nlim

# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig2f_dbh_Nlim <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dM, M)),title = "Changes in tree mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +  
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25))
fig2f_dbh_Nlim

# Relative change k vs. NPP ####

## N unlimited #### 
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig2g_dbh_closedN_Nunlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)), title = "Changes in C turnover rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.20), breaks = seq(0,0.2,0.1)) + 
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2g_dbh_closedN_Nunlim

## N limited #### 
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig2g_dbh_closedN_Nlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)), title = "Changes in C turnover rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.20), breaks = seq(0,0.2,0.1)) +  
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2g_dbh_closedN_Nlim

# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig2g_dbh_Nlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)), title = "Changes in C turnover rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.20), breaks = seq(0,0.2,0.1)) + 
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2g_dbh_Nlim

# Relative longevity vs. growth rates ####
# longevity is maximum age at tile-level, plot mean across multiple years after spinup

## N unlimited #### 
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp1gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp2gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp3gl_closedN_Nunlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig2h_dbh_closedN_Nunlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dL, L)),title = "Changes in tree longevity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = c(-0.2,-0.1,0)) +
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2h_dbh_closedN_Nunlim

## N limited #### 
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp1gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp2gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp3gl_closedN_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig2h_dbh_closedN_Nlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dL, L)),title = "Changes in tree longevity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = c(-0.2,-0.1,0)) +
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2h_dbh_closedN_Nlim

# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp1gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp2gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L30=mean(MaxAge))
L15 <- ea2sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L15=mean(MaxAge))
L0  <- ea1sa1DBHp3gl_Nlim_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(L0=mean(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig2h_dbh_Nlim <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, shape='+30%'),col="#009E73",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dL, L)),title = "Changes in tree longevity") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = c(-0.2,-0.1,0)) +
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2h_dbh_Nlim

# Figures ####
"K_nitrogen = 0.01 \netaN = 0 \ninit_Nmineral = 0.002 \ninit_slow_soil_C = 0.002 \nN_input = 0"

ff2_closedN_Nunlim <- fig2a_dbh + fig2b_dbh_closedN_Nunlim + fig2c_dbh_closedN_Nunlim + fig2d_dbh_closedN_Nunlim + 
  fig2e_dbh_closedN_Nunlim + fig2f_dbh_closedN_Nunlim + fig2g_dbh_closedN_Nunlim + fig2h_dbh_closedN_Nunlim + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")",
                  title = 'N-closed, N-unlimited',
                  #caption = 'K_nitrogen = 0.01 \netaN = 0 \ninit_Nmineral = 0.002 \ninit_slow_soil_C = 0.002 \nN_input = 0',
                  subtitle = 'K_nitrogen = 0; etaN = 0; init_Nmineral = 0.5; init_slow_soil_C = 40; N_input = 0') & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
ff2_closedN_Nunlim
#ggsave("~/GFDY/manuscript/figures/fig_2_closedN_Nunlim.png", width = 10.5, height = 5.5, dpi=300)
ggsave("~/GFDY/manuscript/figures/fig_2_closedN_Nunlim.png", width = 10.5, height = 6, dpi=300)

ff2_closedN_Nlim <- fig2a_dbh + fig2b_dbh_closedN_Nlim + fig2c_dbh_closedN_Nlim + fig2d_dbh_closedN_Nlim + 
  fig2e_dbh_closedN_Nlim + fig2f_dbh_closedN_Nlim + fig2g_dbh_closedN_Nlim + fig2h_dbh_closedN_Nlim + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")",
                  title = 'N-closed, N-limited',
                  #caption = 'K_nitrogen = 0.01 \netaN = 0 \ninit_Nmineral = 0.002 \ninit_slow_soil_C = 0.002 \nN_input = 0',
                  subtitle = 'K_nitrogen = 0; etaN = 0; init_Nmineral = 0.002; init_slow_soil_C = 0.002; N_input = 0') & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
ff2_closedN_Nlim
ggsave("~/GFDY/manuscript/figures/fig_2_closedN_Nlim.png", width = 10.5, height = 6, dpi=300)

ff2_Nlim <- fig2a_dbh + fig2b_dbh_Nlim + fig2c_dbh_Nlim + fig2d_dbh_Nlim + 
  fig2e_dbh_Nlim + fig2f_dbh_Nlim + fig2g_dbh_Nlim + fig2h_dbh_Nlim + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")",
                  title = 'N-open, N-limited',
                  #caption = 'K_nitrogen = 0.01 \netaN = 0 \ninit_Nmineral = 0.002 \ninit_slow_soil_C = 0.002 \nN_input = 0',
                  subtitle = 'K_nitrogen = 0.01; etaN = 0; init_Nmineral = 0.002; init_slow_soil_C = 0.002; N_input = 0') & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
ff2_Nlim
ggsave("~/GFDY/manuscript/figures/fig_2_openN_Nlim.png", width = 10.5, height = 6, dpi=300)
