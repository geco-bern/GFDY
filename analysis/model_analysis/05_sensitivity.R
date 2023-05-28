# This script evaluates the model and compares the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)
library(patchwork)

# DBH mortality ####
# DBH mortality has the shape params: p1=1.5, p2=2.5, p3=4.0
load("~/GFDY/data/inputs_mod/df_drivers_DBH_gs.RData")
load("~/GFDY/data/inputs_mod/settings_calib_DBH_gs_uniq_euler.RData")
df_drivers$params_siml[[1]]$method_mortality

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_DBH_gs$par_opt["phiRL"]  
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_DBH_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_DBH_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_DBH_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_DBH_gs$par_opt["par_mort_under"]

load("~/GFDY/data/raw_mod/d_param_bio.rda")
d_param_b1_AG <- d_param %>% dplyr::filter(equation_n==3,parameter_id==1,component_n=="Aboveground",parameter=="b1") %>% rename(SPECIES=species, b1_AG=value)
summary(d_param_b1_AG$b1_AG)
b1_AG <- c(2.2,2.36,2.5)

# run the model ####
df_drivers$params_species[[1]]$thetaBM <- b1_AG[1]
df_drivers$params_species[[1]]$thetaBM <- b1_AG[3]

df_calib_DBH_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

# DBH p1 = 1.5 ####

# thetaBM1 = 2.2
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_thetaBM1_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_thetaBM1_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_thetaBM1_out_annual_cohorts.csv")

# thetaBM2 = 2.5
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_thetaBM2_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_thetaBM2_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_thetaBM2_out_annual_cohorts.csv")

# DBH p2 = 2.5 ####

# thetaBM1 = 2.2
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_thetaBM1_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_thetaBM1_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_thetaBM1_out_annual_cohorts.csv")

# thetaBM2 = 2.5
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_thetaBM2_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_thetaBM2_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_thetaBM2_out_annual_cohorts.csv")

# DBH p3 = 4.0 ####

# thetaBM1 = 2.2
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_thetaBM1_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_thetaBM1_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_thetaBM1_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_thetaBM1_out_annual_cohorts.csv")

# thetaBM2 = 2.5
# LUE control
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_thetaBM2_out_annual_cohorts.csv")
# LUE +15% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_thetaBM2_out_annual_cohorts.csv")
# LUE +30% (modified in the model)
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_thetaBM2_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_thetaBM2_out_annual_cohorts.csv")

# Read outputs ####
## Relative change biomass (plantC) vs. NPP ####

### DBH p1 ####
ea1sa1DBHp1gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_out_annual_tile.csv")
ea2sa1DBHp1gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_out_annual_tile.csv")
ea3sa1DBHp1gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_out_annual_tile.csv")
ea1sa1DBHp1gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_thetaBM1_out_annual_tile.csv")
ea2sa1DBHp1gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_thetaBM1_out_annual_tile.csv")
ea3sa1DBHp1gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_thetaBM1_out_annual_tile.csv")
ea1sa1DBHp1gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_thetaBM2_out_annual_tile.csv")
ea2sa1DBHp1gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_thetaBM2_out_annual_tile.csv")
ea3sa1DBHp1gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_thetaBM2_out_annual_tile.csv")

# DBH p1 - thetaBM0
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1thetaBM0gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1thetaBM0gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1thetaBM0gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p1 - thetaBM1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1thetaBM1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1thetaBM1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1thetaBM1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p1 - thetaBM2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1thetaBM2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1thetaBM2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1thetaBM2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

figS6A <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=DBHp1thetaBM0gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15,col="theta0", shape='0-15%'),size=3) + 
  geom_point(data=DBHp1thetaBM0gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30,col="theta0", shape='0-30%'),size=3) + 
  geom_point(data=DBHp1thetaBM1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15,col="theta1", shape='0-15%'),size=3) + 
  geom_point(data=DBHp1thetaBM1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30,col="theta1", shape='0-30%'),size=3) + 
  geom_point(data=DBHp1thetaBM2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15,col="theta2", shape='0-15%'),size=3) + 
  geom_point(data=DBHp1thetaBM2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30,col="theta2", shape='0-30%'),size=3) + 
  scale_color_viridis(discrete=TRUE,option="viridis",
                      "Parameter",breaks = c("theta1", "theta0", "theta2"),
                      labels =c(expression(paste(theta)[italic("1")]),
                                expression(paste(theta)[italic("2")]),
                                expression(paste(theta)[italic("3")])),
                      guide = guide_legend(override.aes = list(size=1.6),order=1)) +
#  scale_color_manual("Parameter", breaks = c("theta1", "theta0", "theta2"), 
#                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Change in LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6),order=2)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),
       title = expression(paste("G-B changes for ", italic("r")[italic("1")]))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.41, .83),
                     legend.direction="vertical",
                     legend.box = "horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.7, 'cm'),
                     legend.key.height = unit(.4, 'cm'),
                     legend.spacing = unit(0.5, 'cm'),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) 
figS6A

### DBH p2 ####
ea1sa1DBHp2gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_out_annual_tile.csv")
ea2sa1DBHp2gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_out_annual_tile.csv")
ea3sa1DBHp2gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_out_annual_tile.csv")
ea1sa1DBHp2gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_thetaBM1_out_annual_tile.csv")
ea2sa1DBHp2gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_thetaBM1_out_annual_tile.csv")
ea3sa1DBHp2gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_thetaBM1_out_annual_tile.csv")
ea1sa1DBHp2gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_thetaBM2_out_annual_tile.csv")
ea2sa1DBHp2gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_thetaBM2_out_annual_tile.csv")
ea3sa1DBHp2gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_thetaBM2_out_annual_tile.csv")

# DBH p2 - thetaBM0
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2thetaBM0gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2thetaBM0gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2thetaBM0gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2 - thetaBM1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2thetaBM1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2thetaBM1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2thetaBM1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2 - thetaBM2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2thetaBM2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2thetaBM2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2thetaBM2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

figS6B <- ggplot() + 
  geom_point(data=DBHp2thetaBM0gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%',col="theta0"),size=3) + 
  geom_point(data=DBHp2thetaBM0gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%',col="theta0"),size=3) + 
  geom_point(data=DBHp2thetaBM1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%',col="theta1"),size=3) + 
  geom_point(data=DBHp2thetaBM1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%',col="theta1"),size=3) + 
  geom_point(data=DBHp2thetaBM2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%',col="theta2"),size=3) + 
  geom_point(data=DBHp2thetaBM2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%',col="theta2"),size=3) + 
  scale_color_viridis(discrete=TRUE,option="viridis",
                      "Parameter",breaks = c("theta1", "theta0", "theta2"),
                      labels =c(expression(paste(theta)[italic("BM")]~ "= 2.20"),
                                expression(paste(theta)[italic("BM")]~ "= 2.36"),
                                expression(paste(theta)[italic("BM")]~ "= 2.50")))+
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),
       title = expression(paste("G-B changes for ", italic("r")[italic("2")]))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.box = "horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     legend.key.height = unit(.4, 'cm'),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
figS6B

### DBH p3 ####
ea1sa1DBHp3gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_out_annual_tile.csv")
ea2sa1DBHp3gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_out_annual_tile.csv")
ea3sa1DBHp3gl_thetaBM0_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_out_annual_tile.csv")
ea1sa1DBHp3gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_thetaBM1_out_annual_tile.csv")
ea2sa1DBHp3gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_thetaBM1_out_annual_tile.csv")
ea3sa1DBHp3gl_thetaBM1_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_thetaBM1_out_annual_tile.csv")
ea1sa1DBHp3gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_thetaBM2_out_annual_tile.csv")
ea2sa1DBHp3gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_thetaBM2_out_annual_tile.csv")
ea3sa1DBHp3gl_thetaBM2_out_annual_tile <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_thetaBM2_out_annual_tile.csv")

# DBH p3 - thetaBM0
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_thetaBM0_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3thetaBM0gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3thetaBM0gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3thetaBM0gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3 - thetaBM1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_thetaBM1_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3thetaBM1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3thetaBM1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3thetaBM1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3 - thetaBM2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_thetaBM2_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3thetaBM2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3thetaBM2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3thetaBM2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

figS6C <- ggplot() + 
  geom_point(data=DBHp3thetaBM0gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%',col="theta0"),size=3) + 
  geom_point(data=DBHp3thetaBM0gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%',col="theta0"),size=3) + 
  geom_point(data=DBHp3thetaBM1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%',col="theta1"),size=3) + 
  geom_point(data=DBHp3thetaBM1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%',col="theta1"),size=3) + 
  geom_point(data=DBHp3thetaBM2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%',col="theta2"),size=3) + 
  geom_point(data=DBHp3thetaBM2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%',col="theta2"),size=3) + 
  scale_color_viridis(discrete=TRUE,option="viridis",
                      "Parameter",breaks = c("theta1", "theta0", "theta2"),
                      labels =c(expression(paste(theta)[italic("BM")]~ "= 2.20"),
                                expression(paste(theta)[italic("BM")]~ "= 2.36"),
                                expression(paste(theta)[italic("BM")]~ "= 2.50")))+
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),
       title = expression(paste("G-B changes for ", italic("r")[italic("3")]))) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="vertical",
                     legend.box = "horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     legend.key.height = unit(.4, 'cm'),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
figS6C

# Figure S6 ####
fig_S6 <- figS6A + figS6B + figS6C +
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm")) & 
  theme(plot.tag = element_text(size = 12))#+ 
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
fig_S6
ggsave(paste0(here::here(),  "/manuscript/figures/fig_S6.png"), width = 9.5, height = 3.3, dpi=300)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S6.pdf"), width = 9.5, height = 3.3, dpi=300)
