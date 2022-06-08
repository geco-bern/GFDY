# This script plots the outputs from the model simulations

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(ggeffects)
library(patchwork)

# Read model outputs ####

# DBH Mortality gs-leuning 
ea1sa1DBHp1gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_out_annual_tile.csv")
ea1sa1DBHp1gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp1gl_out_annual_cohorts.csv")
ea1sa1DBHp2gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_out_annual_tile.csv")
ea1sa1DBHp2gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp2gl_out_annual_cohorts.csv")
ea1sa1DBHp3gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_out_annual_tile.csv")
ea1sa1DBHp3gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1DBHp3gl_out_annual_cohorts.csv")

ea2sa1DBHp1gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_out_annual_tile.csv")
ea2sa1DBHp1gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp1gl_out_annual_cohorts.csv")
ea2sa1DBHp2gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_out_annual_tile.csv")
ea2sa1DBHp2gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp2gl_out_annual_cohorts.csv")
ea2sa1DBHp3gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_out_annual_tile.csv")
ea2sa1DBHp3gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1DBHp3gl_out_annual_cohorts.csv")

ea3sa1DBHp1gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_out_annual_tile.csv")
ea3sa1DBHp1gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp1gl_out_annual_cohorts.csv")
ea3sa1DBHp2gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_out_annual_tile.csv")
ea3sa1DBHp2gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp2gl_out_annual_cohorts.csv")
ea3sa1DBHp3gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_out_annual_tile.csv")
ea3sa1DBHp3gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1DBHp3gl_out_annual_cohorts.csv")

# GR Mortality gs-leuning 
ea1sa1GRp1gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1GRp1gl_out_annual_tile.csv")
ea1sa1GRp1gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1GRp1gl_out_annual_cohorts.csv")
ea1sa1GRp2gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1GRp2gl_out_annual_tile.csv")
ea1sa1GRp2gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1GRp2gl_out_annual_cohorts.csv")
ea1sa1GRp3gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea1sa1GRp3gl_out_annual_tile.csv")
ea1sa1GRp3gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea1sa1GRp3gl_out_annual_cohorts.csv")

ea2sa1GRp1gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1GRp1gl_out_annual_tile.csv")
ea2sa1GRp1gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1GRp1gl_out_annual_cohorts.csv")
ea2sa1GRp2gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1GRp2gl_out_annual_tile.csv")
ea2sa1GRp2gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1GRp2gl_out_annual_cohorts.csv")
ea2sa1GRp3gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea2sa1GRp3gl_out_annual_tile.csv")
ea2sa1GRp3gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea2sa1GRp3gl_out_annual_cohorts.csv")

ea3sa1GRp1gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1GRp1gl_out_annual_tile.csv")
ea3sa1GRp1gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1GRp1gl_out_annual_cohorts.csv")
ea3sa1GRp2gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1GRp2gl_out_annual_tile.csv")
ea3sa1GRp2gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1GRp2gl_out_annual_cohorts.csv")
ea3sa1GRp3gl_out_annual_tile    <- read.csv("~/GFDY/data/outputs_mod/ea3sa1GRp3gl_out_annual_tile.csv")
ea3sa1GRp3gl_out_annual_cohorts <- read.csv("~/GFDY/data/outputs_mod/ea3sa1GRp3gl_out_annual_cohorts.csv")

# Exploring relationships and plotting

# Mortality formulations ####

# DBH
fig2a_dbh <- ggplot(data.frame(x = c(0, 1.2)), aes(x)) + 
  stat_function(fun = ~ 0.12*.x ^ 1.5, aes(colour = "r1")) + 
  stat_function(fun = ~ 0.12*.x ^ 2.5, aes(colour = "r2")) + 
  stat_function(fun = ~ 0.12*.x ^ 4.0, aes(colour = "r3")) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("S1")]~ "= 1.5")),
                               expression(paste(italic("r")[italic("S2")]~ "= 2.5")),
                               expression(paste(italic("r")[italic("S3")]~ "= 4.0")))) +
  labs(x='DBH', y='m',title="Mortality rate") + 
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
  scale_y_continuous(limits=c(0,0.15), breaks=seq(0,0.15,0.05)) 
fig2a_dbh

# GR
fig3a_gr <- ggplot(data.frame(x = c(0, 20)), aes(x)) + 
  stat_function(fun = ~ 0.12/(1+(exp(-0.5*(.x-10)))), aes(colour = "r1")) +
  stat_function(fun = ~ 0.12/(1+(exp(-0.8*(.x-10)))), aes(colour = "r2")) +
  stat_function(fun = ~ 0.12/(1+(exp(-1.4*(.x-10)))), aes(colour = "r3")) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("GR1")]~ "= -0.5")),
                               expression(paste(italic("r")[italic("GR2")]~ "= -0.8")),
                               expression(paste(italic("r")[italic("GR3")]~ "= -1.4")))) +
  labs(x='GR', y='m',title="Mortality rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.25, .76),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,20), breaks=seq(0,20,5)) +
  scale_y_continuous(limits=c(0,0.13),breaks=seq(0,0.15,0.05))
fig3a_gr

# Stand develop: Stand biomass vs. time ####

# DBH
fig2b_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#009E73", alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#0072B2",alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E00",alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#009E73",alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#0072B2",alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E00",alpha=.7) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "B",title="Total biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.22, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig2b_dbh

# Changes in biomass
B_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea1p1
B_DBH_ea2p1
B_DBH_ea3p1
B_DBH_ea2p1/B_DBH_ea1p1
B_DBH_ea3p1/B_DBH_ea1p1

B_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p2/B_DBH_ea1p2
B_DBH_ea3p2/B_DBH_ea1p2

B_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_DBH_ea2p3/B_DBH_ea1p3
B_DBH_ea3p3/B_DBH_ea1p3

# GR
fig3b_gr <- ggplot() + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#009E73", alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#0072B2",alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'), col="#D55E00",alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#009E73",alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#0072B2",alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'), col="#D55E00",alpha=.7) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "t", y = "B",title="Total biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.78, .22),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,65),breaks=seq(0,60,15))
fig3b_gr

# Changes in biomass
B_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2/B_GR_ea1p2
B_GR_ea3p2/B_GR_ea1p2

B_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea2p2/B_GR_ea1p2
B_GR_ea3p2/B_GR_ea1p2

B_GR_ea1p3 <- ea1sa1GRp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea2p3 <- ea2sa1GRp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea3p3 <- ea3sa1GRp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(plantC))
B_GR_ea2p3/B_GR_ea1p3
B_GR_ea3p3/B_GR_ea1p3

# Growth (NPP) ####

# DBH
fig2c_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid")) +
  labs(x = "t", y = "G",title="Net Primary Productivity") + 
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
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))
fig2c_dbh

# Changes in growth
G_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p1/G_DBH_ea1p1
G_DBH_ea3p1/G_DBH_ea1p1

G_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p2/G_DBH_ea1p2
G_DBH_ea3p2/G_DBH_ea1p2

G_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_DBH_ea2p3/G_DBH_ea1p3
G_DBH_ea3p3/G_DBH_ea1p3

# GR
fig3c_gr <- ggplot() + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=NPP, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=NPP, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=NPP, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid")) +
  labs(x = "t", y = "G",title="Net Primary Productivity") + 
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
  scale_x_continuous(limits=c(0,1520),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))
fig3c_gr

# Changes in growth
G_GR_ea1p1 <- ea1sa1GRp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea2p1 <- ea2sa1GRp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea3p1 <- ea3sa1GRp1gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea2p1/G_GR_ea1p1
G_GR_ea3p1/G_GR_ea1p1

G_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea2p2/G_GR_ea1p2
G_GR_ea3p2/G_GR_ea1p2

G_GR_ea1p3 <- ea1sa1GRp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea2p3 <- ea2sa1GRp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea3p3 <- ea3sa1GRp3gl_out_annual_tile %>% filter(year>=800) %>% summarise(meanB=mean(NPP))
G_GR_ea2p3/G_GR_ea1p3
G_GR_ea3p3/G_GR_ea1p3

# Mortality (Both mortality and biomass turnover) ####

# DBH
fig2d_dbh <- ggplot() + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid")) +
  labs(x = "t", y = "M",title="Tree mortality") + 
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
  scale_x_continuous(limits=c(0,1502),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))
fig2d_dbh

# GR
fig3d_gr <- ggplot() + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=c_deadtrees+m_turnover, color='x3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_linetype_manual("Level of LUE", breaks = c("+15%", "+30%"), 
                        values = c("dashed","solid")) +
  labs(x = "t", y = "M",title="Tree mortality") + 
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
  scale_x_continuous(limits=c(0,1501),breaks=seq(0,1500,500)) +
  scale_y_continuous(limits=c(0,2.7),breaks=seq(0,2.5,0.5))
fig3d_gr

# Relative change biomass (plantC) vs. NPP ####

#DBH
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig2e_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#009E73",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#D55E00",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.22, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.1)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.1)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
fig2e_dbh

# GR
# GR p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# GR p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# GR p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

GRp3gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
GRp3gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
GRp3gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

fig3e_gr <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=GRp1gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#009E73",size=3) + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#009E73",size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#0072B2",size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#0072B2",size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_15, aes(x=dNPP0_15, y=dB0_15, shape='+15%'),col="#D55E00",size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_30, aes(x=dNPP0_30, y=dB0_30, shape='+30%'),col="#D55E00",size=3) + 
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6))) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Changes in biomass") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.22, .81),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.1)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.1))
fig3e_gr

# Relative change mortality vs. NPP ####

# DBH
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig2f_dbh <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x3', shape='0-30%'),size=3) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3", "x4"), 
                     values = c("#009E73", "#0072B2", "#D55E00","#D55E80")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
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
  scale_x_continuous(limits = c(0,0.5)) + 
  scale_y_continuous(limits = c(0,0.5))
fig2f_dbh

# GR
# GR 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# GR p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) 
M15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

# GR p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  #NPP or A_NPP
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

M30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M30=mean(c_deadtrees+m_turnover)) #plantC or A_Biomass
M15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M15=mean(c_deadtrees+m_turnover))
M0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(M0=mean(c_deadtrees+m_turnover))
dM0_15 <- (M15$M15 - M0$M0)/M0$M0
dM15_30 <- (M30$M30 - M15$M15)/M15$M15
dM0_30 <- (M30$M30 - M0$M0)/M0$M0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dM0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dM15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dM0_30)

fig3f_gr <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dM0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dM0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
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
  scale_x_continuous(limits = c(0,0.5)) + 
  scale_y_continuous(limits = c(0,0.5)) 
fig3f_gr

# Relative change k vs. NPP ####

# DBH
# DBH 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# DBH 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig2g_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x3', shape='0-30%'),size=3) +
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)), title = "Changes in C decay rate") + 
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
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(0,0.16), breaks = seq(0,0.15,0.05)) + 
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2g_dbh

# GR
# GR 1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# GR 2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

# GR 3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

k30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k30=mean(1/c_turnover_time)) 
k15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k15=mean(1/c_turnover_time))
k0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(k0=mean(1/c_turnover_time))
dk0_15 <- (k15$k15 - k0$k0)/k0$k0
dk15_30 <- (k30$k30 - k15$k15)/k15$k15
dk0_30 <- (k30$k30 - k0$k0)/k0$k0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dk0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dk15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dk0_30)

fig3g_gr <- ggplot() + 
  geom_hline(yintercept =  0.0, linetype="dashed") +
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dk0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dk0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dk, k)),title = "Changes in C decay rate") + 
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
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(0,0.16), breaks = seq(0,0.15,0.05))  
fig3g_gr

# Carbon Residence time vs. time ####
# Plot c_turnover_time or plantC/NPP

# DBH
fig2gg_dbh <- ggplot() +  
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='Control'),alpha=.7) + 
  geom_line(data=ea1sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='Control'),alpha=.7) +
  geom_line(data=ea1sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='Control'),alpha=.7) +
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1DBHp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1DBHp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("1")])),expression(paste(italic("r")[italic("2")])),
                               expression(paste(italic("r")[italic("3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Year", y = expression(paste(tau)), 
       title = "Carbon residence time with size-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.43, .9),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.7, 'cm'),
                     legend.spacing = unit(.1,"cm"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(10,35), breaks = seq(10,35,10))
fig2gg_dbh

# GR
fig3gg_gr <- ggplot() + 
  geom_line(data=ea1sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='Control'),alpha=.7) + 
  geom_line(data=ea1sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='Control'),alpha=.7) +
  geom_line(data=ea1sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='Control'),alpha=.7) +
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+15%'),alpha=.7) + 
  geom_line(data=ea2sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+15%'),alpha=.7) +
  geom_line(data=ea2sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+15%'),alpha=.7) +
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r1', linetype='+30%'),alpha=.7) + 
  geom_line(data=ea3sa1GRp2gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r2', linetype='+30%'),alpha=.7) +
  geom_line(data=ea3sa1GRp3gl_out_annual_tile, aes(x=year, y=plantC/NPP, color='r3', linetype='+30%'),alpha=.7) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Year", y = expression(paste(tau)), 
       title = "Carbon residence time with size-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.5, 'cm'),
                     legend.spacing = unit(.1,"cm"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(10,35), breaks = seq(10,35,10))
fig3gg_gr

# Relative longevity vs. growth rates ####
# longevity is maximum age at tile-level, plot mean across multiple years after spinup

# DBH
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=1300) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

DBHp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
DBHp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
DBHp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig2h_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
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
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = seq(-0.25,0,0.05)) +
  geom_hline(yintercept =  0.0, linetype="dashed")
fig2h_dbh

# GR
# GR p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1GRp1gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

GRp1gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
GRp1gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
GRp1gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# GR p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1GRp2gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

GRp2gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
GRp2gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
GRp2gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

# DBH p3
NPP30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=800) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

L30 <- ea3sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L30=max(MaxAge))
L15 <- ea2sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L15=max(MaxAge))
L0  <- ea1sa1GRp3gl_out_annual_tile %>% dplyr::filter(year>=1100) %>% summarise(L0=max(MaxAge))
dL0_15 <- (L15$L15 - L0$L0)/L0$L0
dL15_30 <- (L30$L30 - L15$L15)/L15$L15
dL0_30 <- (L30$L30 - L0$L0)/L0$L0

GRp3gl_RelChange_0_15 <- data.frame(dNPP0_15,dL0_15)
GRp3gl_RelChange_15_30 <- data.frame(dNPP15_30,dL15_30)
GRp3gl_RelChange_0_30 <- data.frame(dNPP0_30,dL0_30)

fig3h_gr <- ggplot() + 
  geom_hline(yintercept =  0.0, linetype="dashed") +
  geom_point(data=GRp1gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x1', shape='0-15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x1', shape='0-30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x2', shape='0-15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x2', shape='0-30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_15, aes(x=dNPP0_15, y=dL0_15, color='x3', shape='0-15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_0_30, aes(x=dNPP0_30, y=dL0_30, color='x3', shape='0-30%'),size=3) + 
  scale_color_manual("Parameter value", breaks = c("x1", "x2", "x3"), 
                     values = c("#009E73", "#0072B2", "#D55E00")) +
  scale_shape_manual("Level of LUE", breaks = c("0-15%","0-30%"), 
                     values = c(16,17)) +  
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
  scale_x_continuous(limits = c(0,0.50), breaks = seq(0,0.5,0.10)) + 
  scale_y_continuous(limits = c(-0.25,0), breaks = seq(-0.25,0,0.05)) 
fig3h_gr

# Distribution of tree sizes ####

# DBH
figdistr_dbh <- ggplot() +  
  geom_smooth(data=ea1sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='Control'),se=F,size=.5) + 
  geom_smooth(data=ea1sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea1sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea2sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+15%'),se=F,size=.5) + 
  geom_smooth(data=ea2sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea2sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1DBHp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+30%'),se=F,size=.5) + 
  geom_smooth(data=ea3sa1DBHp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+30%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1DBHp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+30%'),se=F,size=.5) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3", "r4"), 
                     values = c("#009E73", "#0072B2", "#D55E00", "#E69F00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("1")])),expression(paste(italic("r")[italic("2")])),
                               expression(paste(italic("r")[italic("3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Diameter (cm)", y = "Ln N", title = "Size distributions with size-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.45, .9),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.7, 'cm'),
                     legend.spacing = unit(.1,"cm"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  coord_cartesian(xlim = c(25,150),ylim = c(0,6)) +
  scale_y_continuous(breaks = seq(0,6,2)) + 
  scale_x_continuous(breaks = seq(25,150,25)) 
figdistr_dbh

# GR
figdistr_gr <- ggplot() +  
  geom_smooth(data=ea1sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='Control'),se=F,size=.5) + 
  geom_smooth(data=ea1sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea1sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='Control'),se=F,size=.5) +
  geom_smooth(data=ea2sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+15%'),se=F,size=.5) + 
  geom_smooth(data=ea2sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea2sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+15%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1GRp1gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r1', linetype='+30%'),se=F,size=.5) + 
  geom_smooth(data=ea3sa1GRp2gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r2', linetype='+30%'),se=F,size=.5) +
  geom_smooth(data=ea3sa1GRp3gl_out_annual_cohorts, aes(x=dbh, y=log(density), color='r3', linetype='+30%'),se=F,size=.5) +
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3", "r4"), 
                     values = c("#009E73", "#0072B2", "#D55E00", "#E69F00"),
                     guide = guide_legend(override.aes = list(size=.5),order = 1),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")])))) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","longdash","solid"),
                        guide = guide_legend(override.aes = list(color = "black", size=.5))) +
  labs(x = "Diameter (cm)", y = "Ln N", title = "Size distributions with growth rate-dependent mortality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = "none",
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(0.7, 'cm'),
                     legend.spacing = unit(.1,"cm"),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  coord_cartesian(xlim = c(25,300),ylim = c(0,6)) +
  scale_y_continuous(breaks = seq(0,6,2)) + 
  scale_x_continuous(breaks = seq(25,300,25)) 
figdistr_gr

# Self-thinning relationship  ####

# DBH
# DBH1
data_DBH_ea1p1 <- ea1sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "Control") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_DBH_ea2p1 <- ea2sa1DBHp1gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+15%")%>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_DBH_ea3p1 <- ea3sa1DBHp1gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+30%") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

# Select from each QMD bins the number of plots with higher density
valueQuantile = 0.75
quantileX <- data_DBH_ea1p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea1p1Den <- data_DBH_ea1p1 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_DBH_ea2p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea2p1Den <- data_DBH_ea2p1 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_DBH_ea3p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea3p1Den <- data_DBH_ea3p1 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)

data_DBH_p1 <- rbind(data_DBH_ea1p1, data_DBH_ea2p1, data_DBH_ea3p1) 
data_DBH_p1 <- data_DBH_p1 %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_DBH_p1$LUE <- relevel(data_DBH_p1$LUE, ref = "Control")
ggplot() +  geom_point(data = data_DBH_p1, aes(x = log(QMD), y = log(Density12)), color="blue")
data_DBH_p1Den <- rbind(data_DBH_ea1p1Den, data_DBH_ea2p1Den, data_DBH_ea3p1Den) 
data_DBH_p1Den <- data_DBH_p1Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_DBH_p1Den$LUE <- relevel(data_DBH_p1Den$LUE, ref = "Control")
ggplot() +  geom_point(data = data_DBH_p1Den, aes(x = log(QMD), y = log(Density12)), color="blue") 

ggplot() + 
  geom_point(data = data_DBH_p1, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p1Den, aes(x = log(QMD), y = log(Density12)), color="red") 

# STL model as LUE change
Fit_QMD = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p1, na.action = "na.exclude")
summary(Fit_QMD)
plot(allEffects(Fit_QMD))
plot_model(Fit_QMD, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))

pred <- ggpredict(Fit_QMD, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH1 <- as.data.frame(pred)

fig4aLUE_dbh1 <- ggplot() + 
  geom_point(data = data_DBH_p1, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH1, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size mortality - ", italic("r")[italic("S1")])),
       color  = "Biomass", linetype = "Level of LUE") + 
  scale_color_viridis_c(direction = -1,limits=c(27,51),breaks=seq(30,50,5)) +
  #scale_colour_scico(direction = -1,palette = "bamako",limits=c(28,39),breaks=c(30,35)) +
  scale_linetype_manual("Level of LUE", 
                        breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"),order=2)) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10),axis.title = element_text(size = 10),
          legend.text = element_text(size = 9),legend.title = element_text(size = 9),
          plot.title = element_text(size = 10),
          legend.key = element_rect(fill = NA, color = NA),
          legend.position = c(.68, .74),
          legend.direction="vertical",
          legend.box = "horizontal",
          legend.margin = margin(2, 2, 2, 2),
          legend.key.size = unit(.55, 'cm'),
          legend.key.height = unit(.4, 'cm'),
          #legend.box.background = element_rect(color="black",size=0.2),
          legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5)) 
fig4aLUE_dbh1

# DBH2
data_DBH_ea1p2 <- ea1sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "Control") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30))

data_DBH_ea2p2 <- ea2sa1DBHp2gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+15%")%>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30))

data_DBH_ea3p2 <- ea3sa1DBHp2gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+30%") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30))

# Select from each QMD bins the number of plots with higher density
valueQuantile = 0.75
quantileX <- data_DBH_ea1p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea1p2Den <- data_DBH_ea1p2 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_DBH_ea2p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea2p2Den <- data_DBH_ea2p2 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_DBH_ea3p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea3p2Den <- data_DBH_ea3p2 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)

data_DBH_p2 <- rbind(data_DBH_ea1p2, data_DBH_ea2p2, data_DBH_ea3p2) 
data_DBH_p2 <- data_DBH_p2 %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_DBH_p2$LUE <- relevel(data_DBH_p2$LUE, ref = "Control")
ggplot() +  geom_point(data = data_DBH_p2, aes(x = log(QMD), y = log(Density12)), color="blue")
data_DBH_p2Den <- rbind(data_DBH_ea1p2Den, data_DBH_ea2p2Den, data_DBH_ea3p2Den) 
data_DBH_p2Den <- data_DBH_p2Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_DBH_p2Den$LUE <- relevel(data_DBH_p2Den$LUE, ref = "Control")
ggplot() +  geom_point(data = data_DBH_p2Den, aes(x = log(QMD), y = log(Density12)), color="blue") 
ggplot() + 
  geom_point(data = data_DBH_p2, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p2Den, aes(x = log(QMD), y = log(Density12)), color="red") 

# STL model as LUE change
Fit_QMD = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p2, na.action = "na.exclude")
summary(Fit_QMD)
plot(allEffects(Fit_QMD))
plot_model(Fit_QMD, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))

pred <- ggpredict(Fit_QMD, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH2 <- as.data.frame(pred)

fig4bLUE_dbh2 <- ggplot() + 
  geom_point(data = data_DBH_p2, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE,show.legend = FALSE) +
  geom_smooth(data= preddataLUE_DBH2, aes(x=x, y=predicted, linetype=group),col="#0072B2",
              method = "lm",fullrange = T,size = .6, se=F,show.legend = FALSE) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size mortality - ", italic("r")[italic("S2")])),
       color  = "B", linetype = "Level of LUE") + 
  scale_color_viridis_c(direction = -1,limits=c(27,51),breaks=seq(30,50,5)) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"),order=2)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.75, .80),
                     legend.direction="horizontal",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5))
fig4bLUE_dbh2

# DBH3
data_DBH_ea1p3 <- ea1sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "Control") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_DBH_ea2p3 <- ea2sa1DBHp3gl_out_annual_tile %>%
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+15%")%>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_DBH_ea3p3 <- ea3sa1DBHp3gl_out_annual_tile %>% 
  mutate(Mortality = "DBH") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+30%") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

# Select from each QMD bins the number of plots with higher density
valueQuantile = 0.75
quantileX <- data_DBH_ea1p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea1p3Den <- data_DBH_ea1p3 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_DBH_ea2p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea2p3Den <- data_DBH_ea2p3 %>% left_join(quantileX)  %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_DBH_ea3p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_DBH_ea3p3Den <- data_DBH_ea3p3 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)

data_DBH_p3 <- rbind(data_DBH_ea1p3, data_DBH_ea2p3, data_DBH_ea3p3) 
data_DBH_p3 <- data_DBH_p3 %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_DBH_p3$LUE <- relevel(data_DBH_p3$LUE, ref = "Control")
ggplot() + geom_point(data = data_DBH_p3, aes(x = log(QMD), y = log(Density12)), color="blue")  
data_DBH_p3Den <- rbind(data_DBH_ea1p3Den, data_DBH_ea2p3Den, data_DBH_ea3p3Den) 
data_DBH_p3Den <- data_DBH_p3Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_DBH_p3Den$LUE <- relevel(data_DBH_p3Den$LUE, ref = "Control")
ggplot() + geom_point(data = data_DBH_p3Den, aes(x = log(QMD), y = log(Density12)), color="blue")  
ggplot() + 
  geom_point(data = data_DBH_p3, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_DBH_p3Den, aes(x = log(QMD), y = log(Density12)), color="red") 

# STL model as LUE change
Fit_QMD = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_DBH_p3, na.action = "na.exclude")
summary(Fit_QMD)
plot(allEffects(Fit_QMD))
plot_model(Fit_QMD, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))

pred <- ggpredict(Fit_QMD, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_DBH3 <- as.data.frame(pred)

fig4cLUE_dbh3 <- ggplot() + 
  geom_point(data = data_DBH_p3, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE,show.legend = F) +
  geom_smooth(data= preddataLUE_DBH3, aes(x=x, y=predicted, linetype=group),col="#D55E00",
              method = "lm",fullrange = T,size = .6, se=F,show.legend = F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Size mortality - ", italic("r")[italic("S3")])),
       color  = "B", linetype = "Level of LUE") + 
  scale_color_viridis_c(direction = -1,limits=c(27,51),breaks=seq(30,50,5)) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"),order=2)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.75, .80),
                      legend.direction="horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.2,4.4),breaks = seq(3.2,4.6,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.8),breaks = seq(4.5,7,.5))
fig4cLUE_dbh3

# GR

# GR1
data_GR_ea1p1 <- ea1sa1GRp1gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "Control") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_GR_ea2p1 <- ea2sa1GRp1gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+15%")%>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_GR_ea3p1 <- ea3sa1GRp1gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p1") %>% 
  mutate(LUE = "+30%") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

# Select from each QMD bins the number of plots with higher density
valueQuantile = 0.75
quantileX <- data_GR_ea1p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea1p1Den <- data_GR_ea1p1 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_GR_ea2p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea2p1Den <- data_GR_ea2p1 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_GR_ea3p1 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea3p1Den <- data_GR_ea3p1 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)

data_GR_p1 <- rbind(data_GR_ea1p1, data_GR_ea2p1, data_GR_ea3p1) 
data_GR_p1 <- data_GR_p1 %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_GR_p1$LUE <- relevel(data_GR_p1$LUE, ref = "Control")
ggplot() +  geom_point(data = data_GR_p1, aes(x = log(QMD), y = log(Density12)), color="blue")
data_GR_p1Den <- rbind(data_GR_ea1p1Den, data_GR_ea2p1Den, data_GR_ea3p1Den) 
data_GR_p1Den <- data_GR_p1Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_GR_p1Den$LUE <- relevel(data_GR_p1Den$LUE, ref = "Control")
ggplot() +  geom_point(data = data_GR_p1Den, aes(x = log(QMD), y = log(Density12)), color="blue") 
ggplot() + 
  geom_point(data = data_GR_p1, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p1Den, aes(x = log(QMD), y = log(Density12)), color="red") 

# STL model as LUE change
Fit_QMD = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p1, na.action = "na.exclude")
summary(Fit_QMD)
plot(allEffects(Fit_QMD))
plot_model(Fit_QMD, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))

pred <- ggpredict(Fit_QMD, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR1 <- as.data.frame(pred)

fig4aLUE_gr1 <- ggplot() + 
  geom_point(data = data_GR_p1, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR1, aes(x=x, y=predicted, linetype=group),col="#009E73",
              method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate mortality - ", italic("r")[italic("S1")])),
       color  = "Biomass", linetype = "Level of LUE") + 
  scale_color_viridis_c(direction = -1,limits=c(43,62),breaks=seq(45,60,5)) +
  scale_linetype_manual("Level of LUE", 
                        breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"),order=2)) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.68, .74),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.55, 'cm'),
                      legend.key.height = unit(.4, 'cm'),
                      #legend.box.background = element_rect(color="black",size=0.2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig4aLUE_gr1

# GR2
data_GR_ea1p2 <- ea1sa1GRp2gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "Control") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_GR_ea2p2 <- ea2sa1GRp2gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+15%")%>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_GR_ea3p2 <- ea3sa1GRp2gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p2") %>% 
  mutate(LUE = "+30%") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

# Select from each QMD bins the number of plots with higher density
valueQuantile = 0.75
quantileX <- data_GR_ea1p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea1p2Den <- data_GR_ea1p2 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_GR_ea2p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea2p2Den <- data_GR_ea2p2 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_GR_ea3p2 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea3p2Den <- data_GR_ea3p2 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)

data_GR_p2 <- rbind(data_GR_ea1p2, data_GR_ea2p2, data_GR_ea3p2) 
data_GR_p2 <- data_GR_p2 %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_GR_p2$LUE <- relevel(data_GR_p2$LUE, ref = "Control")
ggplot() +  geom_point(data = data_GR_p2, aes(x = log(QMD), y = log(Density12)), color="blue")
data_GR_p2Den <- rbind(data_GR_ea1p2Den, data_GR_ea2p2Den, data_GR_ea3p2Den) 
data_GR_p2Den <- data_GR_p2Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_GR_p2Den$LUE <- relevel(data_GR_p2Den$LUE, ref = "Control")
ggplot() +  geom_point(data = data_GR_p2Den, aes(x = log(QMD), y = log(Density12)), color="blue") 
ggplot() + 
  geom_point(data = data_GR_p2, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p2Den, aes(x = log(QMD), y = log(Density12)), color="red") 

# STL model as LUE change
Fit_QMD = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p2, na.action = "na.exclude")
summary(Fit_QMD)
plot(allEffects(Fit_QMD))
plot_model(Fit_QMD, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))

pred <- ggpredict(Fit_QMD, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR2 <- as.data.frame(pred)

fig4bLUE_gr2 <- ggplot() + 
  geom_point(data = data_GR_p2, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE,show.legend = FALSE) +
  geom_smooth(data= preddataLUE_GR2, aes(x=x, y=predicted, linetype=group),col="#0072B2",
              method = "lm",fullrange = T,size = .6, se=F,show.legend = FALSE) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate mortality - ", italic("r")[italic("GR2")])),
       color  = "B", linetype = "Level of LUE") + 
  scale_color_viridis_c(direction = -1,limits=c(43,62),breaks=seq(45,60,5)) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"),order=2)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.75, .80),
                     legend.direction="horizontal",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig4bLUE_gr2

# GR3
data_GR_ea1p3 <- ea1sa1GRp3gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "Control") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_GR_ea2p3 <- ea2sa1GRp3gl_out_annual_tile %>%
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+15%")%>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

data_GR_ea3p3 <- ea3sa1GRp3gl_out_annual_tile %>% 
  mutate(Mortality = "GR") %>% 
  mutate(Parameter = "p3") %>% 
  mutate(LUE = "+30%") %>% 
  filter(year>=1000) %>% 
  mutate(QMD_bins = cut(QMD, breaks = 30)) 

# Select from each QMD bins the number of plots with higher density
valueQuantile = 0.75
quantileX <- data_GR_ea1p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea1p3Den <- data_GR_ea1p3 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_GR_ea2p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea2p3Den <- data_GR_ea2p3 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)
quantileX <- data_GR_ea3p3 %>% group_by(QMD_bins) %>% summarise(quantile(Density12, c(valueQuantile))) 
data_GR_ea3p3Den <- data_GR_ea3p3 %>% left_join(quantileX) %>% filter(Density12>=`quantile(Density12, c(valueQuantile))`)

data_GR_p3 <- rbind(data_GR_ea1p3, data_GR_ea2p3, data_GR_ea3p3) 
data_GR_p3 <- data_GR_p3 %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_GR_p3$LUE <- relevel(data_GR_p3$LUE, ref = "Control")
ggplot() +  geom_point(data = data_GR_p3, aes(x = log(QMD), y = log(Density12)), color="blue")
data_GR_p3Den <- rbind(data_GR_ea1p3Den, data_GR_ea2p3Den, data_GR_ea3p3Den) 
data_GR_p3Den <- data_GR_p3Den %>% mutate(logDensity12=log(Density12),logQMD=log(QMD),LUE=as.factor(LUE))
data_GR_p3Den$LUE <- relevel(data_GR_p3Den$LUE, ref = "Control")
ggplot() +  geom_point(data = data_GR_p3Den, aes(x = log(QMD), y = log(Density12)), color="blue") 
ggplot() + 
  geom_point(data = data_GR_p3, aes(x = log(QMD), y = log(Density12)), color="blue") +
  geom_point(data = data_GR_p3Den, aes(x = log(QMD), y = log(Density12)), color="red") 

# STL model as LUE change
Fit_QMD = lm(logDensity12 ~ scale(logQMD) + LUE, data = data_GR_p3, na.action = "na.exclude")
summary(Fit_QMD)
plot(allEffects(Fit_QMD))
plot_model(Fit_QMD, type = "pred",show.data=TRUE, dot.size=1.5,terms = c("logQMD","LUE"))

pred <- ggpredict(Fit_QMD, terms = c("logQMD", "LUE"), full.data = TRUE)
plot(pred, add.data = F) 
preddataLUE_GR3 <- as.data.frame(pred)

fig4cLUE_gr3 <- ggplot() + 
  geom_point(data = data_GR_p3, aes(x = logQMD, y = logDensity12, col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE,show.legend = FALSE) +
  geom_smooth(data= preddataLUE_GR3, aes(x=x, y=predicted, linetype=group),col="#D55E00",
              method = "lm",fullrange = T,size = .6, se=F,show.legend = FALSE) +
  labs(x = "Ln QMD", y = "Ln N",title = expression(paste("Growth rate mortality - ", italic("r")[italic("GR3")])),
       color  = "B", linetype = "Level of LUE") + 
  scale_color_viridis_c(direction = -1,limits=c(43,62),breaks=seq(45,60,5)) +
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "black"),order=2)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 11),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.75, .80),
                     legend.direction="horizontal",
                     legend.margin = margin(2, 2, 2, 2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.2,6.3),breaks = seq(4.5,6.0,.5))
fig4cLUE_gr3

# Link model and observations ####
# Upward shift of the STL in the model

# DBH
# DBH1
preddataLUE_DBH1
preddataLUE_DBH1_agg <- preddataLUE_DBH1 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted)))

N30 <- preddataLUE_DBH1_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH1_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp1gl_RelChange_B_NPP_0_30 <- DBHp1gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp1gl_RelChange_B_NPP_0_15 <- DBHp1gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# DBH2
preddataLUE_DBH2
preddataLUE_DBH2_agg <- preddataLUE_DBH2 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted)))

N30 <- preddataLUE_DBH2_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH2_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp2gl_RelChange_B_NPP_0_30 <- DBHp2gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp2gl_RelChange_B_NPP_0_15 <- DBHp2gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# DBH3
preddataLUE_DBH3
preddataLUE_DBH3_agg <- preddataLUE_DBH3 %>% group_by(x) %>%  
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_DBH3_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_DBH3_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
DBHp3gl_RelChange_B_NPP_0_30 <- DBHp3gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)
DBHp3gl_RelChange_B_NPP_0_15 <- DBHp3gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)

# Upward shift of the STL from observations when increasing Growth 15 and 30%
load("~/GFDY/data/outputs_obs/aggData_preds.RData")
aggData_preds

N30 <- aggData_preds %>%
  summarise(STL_30=mean(STL_30,na.rm=T)) %>% pull()

N15 <- aggData_preds %>%
  summarise(STL_15=mean(STL_15,na.rm=T)) %>% pull()

fig5_dbh <- ggplot() + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r1', shape='+15%'),size=3) + 
  geom_point(data=DBHp1gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r1', shape='+30%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r2', shape='+15%'),size=3) + 
  geom_point(data=DBHp2gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r2', shape='+30%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r3', shape='+15%'),size=3) + 
  geom_point(data=DBHp3gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r3', shape='+30%'),size=3) + 
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("S1")])),expression(paste(italic("r")[italic("S2")])),
                               expression(paste(italic("r")[italic("S3")]))),
                    guide = guide_legend(override.aes = list(size=1.6),order = 1)) +
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color ="black",size=1.6))) +
  labs(x = expression(frac(dlnB, dlnG)), y = expression(frac(dN, N)),title = "Relative shifts in the STL for size-dependent mortality") + 
  theme_bw() + #guides(color = guide_legend(order = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.28, .9),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.01, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0.5,0.8),breaks=seq(0.5,0.8,0.1)) + 
  scale_y_continuous(limits = c(-0.001,0.2),breaks=seq(0,0.2,0.05)) +
  geom_hline(yintercept = N15, linetype="dashed") +
  geom_hline(yintercept = N30, linetype="dashed") +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=N15,ymax=N30),fill="grey", alpha=0.2)
fig5_dbh

# GR
# GR1
# Upward shift of the STL in the model
preddataLUE_GR1
preddataLUE_GR1_agg <- preddataLUE_GR1 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted)))

N30 <- preddataLUE_GR1_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR1_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp1gl_RelChange_B_NPP_0_15 <- GRp1gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp1gl_RelChange_B_NPP_0_30 <- GRp1gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR2
# Upward shift of the STL in the model
preddataLUE_GR2
preddataLUE_GR2_agg <- preddataLUE_GR2 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted)))  

N30 <- preddataLUE_GR2_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR2_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp2gl_RelChange_B_NPP_0_15 <- GRp2gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp2gl_RelChange_B_NPP_0_30 <- GRp2gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# GR3
# Upward shift of the STL in the model
preddataLUE_GR3
preddataLUE_GR3_agg <- preddataLUE_GR3 %>% group_by(x) %>% 
  mutate(STL_15=predicted-lag(predicted)) %>%
  mutate(STL_30=predicted-lag(lag(predicted))) 

N30 <- preddataLUE_GR3_agg %>%
  filter(group=="+30%") %>%
  ungroup(x) %>%
  summarise(STL_30=mean(STL_30)) %>% pull()

N15 <- preddataLUE_GR3_agg %>%
  filter(group=="+15%") %>%
  ungroup(x) %>%
  summarise(STL_15=mean(STL_15)) %>% pull()

# Relative change biomass (plantC) vs. NPP
GRp3gl_RelChange_B_NPP_0_15 <- GRp3gl_RelChange_B_NPP_0_15 %>%
  mutate(dlnB_dlnG = dB0_15/dNPP0_15) %>%
  mutate(N15=N15)
GRp3gl_RelChange_B_NPP_0_30 <- GRp3gl_RelChange_B_NPP_0_30 %>%
  mutate(dlnB_dlnG = dB0_30/dNPP0_30) %>%
  mutate(N30=N30)

# Upward shift of the STL from observations when increasing Growth 15 and 30%
load("~/GFDY/data/outputs_obs/aggData_preds.RData")
aggData_preds

N30 <- aggData_preds %>%
  summarise(STL_30=mean(STL_30,na.rm=T)) %>% pull()

N15 <- aggData_preds %>%
  summarise(STL_15=mean(STL_15,na.rm=T)) %>% pull()

fig5_gr <- ggplot() + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r1', shape='+15%'),size=3) + 
  geom_point(data=GRp1gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r1', shape='+30%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r2', shape='+15%'),size=3) + 
  geom_point(data=GRp2gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r2', shape='+30%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_15, aes(x=dlnB_dlnG, y=N15, color='r3', shape='+15%'),size=3) + 
  geom_point(data=GRp3gl_RelChange_B_NPP_0_30, aes(x=dlnB_dlnG, y=N30, color='r3', shape='+30%'),size=3) + 
  scale_color_manual("Parameter", breaks = c("r1", "r2", "r3"), 
                     values = c("#009E73", "#0072B2", "#D55E00"),
                     labels =c(expression(paste(italic("r")[italic("GR1")])),expression(paste(italic("r")[italic("GR2")])),
                               expression(paste(italic("r")[italic("GR3")]))),
                     guide = guide_legend(override.aes = list(size=1.6),order = 1)) +
  scale_shape_manual("Level of LUE", breaks = c("+15%","+30%"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color ="black",size=1.6))) +
  labs(x = expression(frac(dlnB, dlnG)), y = expression(frac(dN, N)),title = "Relative shifts in the STL for growth rate mortality") + 
  theme_bw() + #guides(color = guide_legend(order = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.30, .9),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.01, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(0.89,0.95),breaks=seq(0.89,0.95,0.01)) + 
  scale_y_continuous(limits = c(-0.001,0.2),breaks=seq(0,0.2,0.05)) +
  geom_hline(yintercept = N15, linetype="dashed") +
  geom_hline(yintercept = N30, linetype="dashed") +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=N15,ymax=N30),fill="grey", alpha=0.2)
fig5_gr

# Figure 2 ####
ff2 <- fig2a_dbh + fig2b_dbh + fig2c_dbh + fig2d_dbh + fig2e_dbh + fig2f_dbh + fig2g_dbh + fig2h_dbh + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff2
ggsave("~/GFDY/manuscript/figures/fig_2.png", width = 10.5, height = 5.5, dpi=300)

# Figure 3 ####
ff3 <- fig3a_gr + fig3b_gr + fig3c_gr + fig3d_gr + fig3e_gr + fig3f_gr + fig3g_gr + fig3h_gr + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff3
ggsave("~/GFDY/manuscript/figures/fig_3.png", width = 10.5, height = 5.5, dpi=300)

# Figure 4 ####
ff4 <- fig4aLUE_dbh1 + fig4bLUE_dbh2 + fig4cLUE_dbh3 + 
  fig4aLUE_gr1 + fig4bLUE_gr2 + fig4cLUE_gr3 + 
  plot_layout(ncol = 3) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff4
ggsave("~/GFDY/manuscript/figures/fig_4.png", width = 9.8, height = 6.3, dpi=300)

# Figure 5 ####
ff5 <- fig5_dbh + fig5_gr + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+  
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff5
ggsave("~/GFDY/manuscript/figures/fig_5.png", width = 8.5, height = 4.2, dpi=300)

# Figure S2 ####
ffs2 <- figdistr_dbh + figdistr_gr +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+ 
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffs2
ggsave("~/GFDY/manuscript/figures/fig_S2.png", width = 8.5, height = 4.2, dpi=300)

# Figure S3 ####
ffs3 <- fig2gg_dbh + fig3gg_gr + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ffs3
ggsave("~/GFDY/manuscript/figures/fig_S3.png",width = 8.5, height = 4.2, dpi=300)

# Box 1 ####
xx <- rep(0.5,3)
yy <- c(0,0.25,0.5)
oo <- as.factor(c("Constant self-thinning","Intermediate change","Constant decay rate"))
df <- data.frame(oo,xx,yy)
df
str(df)
levels(df$oo) 
df$oo <- factor(df$oo, levels = c("Constant decay rate","Intermediate change","Constant self-thinning"))

fig00a <- ggplot() + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(data=df, aes(x=xx, y=yy,shape=oo),col="black",size=3) + 
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Relative change in biomass per relative change in growth") + 
  scale_shape_manual(values = c(15, 1, 16)) + expand_limits(x = 0, y = 0) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text = element_blank(),axis.title = element_text(size = 9),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_blank(),
                     plot.title = element_text(size = 9),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.25, .85),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.9, 'lines'),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) + 
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +
  coord_cartesian(clip = 'off')
fig00a

fig00b <- ggplot() +
  geom_abline(slope=-1, intercept = 2.5, linetype="solid",alpha=0.8) +
  geom_abline(slope=-1, intercept = 2.5, linetype="dashed",color="darkgrey",alpha=0.8) +
  geom_abline(slope=-1, intercept = 2.7, linetype="solid",color="darkgrey") +
  geom_abline(slope=-1, intercept = 2.9, linetype="solid") +
  labs(x = "ln QMD", y = "ln N",title="Changes in the STL from changes in growth") + 
  scale_x_continuous(limits = c(0,3),breaks=seq(0,3,0.1),expand=c(0,0)) + 
  scale_y_continuous(limits = c(0,3),breaks=seq(0,3,0.1),expand=c(0,0)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text = element_blank(),axis.title = element_text(size = 9),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 8.5),legend.title = element_blank(),
                     plot.title = element_text(size = 9),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.20, .8),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.9, 'lines'),
                     legend.box.margin = margin(1, 1, 1, 1),
                     panel.border = element_rect(colour = "black", fill=NA,size=.9)) 
fig00b

ff_box <- fig00a + fig00b +
  plot_layout(ncol = 2)  +  plot_annotation(tag_levels = 'A') 
ff_box
ggsave("~/GFDY/manuscript/extra_figures/ff_box.png", width = 8, height = 4, dpi=300)

# Other figures ####
# Biomass for DBH param 1

B_DBH_1 <- ggplot() + 
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='Control'),alpha=1,col="#c43b3b",size = .6) + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'),alpha=1,col="#c43b3b",size = .6) + 
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'),alpha=1,col="#c43b3b",size = .6) + 
  scale_linetype_manual("Growth", 
                        breaks = c("Control","+15%", "+30%"),
                        labels = c("Low","Medium", "High"),
                        values = c("dotted","dashed","solid")) +
  labs(x = "Year", y = expression(paste("Biomass (Kg C ", m^-2, " ", yr^-1, ") ")),title = "For size-dependent mortality:",
       subtitle = "Total biomass",
       linetype = "Growth") +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      plot.subtitle  = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.80, .25),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.key.size = unit(.6, 'cm'),
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) 
B_DBH_1

# Biomass for GR param 1
B_GR_1 <- ggplot() + 
  geom_line(data=ea1sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='Control'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+15%'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC, linetype='+30%'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Year", y = expression(paste("Biomass (Kg C ", m^-2, " ", yr^-1, ") ")),title = "For growth rate-dependent mortality:",
       subtitle = "Total biomass",
       linetype = "Level of LUE") +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      plot.subtitle  = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.85, .25),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.key.size = unit(.6, 'cm'),
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) 
B_GR_1

# Carbon residence time for DBH param 1
CRT_DBH_1 <- ggplot() +  
  geom_line(data=ea1sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, linetype='Control'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  geom_line(data=ea2sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, linetype='+15%'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  geom_line(data=ea3sa1DBHp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, linetype='+30%'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Year", y = expression(paste("Carbon residence time ( ", tau, " , ", yr, ") ")),title = "",
       subtitle = "Carbon residence time") +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      plot.subtitle  = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.85, .25),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.key.size = unit(.6, 'cm'),
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(15,21), breaks = seq(10,30,2))
CRT_DBH_1

# Carbon residence time for GR param 1
CRT_GR_1 <- ggplot() +  
  geom_line(data=ea1sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, linetype='Control'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  geom_line(data=ea2sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, linetype='+15%'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  geom_line(data=ea3sa1GRp1gl_out_annual_tile, aes(x=year, y=plantC/NPP, linetype='+30%'),alpha=1,col="#c43b3b",size = .6,show.legend = F) + 
  scale_linetype_manual("Level of LUE", breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid")) +
  labs(x = "Year", y = expression(paste("Carbon residence time ( ", tau, " , ", yr, ") ")),title = "",
       subtitle = "Carbon residence time") +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      plot.subtitle  = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.85, .25),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.key.size = unit(.6, 'cm'),
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(450,1500), breaks = seq(500,1500,500)) + 
  scale_y_continuous(limits = c(19,27), breaks = seq(10,30,2))
CRT_GR_1

# STL for DBH param 1
STL_DBH_1 <- ggplot() + 
  geom_point(data = data_DBH_p1Den, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_DBH1, aes(x=x, y=predicted, linetype=group),col="#c43b3b",
              method = "lm",fullrange = T,size = .6, se=F,show.legend = F) +
  labs(x = "Quadratic Mean Diameter (ln QMD)", y = "Stand density (ln N)",
       color  = "Biomass", linetype = "Level of LUE",title = "",
       subtitle = "Size-density relationship") +
  scale_color_viridis_c(direction = -1,limits=c(27,41),breaks=seq(30,40,5)) +
  #scale_colour_scico(direction = -1,palette = "bamako",limits=c(28,39),breaks=c(30,35)) +
  scale_linetype_manual("Level of LUE", 
                        breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "#c43b3b"),order=2)) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      plot.subtitle  = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.85, .70),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.key.size = unit(.4, 'cm'),
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.4,4.0),breaks = seq(3.4,4.0,0.2)) + 
  scale_y_continuous(limits = c(4.8,6.3),breaks = seq(5,6,.5)) 
STL_DBH_1

# STL for GR param 1
STL_GR_1 <- ggplot() + 
  geom_point(data = data_GR_p1Den, aes(x = logQMD, y = logDensity12,col=plantC), alpha=0.5, size = 1, inherit.aes = FALSE) +
  geom_smooth(data= preddataLUE_GR1, aes(x=x, y=predicted, linetype=group),col="#c43b3b",
              method = "lm",fullrange = T,size = .6, se=F,show.legend = F) +
  labs(x = "Quadratic Mean Diameter (ln QMD)", y = "Stand density (ln N)",
       color  = "Biomass", linetype = "Level of LUE",title = "",
       subtitle = "Size-density relationship") +
  scale_color_viridis_c(direction = -1,limits=c(43,62),breaks=seq(45,60,5)) +
  #scale_colour_scico(direction = -1,palette = "bamako",limits=c(28,39),breaks=c(30,35)) +
  scale_linetype_manual("Level of LUE", 
                        breaks = c("Control","+15%", "+30%"), 
                        values = c("dotted","dashed","solid"),
                        guide = guide_legend(override.aes = list(color = "#c43b3b"),order=2)) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      plot.subtitle  = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.85, .70),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.key.size = unit(.4, 'cm'),
                      legend.margin = margin(2, 2, 2, 2),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(3.6,4.4),breaks = seq(3.6,4.4,0.2)) + 
  scale_y_continuous(limits = c(4.3,6.2),breaks = seq(4.5,6,.5))
STL_GR_1

# Figure
ff_stl <- B_DBH_1 + STL_DBH_1 + B_GR_1 + STL_GR_1 + 
  plot_layout(ncol = 2) 
ff_stl
ggsave("~/GFDY/manuscript/extra_figures/ff_stl.png", width = 7, height = 6, dpi=300)

ff_stl <- B_DBH_1 + STL_DBH_1 + CRT_DBH_1 + B_GR_1 + STL_GR_1 + CRT_GR_1 +
  plot_layout(ncol = 3) 
ff_stl
ggsave("~/GFDY/manuscript/extra_figures/ff_stl_crt.png", width = 9, height = 6, dpi=300)



