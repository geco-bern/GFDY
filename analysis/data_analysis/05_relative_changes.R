
# load packages
library(dplyr)
library(ggplot2)
library(viridis)

# Relative change biomass (plantC) vs. NPP ####

# From Walker et al. 2021 ####
table2_walker <- readr::read_csv("~/GFDY/data/raw_obs/table2_walker.csv") |> 
  rename(X95CI_beta = `95CI_beta`)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)

table2_walker_B <- table2_walker %>% 
  filter(biomeE_var=="plantC") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         SD_beta = ifelse(is.na(SD_beta), mean(SD_beta/beta,na.rm=T), SD_beta))
         
table2_walker_G <- table2_walker %>% 
  filter(biomeE_var=="NPP") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))

# With loop ...
out <- data.frame()

for (n in 1:1e5){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = n, biomass = B_sample, growth = G_sample, ratio = B_sample / G_sample) |> 
    bind_rows(out)
}

# ... or function
sample_walker <- function(id, table2_walker_G, table2_walker_B){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = id, biomass = B_sample, growth = G_sample, ratio = B_sample / G_sample)
  
  return(out)
}

out <- purrr::map_dfr(
  as.list(seq(1e5)),
  ~sample_walker(., table2_walker_G, table2_walker_B)
)

write.csv(out, "~/GFDY/data/raw_obs/out_bootstrap.csv")

out <- read.csv("~/GFDY/data/raw_obs/out_bootstrap.csv")
gg1 <- out |> 
  ggplot(aes(growth, biomass)) +
  geom_hex() +
  scale_fill_gradientn(
    colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[G]), y = expression(beta[B]),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.12, .78),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1))

out |> 
  ggplot(aes(growth, ..density..)) +
  geom_density() +
  theme_classic()

out |> 
  ggplot(aes(biomass, ..density..)) +
  geom_density() +
  theme_classic()

gg2 <- out |> 
  ggplot(aes(ratio, ..density..)) +
  geom_density() +
  xlim(-0.3, 5) +
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[B]/beta[G]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     plot.title = element_text(size = 10))

gg1 + gg2 + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") 
#cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
ggsave(paste0(here::here(), "/manuscript/figures/fig_SBoot.png"), width = 8, height = 4)
ggsave(paste0(here::here(), "/manuscript/figures/fig_SXXX.pdf"), width = 8, height = 4)

# From forest data ####
# Option 1

## Stand biomass (B) ####
fit_biomass = lmer(Biomass_Kg_m2 ~ Year + (1|PlotID) + (1|Species),
                   data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_biomass)
out <- summary(fit_biomass)
out$coefficients
plot(allEffects(fit_biomass))
plot_model(fit_biomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_B <- out$coefficients[2,1]
coef_B_se <- out$coefficients[2,2]
dB_B <- coef_B/mean(aggData_QMDbinsDen_out$Biomass_Kg_m2,na.rm=T)
dB_B_se <- coef_B_se/mean(aggData_QMDbinsDen_out$Biomass_Kg_m2,na.rm=T)

## Stand net biomass change (G) ####
fit_biomass_change = lmer(BiomassIncrement_Kg_m2_year ~ Year + (1|PlotID) + (1|Species),
                          data = aggData_QMDbinsDen_out, na.action = "na.exclude")
summary(fit_biomass_change)
out <- summary(fit_biomass_change)
out$coefficients
plot(allEffects(fit_biomass_change))
plot_model(fit_biomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_G <- out$coefficients[2,1]
coef_G_se <- out$coefficients[2,2]
dG_G <- coef_G/mean(aggData_QMDbinsDen_out$BiomassIncrement_Kg_m2_year,na.rm=T)
dG_G_se <- coef_G_se/mean(aggData_QMDbinsDen_out$BiomassIncrement_Kg_m2_year,na.rm=T)

# ... or Remove outliers
residGrowth <- residuals(fit_biomass_change)
rowindex_outliers <- as.integer(names(boxplot.stats(residGrowth, coef = 2.5)$out))
aggData_QMDbinsDen_out <- aggData_QMDbinsDen_out %>% 
  mutate(rowindex = dplyr::row_number()) %>%
  mutate(outlier = rowindex %in% rowindex_outliers) 
aggData_QMDbinsDen_out_out <- aggData_QMDbinsDen_out |> filter(outlier==FALSE)
aggData_QMDbinsDen_out |> 
  ggplot(aes(x = Year, y = BiomassIncrement_Kg_m2_year, color = outlier)) + 
  geom_point()
fit_biomass_change_out = lmer(BiomassIncrement_Kg_m2_year ~ Year + (1|PlotID) + (1|Species),
                              data = aggData_QMDbinsDen_out_out, na.action = "na.exclude")
summary(fit_biomass_change_out)
out <- summary(fit_biomass_change_out)
out$coefficients
plot(allEffects(fit_biomass_change_out))
plot_model(fit_biomass_change_out,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_G <- out$coefficients[2,1]
coef_G_se <- out$coefficients[2,2]
dG_G <- coef_G/mean(aggData_QMDbinsDen_out$BiomassIncrement_Kg_m2_year,na.rm=T)
dG_G_se <- coef_G_se/mean(aggData_QMDbinsDen_out$BiomassIncrement_Kg_m2_year,na.rm=T)

fig_rel_change_stand <- ggplot() +  
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(aes(x=dG_G, y=dB_B),col="#009E73",size=2) + 
  geom_errorbar(aes(x=dG_G, y=dB_B,
                    xmin=dG_G-dG_G_se, xmax=dG_G+dG_G_se,
                    ymin=dB_B-dB_B_se, ymax=dB_B+dB_B_se), 
                width=0,col="#009E73") +
  geom_errorbarh(aes(y=dB_B,
                     xmin=dG_G-dG_G_se, xmax=dG_G+dG_G_se), 
                 height=0,col="#009E73") +
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Stand-level growth") + 
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
  scale_x_continuous(limits = c(-0.002,0.01),breaks=seq(0,0.01,0.005)) + 
  scale_y_continuous(limits = c(-0.002,0.01),breaks=seq(0,0.01,0.005)) 
fig_rel_change_stand

## Mean tree biomass change ####
fit_aggtreebiomass_change = lmer(mean_tree_biomass_growth_kgperyear ~ Year + (1|PlotID),
                              data = aggTreeData, na.action = "na.exclude")
summary(fit_aggtreebiomass_change)
out <- summary(fit_aggtreebiomass_change)
out$coefficients
plot(allEffects(fit_aggtreebiomass_change))
plot_model(fit_aggtreebiomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_GG <- out$coefficients[2,1]
coef_GG_se <- out$coefficients[2,2]
dG_GG <- coef_GG/mean(aggTreeData$mean_tree_biomass_growth_kgperyear,na.rm=T)
dG_GG_se <- coef_GG_se/mean(aggTreeData$mean_tree_biomass_growth_kgperyear,na.rm=T)

rel_change_aggtree <- ggplot() +  
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(aes(x=dG_GG, y=dB_B),col="#009E73",size=2) + 
  geom_errorbar(aes(x=dG_GG, y=dB_B,
                    xmin=dG_GG-dG_GG_se, xmax=dG_GG+dG_GG_se,
                    ymin=dB_B-dB_B_se, ymax=dB_B+dB_B_se), 
                width=0,col="#009E73") +
  geom_errorbarh(aes(y=dB_B,
                     xmin=dG_GG-dG_GG_se, xmax=dG_GG+dG_GG_se), 
                 height=0,col="#009E73") +
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title=NULL) + 
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
  scale_x_continuous(limits = c(-0.002,0.01),breaks=seq(0,0.01,0.005)) + 
  scale_y_continuous(limits = c(-0.002,0.01),breaks=seq(0,0.01,0.005))  
rel_change_aggtree

## Tree biomass ####
fit_treebiomass = lmer(Biomass_kg ~ Year + DBH_firstcensus + (1|PlotID/TreeID),
                       data = TreeData, na.action = "na.exclude")
summary(fit_treebiomass)
out <- summary(fit_treebiomass)
out$coefficients
plot(allEffects(fit_treebiomass))
plot_model(fit_treebiomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_B <- out$coefficients[2,1]
coef_B_se <- out$coefficients[2,2]
dB_B <- coef_B/mean(TreeData$Biomass_kg,na.rm=T)
dB_B_se <- coef_B_se/mean(TreeData$Biomass_kg,na.rm=T)

## Tree biomass change ####
fit_treebiomass_change = lmer(Biomass_growth_kgperyear ~ Year + DBH_firstcensus + (1|PlotID/TreeID),
                              data = TreeData, na.action = "na.exclude")
summary(fit_treebiomass_change)
out <- summary(fit_treebiomass_change)
out$coefficients
plot(allEffects(fit_treebiomass_change))
plot_model(fit_treebiomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_GG <- out$coefficients[2,1]
coef_GG_se <- out$coefficients[2,2]
dG_GG <- coef_GG/mean(TreeData$Biomass_growth_kgperyear,na.rm=T)
dG_GG_se <- coef_GG_se/mean(TreeData$Biomass_growth_kgperyear,na.rm=T)

rel_change_tree <- ggplot() +  
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(aes(x=dG_GG, y=dB_B),col="#009E73",size=2) + 
  geom_errorbar(aes(x=dG_GG, y=dB_B,
                    xmin=dG_GG-dG_GG_se, xmax=dG_GG+dG_GG_se,
                    ymin=dB_B-dB_B_se, ymax=dB_B+dB_B_se), 
                width=0,col="#009E73") +
  geom_errorbarh(aes(y=dB_B,
                     xmin=dG_GG-dG_GG_se, xmax=dG_GG+dG_GG_se), 
                 height=0,col="#009E73") +
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="Tree-level growth") + 
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
  scale_x_continuous(limits = c(0,0.02),breaks=seq(0,0.01,0.005)) + 
  scale_y_continuous(limits = c(0,0.02),breaks=seq(0,0.01,0.005))  
rel_change_tree

#rel_change_tree + fig_rel_change_stand
ggsave("~/GFDY/manuscript/figures/fig_S3.0.png",width = 5.5, height = 5, dpi=300)

# Option 2
load("~/GFDY/data/inputs_obs/aggData_analysis75.RData")
sort(table(aggData_analysis$PlotID))
# Select plots with minimum 3 census
relchanges_analysis <- aggData_analysis %>%
  group_by(PlotID) %>% mutate(n_census=n()) %>% ungroup() %>% filter(n_census>=3)
length(unique(relchanges_analysis$PlotID))
sort(table(relchanges_analysis$PlotID))

relchanges_analysis <- aggData_analysis %>% group_by(PlotID) %>% 
  mutate(n_census=n()) %>% ungroup() %>% 
  filter(PlotID!=160648&PlotID!=141562&PlotID!=147396&PlotID!=143115&PlotID!=45653&PlotID!=82133) %>%
  filter(n_census>=3) %>% 
  select(PlotID, Year, Biomass_Kg_m2, BiomassIncrement_Kg_m2_year,mean_tree_biomass_growth_kgperm2peryear,dataset) %>%
  group_by(PlotID) %>% 
  mutate(BiomassIncrement_Kg_m2_year = ifelse(is.na(BiomassIncrement_Kg_m2_year), 
  lead(BiomassIncrement_Kg_m2_year), BiomassIncrement_Kg_m2_year)) %>%
  mutate(mean_tree_biomass_growth_kgperm2peryear = ifelse(is.na(mean_tree_biomass_growth_kgperm2peryear), 
  lead(mean_tree_biomass_growth_kgperm2peryear), mean_tree_biomass_growth_kgperm2peryear)) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  mutate(dB_B=(Biomass_Kg_m2-lag(Biomass_Kg_m2))/lag(Biomass_Kg_m2),
         dG_Gtree=(mean_tree_biomass_growth_kgperm2peryear-lag(mean_tree_biomass_growth_kgperm2peryear))/lag(mean_tree_biomass_growth_kgperm2peryear),
         dG_Gstand=(BiomassIncrement_Kg_m2_year-lag(BiomassIncrement_Kg_m2_year))/lag(BiomassIncrement_Kg_m2_year)) %>% 
  ungroup() %>% drop_na()

relchanges_analysis |> 
  ggplot(aes(dG_Gtree, dB_B)) +
  geom_hex() +
  scale_fill_gradientn(
    colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  theme_bw() 

ggplot(data=relchanges_analysis, aes(x=dG_Gtree, y=dB_B)) + 
  geom_point(aes(col=dataset),size=3,alpha=0.5)
ggplot(data=relchanges_analysis, aes(x=dG_Gstand, y=dB_B)) + 
  geom_point(aes(col=dataset),size=3,alpha=0.5)

relchanges_analysis %>% filter(dataset=="EFM")

tb_relchanges_analysis <- relchanges_analysis %>% 
  group_by(dataset) %>% 
  summarise(dB_B_mean=mean(dB_B, na.rm = TRUE),
            dB_B_sd = sd(dB_B, na.rm = TRUE),
            dG_Gstand_mean=mean(dG_Gstand, na.rm = TRUE),
            dG_Gstand_sd = sd(dG_Gstand, na.rm = TRUE),
            dG_Gtree_mean=mean(dG_Gtree, na.rm = TRUE),
            dG_Gtree_sd = sd(dG_Gtree, na.rm = TRUE),
            n = n()) %>%
  mutate(dB_B_se = dB_B_sd / sqrt(n),
         dB_B_lower.ci = dB_B_mean - qt(1 - (0.05 / 2), n - 1) * dB_B_se,
         dB_B_upper.ci = dB_B_mean + qt(1 - (0.05 / 2), n - 1) * dB_B_se,
         dG_Gstand_se = dG_Gstand_sd / sqrt(n),
         dG_Gstand_lower.ci = dG_Gstand_mean - qt(1 - (0.05 / 2), n - 1) * dG_Gstand_se,
         dG_Gstand_upper.ci = dG_Gstand_mean + qt(1 - (0.05 / 2), n - 1) * dG_Gstand_se,
         dG_Gtree_se = dG_Gtree_sd / sqrt(n),
         dG_Gtree_lower.ci = dG_Gtree_mean - qt(1 - (0.05 / 2), n - 1) * dG_Gtree_se,
         dG_Gtree_upper.ci = dG_Gtree_mean + qt(1 - (0.05 / 2), n - 1) * dG_Gtree_se) %>% ungroup()

fig_data <- ggplot(data=tb_relchanges_analysis) + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(aes(x=dG_Gtree_mean, y=dB_B_mean,col=dataset,shape='Tree-level'),size=2,alpha=1) + 
  geom_errorbar(aes(x=dG_Gtree_mean, y=dB_B_mean,ymin=dB_B_lower.ci, ymax=dB_B_upper.ci,col=dataset), width=0,alpha=0.5) +
  geom_errorbarh(aes(y=dB_B_mean,xmin=dG_Gtree_lower.ci, xmax=dG_Gtree_upper.ci,col=dataset), height=0,alpha=0.5) +
  geom_point(aes(x=dG_Gstand_mean, y=dB_B_mean,col=dataset,shape='Stand-level'),size=2,alpha=1) + 
  geom_errorbar(aes(x=dG_Gstand_mean, y=dB_B_mean,ymin=dB_B_lower.ci, ymax=dB_B_upper.ci,col=dataset), width=0,alpha=0.5) +
  geom_errorbarh(aes(y=dB_B_mean,xmin=dG_Gstand_lower.ci, xmax=dG_Gstand_upper.ci,col=dataset), height=0,alpha=0.5) +
  scale_color_viridis(discrete=TRUE,option="viridis",
                      "Dataset") +
  scale_shape_manual("Growth", breaks = c("Tree-level","Stand-level"), 
                     values = c(16,17),
                     guide = guide_legend(override.aes = list(color = "black",size=1.6),order=2)) +  
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="From forest data") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                     plot.title = element_text(size = 10),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.62, .15),
                     legend.direction="horizontal",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(limits = c(-0.6,0.8),breaks=seq(-0.5,1,0.5)) + 
  scale_y_continuous(limits = c(-0.6,0.8),breaks=seq(-0.5,1,0.5)) 
fig_data

