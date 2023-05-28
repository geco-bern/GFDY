
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
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(breaks=seq(-5,5,5)) + 
  scale_y_continuous(breaks=seq(0,4,2)) 
gg1

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
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[B]/beta[G]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                     plot.title = element_text(size = 10)) + 
  scale_x_continuous(limits= c(-0.3, 5), breaks=seq(0,5,2.5)) + 
  scale_y_continuous(breaks=c(0,0.6,0.3)) 
gg2

# Figure S9 ####
gg1 + gg2 + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")")& 
  theme(plot.tag = element_text(size = 12)) 
#cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
ggsave(paste0(here::here(), "/manuscript/figures/fig_S9.png"), width = 8, height = 4)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S9.pdf"), width = 8, height = 4)

# From forest data ####

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

# Figure S3 ####
fig_s3 <- rel_change_aggtree
fig_s3
ggsave(paste0(here::here(), "/manuscript/figures/fig_S3.png"), width = 5, height = 4.5, dpi=300)
ggsave(paste0(here::here(), "/manuscript/figures/fig_S3.pdf"), width = 5, height = 4.5, dpi=300)

