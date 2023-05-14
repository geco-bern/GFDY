
# load packages
library(dplyr)
library(ggplot2)
library(viridis)

# Relative change biomass (plantC) vs. NPP ####

# From model simulations ####

#DBH
# DBH p1
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp1gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp1gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp1gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp1gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p2
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp2gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
dB0_15 <- (B15$B15 - B0$B0)/B0$B0
dB15_30 <- (B30$B30 - B15$B15)/B15$B15
dB0_30 <- (B30$B30 - B0$B0)/B0$B0

DBHp2gl_RelChange_B_NPP_0_15 <- data.frame(dNPP0_15,dB0_15)
DBHp2gl_RelChange_B_NPP_15_30 <- data.frame(dNPP15_30,dB15_30)
DBHp2gl_RelChange_B_NPP_0_30 <- data.frame(dNPP0_30,dB0_30)

# DBH p3
# Calculate the relative change as (Final value - initial value)/initial value
NPP30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP30=mean(NPP))  
NPP15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP15=mean(NPP))  
NPP0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(NPP0=mean(NPP))
dNPP0_15 <- (NPP15$NPP15 - NPP0$NPP0)/NPP0$NPP0
dNPP15_30 <- (NPP30$NPP30 - NPP15$NPP15)/NPP15$NPP15
dNPP0_30 <- (NPP30$NPP30 - NPP0$NPP0)/NPP0$NPP0

B30 <- ea3sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B30=mean(plantC)) 
B15 <- ea2sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B15=mean(plantC))
B0  <- ea1sa1DBHp3gl_out_annual_tile %>% dplyr::filter(year>=900) %>% summarise(B0=mean(plantC))
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
  scale_x_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) + 
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,0.5,0.25)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
fig2e_dbh

# From Walker et al. 2020 ####
table2_walker <- read.csv("~/GFDY/data/raw_obs/table2_walker.csv")
str(table2_walker)

agg_table2 <- table2_walker %>% group_by(Plotted,biomeE_var) %>% 
  summarise(mean=mean(beta, na.rm = TRUE),
            sd = sd(beta, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% ungroup()

agg_table2 <- agg_table2 %>% dplyr::select(-c(Plotted, n, se)) %>%
  pivot_wider(names_from = biomeE_var, values_from = c(mean,sd,lower.ci,upper.ci))

ggplot(data=agg_table2, aes(x=mean_NPP, y=mean_plantC)) + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(col="#009E73",size=2) + 
  geom_errorbar(aes(xmin=lower.ci_NPP, xmax=upper.ci_NPP,ymin=lower.ci_plantC, ymax=upper.ci_plantC), 
                width=0,col="#009E73") +
  geom_errorbarh(aes(xmin=lower.ci_NPP, xmax=upper.ci_NPP), 
                height=0,col="#009E73") +
  labs(x = expression(beta[G]), y = expression(beta[B]),title=NULL) + 
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
  scale_x_continuous(limits = c(0,1.5),breaks=seq(0,1.5,0.5)) + 
  scale_y_continuous(limits = c(0,1.5),breaks=seq(0,1.5,0.5)) 

table2_walker <- read.csv("~/GFDY/data/raw_obs/table2_walker.csv")
str(table2_walker)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)

table2_walker_B <- table2_walker %>% filter(biomeE_var=="plantC") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         SD_beta = ifelse(is.na(SD_beta), mean(SD_beta/beta,na.rm=T), SD_beta))
         
table2_walker_G <- table2_walker %>% filter(biomeE_var=="NPP") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))

out_B <- data.frame()
out_G <- data.frame()
out <- data.frame()

for (n in 1:100000){
  
  i <- sample(dim(table2_walker_B)[1],1)
  j <- sample(dim(table2_walker_G)[1],1)

  B_sample <- rnorm(1000,table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1000,table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out_B <- rbind(out_B, data.frame(B_sample,i))
  out_G <- rbind(out_G, data.frame(G_sample,j,n))
  out   <- cbind(out_B, out_G)
  
}  
  
out_agg <- out %>% 
  summarise(mean_beta_B = mean(B_sample, na.rm = TRUE),
            sd_beta_B = sd(B_sample, na.rm = TRUE),
            mean_beta_G = mean(G_sample, na.rm = TRUE),
            sd_beta_G = sd(G_sample, na.rm = TRUE),
            n = n()) %>%
  mutate(se_beta_B = sd_beta_B / sqrt(n),
         lower.ci_beta_B = mean_beta_B - qt(0.95, df=n - 1) * sd_beta_B/sqrt(n),
         upper.ci_beta_B = mean_beta_B + qt(0.95, df=n - 1) * sd_beta_B/sqrt(n),
         se_beta_G = sd_beta_G / sqrt(n),
         lower.ci_beta_G = mean_beta_G - qt(0.95, df=n - 1) * sd_beta_G/sqrt(n),
         upper.ci_beta_G = mean_beta_G + qt(0.95, df=n - 1) * sd_beta_G/sqrt(n)) %>% ungroup()

out_agg <- out %>% group_by(n) %>%
  summarise(mean_beta_B = mean(B_sample, na.rm = TRUE),
            sd_beta_B = sd(B_sample, na.rm = TRUE),
            mean_beta_G = mean(G_sample, na.rm = TRUE),
            sd_beta_G = sd(G_sample, na.rm = TRUE),
            sample = n())

ggplot(data=out_agg) + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(aes(x=mean_beta_G, y=mean_beta_B, col=n), size=2)

ggplot(data=out_agg, aes(x=mean_beta_G, y=mean_beta_B)) + 
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(col="#009E73",size=2) + 
  geom_errorbar(aes(xmin=mean_beta_G-sd_beta_G, #lower.ci_beta_G, 
                    xmax=mean_beta_G+sd_beta_G, #upper.ci_beta_G,
                    ymin=mean_beta_B-sd_beta_B, #lower.ci_beta_B, 
                    ymax=mean_beta_B+sd_beta_B), #upper.ci_beta_B), 
                width=0,col="#009E73") +
  geom_errorbarh(aes(xmin=mean_beta_G-sd_beta_G, #lower.ci_beta_G, 
                     xmax=mean_beta_G+sd_beta_G), #upper.ci_beta_G), 
                 height=0,col="#009E73") +
  labs(x = expression(beta[G]), y = expression(beta[B]),title=NULL) + 
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
  scale_x_continuous(limits = c(-0.4,1.7),breaks=seq(0,1.5,0.5)) + 
  scale_y_continuous(limits = c(-0.4,1.7),breaks=seq(0,1.5,0.5)) 
ggsave("~/GFDY/manuscript/figures/fig_SXXWalker.png",width = 5, height = 4, dpi=300)

# Bootstrapping
# See here https://rpubs.com/evelynebrie/bootstrapping

# For beta_NPP
# Option 1
library(boot)
data_beta_NPP <- rnorm(20000,agg_table2$mean_NPP,agg_table2$sd_NPP) 
b_beta_NPP <- boot(data_beta_NPP, function(u,i) mean(u[i]), R = 10000)
ci_beta_NPP <- boot.ci(b_beta_NPP, type = c("norm", "basic", "perc"))
ci_low_beta_NPP <- ci_beta_NPP$normal[2]
ci_high_beta_NPP <- ci_beta_NPP$normal[3]
mean_data_beta_NPP <- mean(data_beta_NPP)

data_beta_plantC <- rnorm(20000,agg_table2$mean_plantC,agg_table2$sd_plantC) 
b_beta_plantC <- boot(data_beta_plantC, function(u,i) mean(u[i]), R = 10000)
ci_beta_plantC <- boot.ci(b_beta_plantC, type = c("norm", "basic", "perc"))
ci_low_beta_plantC <- ci_beta_plantC$normal[2]
ci_high_beta_plantC <- ci_beta_plantC$normal[3]
mean_data_beta_plantC <- mean(data_beta_plantC)

ggplot() +  
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") +
  geom_point(aes(x=mean_data_beta_NPP, y=mean_data_beta_plantC),col="#009E73",size=2) + 
  geom_errorbar(aes(x=mean_data_beta_NPP, y=mean_data_beta_plantC,
                    xmin=ci_low_beta_NPP, xmax=ci_high_beta_NPP,
                    ymin=ci_low_beta_plantC, ymax=ci_high_beta_plantC), 
                width=0,col="#009E73") +
  geom_errorbarh(aes(y=mean_data_beta_plantC,
                     xmin=ci_low_beta_NPP, xmax=ci_high_beta_NPP), 
                 height=0,col="#009E73") +
  labs(x = expression(beta[G]), y = expression(beta[B]),title=NULL) + 
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
  scale_x_continuous(limits = c(0.4,0.5),breaks=seq(0.4,0.5,0.05)) + 
  scale_y_continuous(limits = c(0.8,0.9),breaks=seq(0.8,0.9,0.05)) 


# Option 2
# 1.1 Creating a random normal distribution using rnorm()
set.seed(300) # Setting the seed for replication purposes
data_beta_NPP <- rnorm(50000,agg_table2$mean_NPP,agg_table2$sd_NPP) 

# 1.2 Performing sanity checks on myData using length(), mean() and sd()
length(data_beta_NPP)
mean(data_beta_NPP)
sd(data_beta_NPP)

# 2.1 Resampling from myData 25000 times using for(i in x)
set.seed(200) # Setting the seed for replication purposes
sample.size <- 50000 # Sample size
n.samples <- 25000 # Number of bootstrap samples
bootstrap.results.NPP <- c() # Creating an empty vector to hold the results
for (i in 1:n.samples)
{
  obs <- sample(1:sample.size, replace=TRUE)
  bootstrap.results.NPP[i] <- mean(data_beta_NPP[obs]) # Mean of the bootstrap sample
}
length(bootstrap.results.NPP) # Sanity check: this should contain the mean of 25000 different samples
summary(bootstrap.results.NPP) # Sanity check
boots_sd_NPP <- sd(bootstrap.results.NPP) # Checking the standard deviation of the distribution of means (this is what we are interested in!)
boots_sd_NPP
boots_mean_NPP <- mean(bootstrap.results.NPP)
boots_mean_NPP

hist(bootstrap.results.NPP, # Creating an histogram
     col="#d83737", # Changing the color
     xlab="Mean", # Giving a label to the x axis
     main=paste("Means of 1000 bootstrap samples from beta_NPP")) # Giving a title to the graph

hist(data_beta_NPP, # Creating an histogram
     col="#37aad8", # Changing the color
     xlab="Value", # Giving a label to the x axis
     main=paste("Distribution of beta_NPP")) # Giving a title to the graph

# For beta_plantC

# Option 2
# 1.1 Creating a random normal distribution using rnorm()
set.seed(300) # Setting the seed for replication purposes
data_beta_plantC <- rnorm(50000,agg_table2$mean_plantC,agg_table2$sd_plantC) 

# 1.2 Performing sanity checks on myData using length(), mean() and sd()
length(data_beta_plantC)
mean(data_beta_plantC)
sd(data_beta_plantC)

# 2.1 Resampling from myData 10000 times using for(i in x)
set.seed(200) # Setting the seed for replication purposes
sample.size <- 50000 # Sample size
n.samples <- 25000 # Number of bootstrap samples
bootstrap.results.plantC <- c() # Creating an empty vector to hold the results
for (i in 1:n.samples)
{
  obs <- sample(1:sample.size, replace=TRUE)
  bootstrap.results.plantC[i] <- mean(data_beta_plantC[obs]) # Mean of the bootstrap sample
}
length(bootstrap.results.plantC) # Sanity check: this should contain the mean of 25000 different samples
summary(bootstrap.results.plantC) # Sanity check
boots_sd_plantC <- sd(bootstrap.results.plantC) # Checking the standard deviation of the distribution of means (this is what we are interested in!)
boots_sd_plantC
boots_mean_plantC <- mean(bootstrap.results.plantC)
boots_mean_plantC

hist(bootstrap.results.plantC, # Creating an histogram
     col="#d83737", # Changing the color
     xlab="Mean", # Giving a label to the x axis
     main=paste("Means of 1000 bootstrap samples from beta_NPP")) # Giving a title to the graph

hist(data_beta_NPP, # Creating an histogram
     col="#37aad8", # Changing the color
     xlab="Value", # Giving a label to the x axis
     main=paste("Distribution of beta_NPP")) # Giving a title to the graph

bootstrap_tb <- data.frame(boots_mean_NPP,boots_mean_plantC,boots_sd_NPP,boots_sd_plantC)
bootstrap_tb <- bootstrap_tb %>% 
  mutate(n = n.samples,
         boots_se_NPP = boots_sd_NPP / sqrt(n),
         boots_lower.ci_NPP = boots_mean_NPP - qt(1 - (0.05 / 2), n - 1) * boots_se_NPP,
         boots_upper.ci_NPP = boots_mean_NPP + qt(1 - (0.05 / 2), n - 1) * boots_se_NPP,
         boots_se_plantC = boots_sd_plantC / sqrt(n),
         boots_lower.ci_plantC = boots_mean_plantC - qt(1 - (0.05 / 2), n - 1) * boots_se_plantC,
         boots_upper.ci_plantC = boots_mean_plantC + qt(1 - (0.05 / 2), n - 1) * boots_se_plantC) %>% 
  ungroup()

fig_boots <- ggplot(data=bootstrap_tb) + 
  geom_point(aes(x=boots_mean_NPP, y=boots_mean_plantC),col="#009E73",size=2) + 
  geom_errorbar(aes(x=boots_mean_NPP, y=boots_mean_plantC,
                    ymin=boots_upper.ci_plantC, ymax=boots_upper.ci_plantC), 
                width=0,col="#009E73",alpha=0.5) +
  geom_errorbarh(aes(y=boots_mean_plantC,
                    xmin=boots_upper.ci_NPP, xmax=boots_upper.ci_NPP), 
                 height=0,col="#009E73",alpha=0.5) +
  labs(x = expression(frac(dG, G)), y = expression(frac(dB, B)),title="From literature") + 
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
  scale_x_continuous(limits = c(0,1),breaks=seq(0,1,0.5)) + 
  scale_y_continuous(limits = c(0,1),breaks=seq(0,1,0.5)) +
  geom_abline(slope=1, intercept = 0.0, linetype="dashed") 
fig_boots

# From forest data ####
# Option 1

# Stand biomass (B)
fit_biomass = lmer(Biomass_Kg_m2 ~ Year + (1|PlotID) + (1|Species),
                   data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(fit_biomass)
out <- summary(fit_biomass)
out$coefficients
plot(allEffects(fit_biomass))
plot_model(fit_biomass,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_B <- out$coefficients[2,1]
coef_B_se <- out$coefficients[2,2]
dB_B <- coef_B/mean(aggData_QMDbinsDen$Biomass_Kg_m2,na.rm=T)
dB_B_se <- coef_B_se/mean(aggData_QMDbinsDen$Biomass_Kg_m2,na.rm=T)

# Stand net biomass change (G)
fit_biomass_change = lmer(BiomassIncrement_Kg_m2_year ~ Year + (1|PlotID) + (1|Species),
                          data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(fit_biomass_change)
out <- summary(fit_biomass_change)
out$coefficients
plot(allEffects(fit_biomass_change))
plot_model(fit_biomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_G <- out$coefficients[2,1]
coef_G_se <- out$coefficients[2,2]
dG_G <- coef_G/mean(aggData_QMDbinsDen$BiomassIncrement_Kg_m2_year,na.rm=T)
dG_G_se <- coef_G_se/mean(aggData_QMDbinsDen$BiomassIncrement_Kg_m2_year,na.rm=T)

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

# Mean tree biomass change
fit_treebiomass_change = lmer(mean_tree_biomass_growth_kgperyear ~ Year + (1|PlotID),
                              data = aggTreeData, na.action = "na.exclude")
summary(fit_treebiomass_change)
out <- summary(fit_treebiomass_change)
out$coefficients
plot(allEffects(fit_treebiomass_change))
plot_model(fit_treebiomass_change,type = "pred",show.data=TRUE, dot.size=1.5, terms = c("Year"))
coef_GG <- out$coefficients[2,1]
coef_GG_se <- out$coefficients[2,2]
dG_GG <- coef_GG/mean(aggTreeData$mean_tree_biomass_growth_kgperyear,na.rm=T)
dG_GG_se <- coef_GG_se/mean(aggTreeData$mean_tree_biomass_growth_kgperyear,na.rm=T)

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
  scale_x_continuous(limits = c(-0.002,0.01),breaks=seq(0,0.01,0.005)) + 
  scale_y_continuous(limits = c(-0.002,0.01),breaks=seq(0,0.01,0.005))  
rel_change_tree

rel_change_tree + fig_rel_change_stand
ggsave("~/GFDY/manuscript/figures/fig_SXX.png",width = 8.5, height = 4.2, dpi=300)

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
# Figure 4 ####
ff4 <- fig_data + fig_boots +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ")") & 
  theme(plot.margin = unit(rep(0.13,4), "cm"))#+ 
#plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff4
ggsave("~/GFDY/manuscript/figures/fig_4.png",width = 8.5, height = 4.2, dpi=300)

