# This script analyses the changes in the STLs w.r.t growth (growth anomalies, i.e., residuals).
# Results generate Figure 1 B.

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) 
library(lmerTest) 
library(effects) 
library(MuMIn)
library(sjPlot)
library(patchwork)
library(ggeffects)
library(patchwork)

# load data
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen55.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen75.RData")
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen90.RData")

# LME model N ~ QMD and Res_Growth0 with 75th percentile
Fit_ResBio = lmer(logDensity ~ scale(logQMD) + scale(Res_Growth0) + (1|PlotID) + (1|Species), data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(Fit_ResBio)
r.squaredGLMM(Fit_ResBio)
plot(allEffects(Fit_ResBio))
plot_model(Fit_ResBio, type = "pred",show.data=TRUE, dot.size=1.5, terms = c("logQMD","Res_Growth0[-0.60,0.50,1.60]"))
summary(aggData_QMDbinsDen$Res_Growth0)

pred <- ggpredict(Fit_ResBio, terms = c("logQMD","Res_Growth0[-0.60,0.50,1.60]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)

plot75Res <- ggplot() + 
  geom_point(data = aggData_QMDbinsDen, aes(x = logQMD, y = logDensity), alpha=0.3, size = .8,col="black", shape = 16, inherit.aes = FALSE) + 
  geom_point(data = aggData_QMDbinsRest, aes(x = logQMD, y = logDensity), alpha=0.2, size = .8,col="grey",shape = 16, inherit.aes = FALSE) + 
  geom_smooth(data= preddata, aes(x=x, y=predicted, color=group), method = "lm",fullrange = T,size = .6, se=F) +
  labs(x = "ln QMD", y = "ln N",title = "STL changes as a function of growth",color  = "Growth anomalies") + 
  scale_color_manual(expression(paste("Growth \n anomalies")), 
                     breaks = c("-0.6","0.5", "1.6"), 
                     labels = c("-0.60","0.50", "1.60"),
                     values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                      legend.text = element_text(size = 9),legend.title = element_text(size = 9),
                      plot.title = element_text(size = 10),
                      legend.key = element_rect(fill = NA, color = NA),
                      legend.position = c(.11, .20),
                      legend.direction="vertical",
                      legend.box = "horizontal",
                      legend.margin = margin(2, 2, 2, 2),
                      legend.key.size = unit(.6, 'cm'),
                      legend.box.margin = margin(1, 1, 1, 1)) +
  scale_x_continuous(limits = c(2,4.5),breaks = seq(2,4.5,0.5))+
  scale_y_continuous(limits = c(4.5,9.2))
plot75Res

hist(aggData_QMDbinsDen$Res_Growth0)
hist_Res <- ggplot(aggData_QMDbinsDen, aes(x=Res_Growth0)) + geom_histogram(color="#FFDB6D", fill="#FFDB6D") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),axis.title = element_text(size = 8),
        plot.margin = unit(c(-.5,.1,.1,.1), "cm")) + ggtitle("") +
  scale_x_continuous("Growth anomalies", breaks = c(-0.60,0.50,1.60)) +
  scale_y_continuous("Frequency", limits = c(0,100), breaks = seq(0,100,20))
hist_Res

gg2 <- plot75Res + inset_element(hist_Res, left = 0.65, bottom = 0.5, right = 0.99, top = 0.99)
gg2

# Figure 3 ms
ff1 <- plot75Year + inset_element(hist_Year, left = 0.6, bottom = 0.6, right = 0.99, top = 0.99, ignore_tag = TRUE) +
  plot75Res + inset_element(hist_Res, left = 0.6, bottom = 0.6, right = 0.99, top = 0.99, ignore_tag = TRUE) + 
  plot_layout(ncol = 2) + 
  #plot_annotation(tag_levels = list(c('a)', 'b)')))
  plot_annotation(tag_levels = 'A', tag_suffix = ")") #& 
  #theme(plot.margin = unit(rep(0.13,4), "cm"))#+
  #plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ff1
ggsave("~/GFDY/manuscript/figures/fig_1.png", width = 8.5, height = 4.2, dpi=300)

# Get upward shift
# predict y for a given x
pred <- ggpredict(Fit_ResBio, terms = c("logQMD","Res_Growth0[0,1]"), full.data = TRUE)
plot(pred, add.data = F) 
preddata <- as.data.frame(pred)
preddata <- preddata %>% group_by(x) %>% 
  mutate(upSTL=predicted-lag(predicted)) %>%
  mutate(increment=predicted/lag(predicted)) %>%
  mutate(percent=upSTL*100/lag(predicted))
preddata
change_STL <- preddata %>%
  filter(group=="1") %>%
  ungroup(x) %>%
  summarise(percent=mean(percent)) %>% pull()
change_STL
