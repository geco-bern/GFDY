# This script calculates the shifts upwards if growths changes with +15 and +30% to link model simulations with data
# Growth enhancements (Res_Growth15,Res_Growth30) are evaluated in the original model (Fit_link, with Res_Growth0) and predicts are calculated.
# Results are used to generate figure 5 and compare the STL shifts in the model and the data.

# load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggridges)

# load data
load("~/GFDY/data/inputs_obs/aggData_QMDbinsDen.RData")

# LMM model lnN ~ lnQMD + Res_Growth0
Fit_link = lmer(logDensity ~ scale(logQMD) + scale(Res_Growth0) + (1|PlotID), data = aggData_QMDbinsDen, na.action = "na.exclude")
summary(Fit_link)
hist(aggData_QMDbinsDen$Res_Growth0)

# Predictions for control growth
aggData_QMDbinsDen <- aggData_QMDbinsDen %>% 
  mutate(predict_link0 = predict(Fit_link)) 

# Predictions for growth + 15%
aggData_15 <- aggData_QMDbinsDen %>% dplyr::select(logQMD,Res_Growth15,PlotID) %>% 
  rename(Res_Growth0=Res_Growth15) 
aggData_QMDbinsDen <- aggData_QMDbinsDen %>% 
  mutate(predict_link15 = predict(Fit_link,aggData_15, allow.new.levels = TRUE)) 

# Predictions for growth + 30%
aggData_30 <- aggData_QMDbinsDen %>% dplyr::select(logQMD,Res_Growth30,PlotID) %>% 
  rename(Res_Growth0=Res_Growth30)
aggData_QMDbinsDen <- aggData_QMDbinsDen %>% 
  mutate(predict_link30 = predict(Fit_link,aggData_30, allow.new.levels = TRUE)) 

# Changes in upward shift
aggData_preds <- aggData_QMDbinsDen %>% dplyr::select(predict_link0,predict_link15,predict_link30) %>%
  mutate(STL_15=(predict_link15-predict_link0)) %>%
  mutate(STL_30=(predict_link30-predict_link0)) 

aggData_preds %>%
  pivot_longer(cols = c(STL_15, STL_30), names_to = "source", values_to = "stl") %>% 
  ggplot(aes(source, stl)) +
  geom_boxplot()

save(aggData_preds, file = "~/GFDY/data/outputs_obs/aggData_preds.RData")

