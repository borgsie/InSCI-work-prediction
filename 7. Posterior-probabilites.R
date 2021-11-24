# INSCI prediction: Supplement 2 - Posterior probabilities
# Author: SJ Borg and DN Borg
# Date: November 2021

# Libraries
library(mice)
library(Rcpp)
library(janitor)
library(tidyverse)
library(dplyr)
library(tidybayes)
library(brms)
library(modelr)
library(ggplot2)
library(naniar)
library(visdat)
library(bayesplot)

setwd("~/Downloads") # Set working director
load("~/Downloads"/"fit_insci_07-06-21.RData") #Load model

#### DB added October 5, 2021
# Probabilities for 'certain' effects
# Less likely to be engaged
gather_draws(fit, b_gender2) %>% mean_qi(.value<0)
gather_draws(fit, b_daily_ass1) %>% mean_qi(.value<0)
gather_draws(fit, b_age) %>% mean_qi(.value<0)
gather_draws(fit, b_social_att3) %>% mean_qi(.value<0)
# More likely to be engaged
gather_draws(fit, b_scim) %>% mean_qi(.value>0)
gather_draws(fit, b_marital_status2) %>% mean_qi(.value>0)
gather_draws(fit, b_education6) %>% mean_qi(.value>0)

# Marital_3
par_effect <- 
  gather_draws(fit, b_marital_status3) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_level_completeness2
par_effect <- 
  gather_draws(fit, b_level_completeness2) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_level_completeness4
par_effect <- 
  gather_draws(fit, b_level_completeness4) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_general_health2
par_effect <- 
  gather_draws(fit, b_general_health2) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_general_health1
par_effect <- 
  gather_draws(fit, b_general_health1) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_second_health
par_effect <- 
  gather_draws(fit, b_second_health) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_pain
par_effect <- 
  gather_draws(fit, b_pain) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_pre_job1
par_effect <- 
  gather_draws(fit, b_pre_job1) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_education2
par_effect <- 
  gather_draws(fit, b_education2) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_education4
par_effect <- 
  gather_draws(fit, b_education4) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_education5
par_effect <- 
  gather_draws(fit, b_education5) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_daily_ass1
par_effect <- 
  gather_draws(fit, b_daily_ass1) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_activity
par_effect <- 
  gather_draws(fit, b_activity) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_qol1
par_effect <- 
  gather_draws(fit, b_qol1) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_qol2
par_effect <- 
  gather_draws(fit, b_qol2) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_voc_rehab1
par_effect <- 
  gather_draws(fit, b_voc_rehab1) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_sleep2
par_effect <- 
  gather_draws(fit, b_sleep2) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_social_att2
par_effect <- 
  gather_draws(fit, b_social_att2) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

# b_voc_rehab3
par_effect <- 
  gather_draws(fit, b_voc_rehab3) %>%
  mutate(effect = .value)
par_effect %>% ggplot() + geom_histogram(aes(x = effect))
par_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
par_effect %>% mean_qi(effect, .width = c(0.5, .95))
par_effect %>% mean_qi(effect>0)
par_effect %>% mean_qi(effect<0)

#### End
