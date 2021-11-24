# INSCI prediction: Conditional probabilities 
# Author: SJ Borg and DN Borg
# Date: November 2021

# Libraries
library(tidyverse)
library(dplyr)
library(tidybayes)
library(brms)
library(modelr)
library(ggplot2)
library(bayesplot)
library(cowplot)

setwd("~/Downloads") # Set working director
load("~/Downloads"/"fit_insci_07-06-21.RData") #Load model

#### FACTOR VARIABLES - education, gender, marital, disability pension, social attitudes, daily assistance, vocational rehabilitation ####

### Marginal probs of education
prob_95 <- conditional_effects(fit, effects = "education", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '1' = '1', '2' = '2', '3' = '3', '4' = '4', '5' = '5', '6' = '6') #'1' = 'primary', '2' = 'secondary', '3' = 'post-2nd', '4' = 'diploma', '5' = 'bachelor', '6' = 'masters'
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Level of Education") +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Education") +
  theme(strip.text.x = element_text(size = 12)) -> plot_edu
plot_edu
#ggsave(file = "Marginal-probs-education", units="in", width = 5.5, height = 4, dpi = 300)

### Marginal probs of gender
prob_95 <- conditional_effects(fit, effects = "gender", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '1' = 'male', '2' = 'female') 
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Gender") +
  theme_bw() +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Gender") +
  theme(strip.text.x = element_text(size = 12)) -> plot_gender
plot_gender

### Marginal probs of marital status
prob_95 <- conditional_effects(fit, effects = "marital_status", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '1' = 'single', '2' = 'married', '3' = 'separated')
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Marital status") +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Marital status") +
  theme(strip.text.x = element_text(size = 12)) -> plot_marital
plot_marital


### Marginal probs of disability pension
prob_95 <- conditional_effects(fit, effects = "disabil_pension", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '0' = 'no', '1' = 'yes')
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Receiving disability pension") +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Disability Pension") +
  theme(strip.text.x = element_text(size = 12)) -> plot_pension
plot_pension


### Marginal probs of social attitudes
prob_95 <- conditional_effects(fit, effects = "social_att", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '1' = 'none', '2' = 'some', '3' = 'n/a')
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Influence of societal attitudes") +
  theme_bw() +
  ylim(.1,.7) +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Societal Attitudes") +
  theme(strip.text.x = element_text(size = 12)) -> plot_social
plot_social


### Marginal probs of daily assistance
prob_95 <- conditional_effects(fit, effects = "daily_ass", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '0' = 'no', '1' = 'yes')
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Receives daily assistance") +
  ylim(0.2,0.7) +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Daily Household Assistance") +
  theme(strip.text.x = element_text(size = 12)) -> plot_assist
plot_assist


### Marginal probs of vocational rehabilitation 
prob_95 <- conditional_effects(fit, effects = "voc_rehab", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95") %>%
  mutate(
    effect1__ = as.factor(effect1__),
    effect1__ = recode_factor(effect1__, '2' = 'none', '3' = 'some', '4' = 'a great deal', '1' = 'not needed')
  )

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), size = 0.5, fatten = 3) +
  labs(y = "Posterior Prob of Work", x = "Vocational Rehabiliation Use") +
  ylim(0.2,0.8) +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Vocational Rehabilitation") +
  theme(strip.text.x = element_text(size = 12)) -> plost_VR
plost_VR


#### CONTINUOUS VARIABLES - age, TSI, SCIM, activity ####

### Marginal probs of age
prob_95 <- conditional_effects(fit, effects = "age", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95")

# Mean and SD of age
mu = 51.96
sigma = 11.80014

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_line(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_ribbon(aes(x = (effect1__*sigma)+mu, ymin = lower__, ymax = upper__), alpha = 0.3) +
  labs(y = "Posterior Prob of Work", x = "Age") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Age") +
  theme(strip.text.x = element_text(size = 12)) -> plot_age
plot_age
#ggsave(file = "Marginal-probs-age", units="in", width = 5.5, height = 4, dpi = 300)

### Marginal probs of time since injury 
prob_95 <- conditional_effects(fit, effects = "tsi", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95")

# Mean and SD of tsi
mu = 17.19  
sigma = 13.06903

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_line(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_ribbon(data = prob_95, aes(x = (effect1__*sigma)+mu, ymin = lower__, ymax = upper__), alpha = 0.3) +
  labs(y = "Posterior Prob of Work", x = "Time since injury") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Time since injury") +
  theme(strip.text.x = element_text(size = 12)) -> plot_tsi
plot_tsi

### Marginal probs of SCIM  
prob_95 <- conditional_effects(fit, effects = "scim", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95")

# Mean and SD of tsi
mu = 62.31  
sigma = 27.69064

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_line(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_ribbon(data = prob_95, aes(x = (effect1__*sigma)+mu, ymin = lower__, ymax = upper__), alpha = 0.3) +
  labs(y = "Posterior Prob of Work", x = "Spinal Cord Independence Measure") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Spinal Cord Independence Measure") +
  theme(strip.text.x = element_text(size = 12)) -> plot_scim
plot_scim

### Marginal probs of Activity  
prob_95 <- conditional_effects(fit, effects = "activity", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95")

# Mean and SD of activity
mu = 24.5  
sigma = 9.621211

prob_95 %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_line(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_ribbon(data = prob_95, aes(x = (effect1__*sigma)+mu, ymin = lower__, ymax = upper__), alpha = 0.3) +
  labs(y = "Posterior Prob of Work", x = "Activity and participation scale") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12),
        legend.text=element_text(size = 10),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  facet_grid(~"Activity and participation scale") +
  theme(strip.text.x = element_text(size = 12)) -> plot_activity
plot_activity

#### Panel plot of probs ####
plot_grid(plot_activity,
          plot_age,
          plot_assist,
          plot_pension,
          plot_edu,
          plot_gender,
          plot_marital,
          plot_social,
          plot_scim,
          plot_tsi,
          plost_VR,
          ncol = 3, 
          nrow = 4, 
          scale = 0.975, 
          align = 'v', axis = "lr")
ggsave(file = "Figure-3.png", units="in", width = 12, height = 12, dpi = 300)

###END
