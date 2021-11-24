# INSCI prediction: Figure 2 and ORs table
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

#### Figure 2 - Odds ratios plot and OR table ####
odds_rat = exp(fixef(fit, summary = TRUE, robust = FALSE, probs = c(0.025, 0.33, 0.66, 0.975))) %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  select(-Est.Error) %>%
  filter(!parameter == "Intercept") %>%
  clean_names()

or_plt <- odds_rat %>% ggplot() +
  geom_hline(yintercept=1, linetype=1, color = "grey45") +
  geom_point(aes(x = reorder(parameter, -estimate), y = estimate), size = 1.5) +
  geom_linerange(aes(x = reorder(parameter, -estimate), y = estimate, ymin = q2_5, ymax = q97_5)) +
  labs(y = "Odds ratio") +
  scale_y_continuous(n.breaks = 6) +
  coord_flip() +
  theme_bw(base_size = 14) +
  scale_x_discrete (labels = c("Masters/PhD education: 3.01 (1.63, 5.44)", "Married: 1.68 (1.13, 2.49)", "Diploma education: 1.51 (0.97, 2.63)",
                               "Bachelor education: 1.39 (0.96, 2.27)", "VR not needed: 1.39 (0.97, 2.18)", "Tetraplegia complete: 1.33 (0.88, 2.70)",
                               "m-SCIM-SR: 1.31 (1.01, 1.68)", "Some VR use: 1.31 (0.97, 1.94)", "Sleep quality, poor: 1.23 (0.95, 1.77)",
                               "Pre-injury job: 1.18 (0.88, 1.92)", "Divorced: 1.14 (0.85, 1.90)", "Time since injury: 1.12 (0.97, 1.34)",
                               "Tetraplegia incomplete: 1.11 (0.89, 1.56)", "General health, neither: 1.10 (0.89, 1.51)", "Post-2nd education: 1.08 (0.83, 1.58)",
                               "2 Hospitalisations: 1.08 (0.83, 1.58)", "A great deal VR use: 1.07 (0.79, 1.65)", "Paraplegia complete: 1.03 (0.82, 1.39)",
                               "SF-36 vitality scale: 1.01 (0.87, 1.17)", "Transport scale: 0.99 (0.85, 1.15)", "1 Hospitalisation: 0.98 (0.75, 1.24)",
                               "Rural area: 0.97 (0.75, 1.21)", "Pain scale: 0.95 (0.80, 1.08)", "Non-traumatic: 0.94 (0.64, 1.22)",
                               "Remote area: 0.93 (0.66, 1.18)", ">2 Hospitalisations: 0.92 (0.56, 1.26)", "General health, poor: 0.91 (0.62, 1.17)",
                               "Social att., influencial: 0.91 (0.63, 1.16)", "QoL Neither: 0.88 (0.60, 1.11)", "Secondary conditions: 0.88 (0.70, 1.05)",
                               "Activity scale: 0.84 (0.66, 1.02)", "Age: 0.75 (0.63, 0.90)", "Daily assistance: 0.75 (0.49, 1.03)",
                               "QoL Poor: 0.73 (0.38, 1.09)", "Higher 2nd education: 0.68 (0.39, 1.04)", "Female: 0.55 (0.37, 0.81)",
                               "Social att., n/a: 0.52 (0.36, 0.75)", "Disability pension: 0.17 (0.13, 0.24)")) +
  theme(axis.title.y = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle(label = 'Predictor; OR (95% CrI)') +
  theme(plot.title = element_text(size = 12, hjust = -0.62, face = "bold")) 
        
or_plt
ggsave(or_plt, file = "Figure-2-OR-plot.png", units="in", width = 8, height = 8, dpi = 300)
write.csv(odds_rat, file = "odds_rat.csv", row.names = F)

#### Odds ratios, changing base condition (i.e., when there is >2 levels in the factor) ####

##LEVEL_COMPLETENESS##
# Level_Completeness_2 vs 3
oddsrat = gather_draws(fit, b_level_completeness2, b_level_completeness3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_level_completeness3`-`b_level_completeness2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
# Level_Completeness_2 vs 4
oddsrat = gather_draws(fit, b_level_completeness2, b_level_completeness4) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_level_completeness4`-`b_level_completeness2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
# Level_Completeness_3 vs 4
oddsrat = gather_draws(fit, b_level_completeness3, b_level_completeness4) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_level_completeness4`-`b_level_completeness3`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##MARITAL STATUS##
# marital_status2 vs 3
oddsrat = gather_draws(fit, b_marital_status2, b_marital_status3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_marital_status3`-`b_marital_status2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##QUALITY OF LIFE##
#qol2 vs 1
oddsrat = gather_draws(fit, b_qol2, b_qol1) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_qol1`-`b_qol2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##GENERAL HEALTH##
#general_health2 vs 1
oddsrat = gather_draws(fit, b_general_health2, b_general_health1) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_general_health1`-`b_general_health2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##VOCATIONAL REHABILITATION##
#voc_rehab1 vs 3
oddsrat = gather_draws(fit, b_voc_rehab1, b_voc_rehab3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_voc_rehab3`-`b_voc_rehab1`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#voc_rehab1 vs 4
oddsrat = gather_draws(fit, b_voc_rehab1, b_voc_rehab4) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_voc_rehab4`-`b_voc_rehab1`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#voc_rehab3 vs 4
oddsrat = gather_draws(fit, b_voc_rehab3, b_voc_rehab4) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_voc_rehab4`-`b_voc_rehab3`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##HOSPITALISATIONS##
#hosp1 vs 2
oddsrat = gather_draws(fit, b_hosp1, b_hosp2) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_hosp2`-`b_hosp1`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#hosp1 vs 3
oddsrat = gather_draws(fit, b_hosp1, b_hosp3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_hosp3`-`b_hosp1`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#hosp2 vs 3
oddsrat = gather_draws(fit, b_hosp2, b_hosp3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_hosp3`-`b_hosp2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##GEOGRAPHY##
#geography2 vs 3
oddsrat = gather_draws(fit, b_geography2, b_geography3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_geography3`-`b_geography2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

##EDUCATION##
#education2 vs 3
oddsrat = gather_draws(fit, b_education2, b_education3) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education3`-`b_education2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education2 vs 4
oddsrat = gather_draws(fit, b_education2, b_education4) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education4`-`b_education2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education2 vs 5
oddsrat = gather_draws(fit, b_education2, b_education5) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education5`-`b_education2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education2 vs 6
oddsrat = gather_draws(fit, b_education2, b_education6) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education6`-`b_education2`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
oddsrat %>% mean_qi(exp(diff)>1)
#education3 vs 4
oddsrat = gather_draws(fit, b_education3, b_education4) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education4`-`b_education3`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education3 vs 5
oddsrat = gather_draws(fit, b_education3, b_education5) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education5`-`b_education3`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education3 vs 6
oddsrat = gather_draws(fit, b_education3, b_education6) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education6`-`b_education3`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
oddsrat %>% mean_qi(exp(diff)>1)
#education4 vs 5
oddsrat = gather_draws(fit, b_education4, b_education5) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education5`-`b_education4`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education4 vs 6
oddsrat = gather_draws(fit, b_education4, b_education6) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education6`-`b_education4`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
oddsrat %>% mean_qi(exp(diff)>1)
#education5 vs 6
oddsrat = gather_draws(fit, b_education5, b_education6) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education6`-`b_education5`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff)) 
oddsrat %>% mean_qi(exp(diff)>1)

##Reversed for in-text structuring

#education4 vs 2
oddsrat = gather_draws(fit, b_education4, b_education2) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education2`-`b_education4`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))
#education5 vs 2
oddsrat = gather_draws(fit, b_education5, b_education2) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  mutate(diff = `b_education2`-`b_education5`)
oddsrat %>% ggplot() + stat_halfeye(aes(x = exp(diff)), .width = c(0.66, 0.95)) + theme_bw(base_size = 14) + geom_vline(xintercept = 1, colour = "red")
oddsrat %>% mean_qi(exp(diff))

## END
