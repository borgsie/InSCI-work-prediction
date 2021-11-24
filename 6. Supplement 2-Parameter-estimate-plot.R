# INSCI prediction: Figure 3 Parameter estimates
# Author: SJ Borg and DN Borg
# Date: November 2021

# Libraries
library(Rcpp)
library(janitor)
library(tidyverse)
library(dplyr)
library(tidybayes)
library(brms)
library(modelr)
library(ggplot2)
library(cowplot)
library(bayesplot)
library(cowplot)

setwd("~/Downloads") # Set working director
load("~/Downloads"/"fit_insci_07-06-21.RData") #Load model

# Set plot components
insci_theme = theme(panel.grid.major.x=element_blank(),
                    panel.grid.minor.x=element_blank(),
                    panel.grid.major.y=element_blank(),
                    panel.grid.minor.y=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank())
insci_labs = labs(x = expression(beta~"(logit)"), y = "Density")
insci_line = geom_vline(xintercept = 0, linetype = 1, color = "red")

plotly_build2 <- function(...) {
  p <- plotly_build(...)
  p$x[c("attrs", "visdat", "cur_data")] <- NULL
  p
}

# gender2
gather_draws(fit, b_gender2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Female") -> plot1

# age
gather_draws(fit, b_age) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Age") -> plot2

# marital_status2
gather_draws(fit, b_marital_status2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Married") -> plot3
# marital_status3
gather_draws(fit, b_marital_status3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Divorced") -> plot4

# level_completeness2
gather_draws(fit, b_level_completeness2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Tetra, incomplete") -> plot5
# level_completeness3
gather_draws(fit, b_level_completeness3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Para, complete") -> plot6
# level_completeness4
gather_draws(fit, b_level_completeness4) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Tetra, complete") -> plot7

# trauma2
gather_draws(fit, b_trauma2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Non-traumatic") -> plot8

# tsi
gather_draws(fit, b_tsi) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Time since injury") -> plot9

# general_health2
gather_draws(fit, b_general_health2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Gen. health, neither") -> plot10
# general_health1
gather_draws(fit, b_general_health1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Gen. health, poor") -> plot11

# second_health
gather_draws(fit, b_second_health) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Secondary cond.") -> plot12

# hosp1
gather_draws(fit, b_hosp1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"1 Hospitalisation")-> plot13
# hosp2
gather_draws(fit, b_hosp2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"2 Hospitalisations") -> plot14
# hosp3
gather_draws(fit, b_hosp3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~">2 Hospitalisations") -> plot15

# sf36
gather_draws(fit, b_sf36) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"SF-36") -> plot16

# pre_job1
gather_draws(fit, b_pre_job1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Pre-injury job") -> plot17

# education2
gather_draws(fit, b_education2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"High 2nd education") -> plot18
# education3
gather_draws(fit, b_education3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Post-2nd education") -> plot19
# education4
gather_draws(fit, b_education4) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Diploma") -> plot20
# education5
gather_draws(fit, b_education5) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Bachelor") -> plot21
# education6
gather_draws(fit, b_education6) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Masters") -> plot22

# disabil_pension1
gather_draws(fit, b_disabil_pension1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Disability pension") -> plot23

# daily_ass1
gather_draws(fit, b_daily_ass1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Daily assistance") -> plot24

# geography2
gather_draws(fit, b_geography2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Rural area") -> plot25
# geography3
gather_draws(fit, b_geography3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Remote area") -> plot26

# pain
gather_draws(fit, b_pain) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Pain scale") -> plot27

# activity
gather_draws(fit, b_activity) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Activity scale") -> plot28

# qol2
gather_draws(fit, b_qol1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"QoL, poor") -> plot29
# qol3
gather_draws(fit, b_qol2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"QoL, neither") -> plot30

# voc_rehab3
gather_draws(fit, b_voc_rehab3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Some VR use") -> plot31
# voc_rehab3
gather_draws(fit, b_voc_rehab4) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"A great deal VR") -> plot32
# voc_rehab4
gather_draws(fit, b_voc_rehab1) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"VR, not needed") -> plot33

# sleep2
gather_draws(fit, b_sleep2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Sleep, poor") -> plot34

# scim
gather_draws(fit, b_scim) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"m-SCIM-SR") -> plot35

# social_att1
gather_draws(fit, b_social_att2) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Attitude, influential") -> plot36
# social_att2
gather_draws(fit, b_social_att3) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Attitutde, N/A") -> plot37

# transport
gather_draws(fit, b_transport) %>% mutate(effect = .value) %>%
  ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) + insci_theme + insci_labs + insci_line +
  facet_grid(~"Transport mini-scale") -> plot38

# Panel of effects (logit)
plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
          plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18, plot19, plot20,
          plot21, plot22, plot23, plot24, plot25, plot26, plot27, plot28, plot29, plot30,
          plot31, plot32, plot33, plot34, plot35, plot36, plot37, plot38,
          ncol = 5, 
          nrow = 8,
          scale = 0.9, 
          align = 'v', axis = "lr")
ggsave(file = "parameter-estimate-plot.png", units="in", width = 8, height = 12, dpi = 300)

#### END
