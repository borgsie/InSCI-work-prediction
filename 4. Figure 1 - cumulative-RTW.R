# INSCI prediction: Figure 1 cumulative RTW
# Author: SJ Borg and DN Borg
# Date: November 2021

library(bayesplot)
library(brms)
library(cowplot)
library(dplyr)
library(ggplot2)
library(janitor)
library(mice)
library(modelr)
library(naniar)
library(qwraps2)
library(Rcpp)
library(scales)
library(tidybayes)
library(tidyverse)
library(visdat)

setwd("~/Downloads") # Set working director
load("~/Downloads"/"fit_insci_07-06-21.RData") #Load model

### Figure 1: Cumulative time to resuming work ###

d = read.csv("cumulative time to rtw.csv") %>%
  clean_names()

d$cum_pct <- cumsum(d$indiv_pct)

cum_plot <- ggplot(data = d, aes(x = time_to_work_years, y = cumsum(indiv_pct))) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle=0, hjust = 0.5)) + 
  theme(text = element_text(size=12)) +
  # scale_x_discrete(labels = d$time_to_work_years) +
  scale_x_continuous(breaks = breaks_width(2)) +
  theme_bw() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  ylab("Cumulative percent") + xlab("Time to resuming/gaining work (years)")
ggsave(cum_plot, file = "cumulative time to RTW plot.png")

# End