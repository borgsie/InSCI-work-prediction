# INSCI prediction: Tables 1, 2 and 3
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

#### Table 1: Participant characteristics ####

d = read.csv("RTW Predictive modelling variables.csv") %>%
  clean_names()
data.frame(colnames(d)) #column names and index
names(d)

## Labels and cleaning 
d1 <- d %>%
  mutate(
    age = as.numeric(age_ak),
    disabil_pension = as.factor(q74), #0=No, 1=Yes
    education = as.factor(recode_factor(q7a, `1` = "1", `2` = "1", `3` = "2", `4` = "3", `5` = "4", `6` = "5", `7` = "6")), 
    # Categories for education 1=primary/lower secondary, 2=higher secondary, 3=post-secondary, 4=short tert (diploma), 5=bachelor, 6=masters or higher
    gender = as.factor(q1), #1=Male, 2=Female
    geography = as.factor(rurality_cat3), #1=capital/metro, 2=Lg/Sml rural, 3=other rural/remote
    level_completeness = as.factor(lev_comp_2), #1=para incomp; 2=tetra incomp; 3=para comp; 4=tetra comp
    marital_status = as.factor(recode_factor(marital_4_cat, `1` = "1", `2` = "2", `4` = "2", `3` = "3")), 
    # Categories for marital status 1=single; 2=married or partnership; 3=separated, divorced or widowed
    trauma = as.factor(moi), #1=traumatic, 2=non-traumatic
    tsi = as.numeric(doi_cont_ak)
  )

table1 <- 
  list("Characteristic" =
         list("Male" = ~ qwraps2::n_perc0(gender == "1", na_rm = TRUE),
              "Age, median (IQR)" = ~ qwraps2::median_iqr(age, na_rm = TRUE), 
              "Single" = ~ qwraps2::n_perc0(marital_status == "1", na_rm = TRUE), 
              "Married" = ~ qwraps2::n_perc0(marital_status == "2", na_rm = TRUE),
              "Separated, divorced or widowed" = ~ qwraps2::n_perc0(marital_status == "3", na_rm = TRUE),
              "Receiving disability pension" = ~ qwraps2::n_perc0(disabil_pension == "1", na_rm = TRUE),
              "Capital and metropolitan areas" = ~ qwraps2::n_perc0(geography == "1", na_rm = TRUE),
              "Large & small rural ares" = ~ qwraps2::n_perc0(geography == "2", na_rm = TRUE),
              "Other rural & remote areas" = ~ qwraps2::n_perc0(geography == "3", na_rm = TRUE)),
       "Education" =
         list("Primary or lower secondary" = ~ qwraps2::n_perc0(education == "1", na_rm = TRUE),
              "Higher secondary" = ~ qwraps2::n_perc0(education == "2", na_rm = TRUE),
              "Post-secondary*" = ~ qwraps2::n_perc0(education == "3", na_rm = TRUE),
              "Short-tertiary (e.g., diploma)" = ~ qwraps2::n_perc0(education == "4", na_rm = TRUE),
              "Bachelor, or equivalent" = ~ qwraps2::n_perc0(education == "5", na_rm = TRUE),
              "Masters or PhD" = ~ qwraps2::n_perc0(education == "6", na_rm = TRUE)),
       "Lesion characteristics" =
         list("Traumatic" = ~ qwraps2::n_perc0(trauma == "1", na_rm = TRUE),
              "Non-traumatic" = ~ qwraps2::n_perc0(trauma == "2", na_rm = TRUE),
              "Time since injury, median (IQR)" = ~ qwraps2::median_iqr(tsi, na_rm = TRUE),
              "Paraplegia, incomplete" = ~ qwraps2::n_perc0(level_completeness == "3", na_rm = TRUE), 
              "Paraplegia, complete" = ~ qwraps2::n_perc0(level_completeness == "4", na_rm = TRUE),
              "Tetraplegia, incomplete" = ~ qwraps2::n_perc0(level_completeness == "1", na_rm = TRUE),
              "Tetraplegia, complete" = ~ qwraps2::n_perc0(level_completeness == "2", na_rm = TRUE))
  )

## Table 1
total <- summary_table(d1, table1)
write.csv(total, "table-1.csv", row.names = TRUE)

## END of section

#### Table 2: Predictive model characteristics ####

d = read.csv("RTW Predictive modelling variables.csv") %>%
  clean_names()
names(d)[1] <- 'insci_id'
d1 = read.csv("RTW Descriptive paper.csv") %>%
  clean_names()
data.frame(colnames(d1))
d2 = subset(d1, select = c(1,61))
d3 <- left_join(d, d2, by = "insci_id")
names(d1)

## Labels and cleaning 
d1 <- d3 %>%
  mutate(
    activity = as.numeric(ap_tot),
    daily_ass = as.factor(q6b_3), #0=No, 1=Yes
    disabil_pension = as.factor(q74), #0=No, 1=Yes
    general_health = factor(gen_hlth_3cat, levels = c('3', '2', '1')), #reordered so good is first: #1=v. poor/poor, 2=neither, 3=good/v.good 
    hosp = as.factor(hospitalisations_cat), #0=no hops, 1=1 hospitalisation, 2=2 hosps, 3=3 or more hosps
    level_completeness = as.factor(lev_comp_2), #1=para incomp; 2=tetra incomp; 3=para comp; 4=tetra comp
    pain = as.numeric(q38),
    pre_job = as.factor(q71a), #0=no, 1=pre-injury job
    qol = factor(qo_l_cat3, levels = c('3', '2', '1')), #reordered so good is first, 1=poor/v.poor, 2=neither, 3=good/v.good 
    scim = as.numeric(scim), #higher score is higher functioning
    second_health = as.numeric(scs_45), #number of severe/extreme secondary conditions
    sf36 = as.numeric(sf36_vit),
    sleep = factor(recode_factor(sleep, `3` = "1", `4` = "1", `1` = "2", `2` = "2")), #Recoded to 1=very good/fairly good (REF), 2=very bad/fairly bad
    social_att = factor(recode(q93, `2` = "1", `3` = "2", `4` = "2", `1` = "3"), levels = c(1,2,3), labels = c("no effect", "some effect", "N/A")), #cats now 1=no effect, 2=some/lot of influence, 3=N/A
    transport = as.numeric(transport), #ranges from 2 to 8, higher numbers is more transport issues
    voc_rehab = factor(voc_rehab_4_cat, levels = c('2', '3', '4', '1')), #Reordered as 2=no didn't use (REF category), 3=small/some extent, 4=great deal, 1=didn't need VR 
    work = as.factor(q76), #0=No, 1=Yes
    work_after_rehab = factor(recode(q73_1, `1` = "1", `2` = "2", `3` = "2"), levels = c(1,2), labels = c("no work", "worked after rehab"))
  )

table2 <- 
  list("Characteristic" =
         list("Pre-injury employment" = ~ qwraps2::n_perc0(pre_job == "1", na_rm = TRUE),
              "Worked after inpatient rehabiliation period" = ~ qwraps2::n_perc0(work_after_rehab == "worked after rehab", na_rm = TRUE),
              "Worked immediately after rehabiliation" = ~ qwraps2::n_perc0(q73_1 == "2", na_rm = TRUE),
              "Resumed employment at a later stage" = ~ qwraps2::n_perc0(q73_1 == "3", na_rm = TRUE),
              "Currently in paid employment" = ~ qwraps2::n_perc0(work == "1", na_rm = TRUE),
              "Time to returning to work(months)*, median (IQR)" = ~ qwraps2::median_iqr(time_to_work, na_rm = TRUE),
              "VR, Did not use" = ~ qwraps2::n_perc0(voc_rehab == "2", na_rm = TRUE),
              "VR, Small use or some use" = ~ qwraps2::n_perc0(voc_rehab == "3", na_rm = TRUE),
              "VR, A great deal of use" = ~ qwraps2::n_perc0(voc_rehab == "4", na_rm = TRUE),
              "VR, Did not need" = ~ qwraps2::n_perc0(voc_rehab == "1", na_rm = TRUE),
              "Receives daily assistance, n (%)" = ~ qwraps2::n_perc0(daily_ass == "1", na_rm = TRUE),
              "m-SCIM-SR, median (IQR)" = ~ qwraps2::median_iqr(scim, na_rm = TRUE),
              "Health, Very good or excellent" = ~ qwraps2::n_perc0(general_health == "3", na_rm = TRUE),
              "Health, Good" = ~ qwraps2::n_perc0(general_health == "2", na_rm = TRUE),
              "Health, Fair or poor" = ~ qwraps2::n_perc0(general_health == "1", na_rm = TRUE),
              "Hospitalisations, None" = ~ qwraps2::n_perc0(hosp == "0", na_rm = TRUE),
              "Hospitalisations, One" = ~ qwraps2::n_perc0(hosp == "1", na_rm = TRUE),
              "Hospitalisations, Two" = ~ qwraps2::n_perc0(hosp == "2", na_rm = TRUE),
              "Hospitalisations, More than two" = ~ qwraps2::n_perc0(hosp == "3", na_rm = TRUE),
              "Severe secondary conditions sum, median (IQR)" = ~ qwraps2::median_iqr(second_health, na_rm = TRUE),
              "Pain, median (IQR)" = ~ qwraps2::median_iqr(pain, na_rm = TRUE), 
              "Transport measure, median (IQR)" = ~ qwraps2::median_iqr(transport, na_rm = TRUE), 
              "Activity and participation scale, median (IQR)" = ~ qwraps2::median_iqr(activity, na_rm = TRUE), 
              "No effect on life" = ~ qwraps2::n_perc0(social_att == "no effect", na_rm = TRUE),
              "Some or a lot of influence" = ~ qwraps2::n_perc0(social_att == "some effect", na_rm = TRUE),
              "Not applicable" = ~ qwraps2::n_perc0(social_att == "N/A", na_rm = TRUE),
              "SF-36 vitality scale, median (IQR)" = ~ qwraps2::median_iqr(sf36, na_rm = TRUE), 
              "QoL, Good or very good" = ~ qwraps2::n_perc0(qol == "3", na_rm = TRUE),
              "QoL, Neither" = ~ qwraps2::n_perc0(qol == "2", na_rm = TRUE),
              "QoL, Poor or very poor" = ~ qwraps2::n_perc0(qol == "1", na_rm = TRUE),
              "Sleep, Very good or fairly good" = ~ qwraps2::n_perc0(sleep == "1", na_rm = TRUE),
              "Sleep, Very bad or fairly bad" = ~ qwraps2::n_perc0(sleep == "2", na_rm = TRUE))
)              

total <- summary_table(d1, table2)
write.csv(total, "table-2.csv", row.names = TRUE)

# End of section
         
#### Table 3: Work integration measures ####

## Merge work integration variables into prediction variables
d = read.csv("insci-rtw-and-work-integration-measures.csv") %>%
  clean_names()

## Labels and cleaning 
work <- d %>%
  mutate(
    work_done = factor(recode(q79, `1` = "1", `2` = "2", `3` = "2", `4` = "3", `5` = "3"), levels = c(1,2,3), 
                       labels = c("no prob", "medium prob", "extreme prob")),
    work_access = factor(recode(q80, `1` = "1", `2` = "2", `3` = "2", `4` = "3", `5` = "3"), levels = c(1,2,3), 
                         labels = c("no prob", "medium prob", "extreme prob")),
    work_device = factor(recode(q81, `1` = "1", `2` = "2", `3` = "3", `4` = "3", `5` = "4", `6` = "5"), levels = c(1,2,3,4,5), 
                         labels = c("completely", "large extent", "some extent", "not at all", "do not need")),
    recognition = factor(recode(q82, `1` = "1", `2` = "1", `3` = "2", `4` = "2"), levels = c(1,2), 
                         labels = c("agree", "disagree")),
    salary = factor(recode(q83, `1` = "1", `2` = "1", `3` = "2", `4` = "2"), levels = c(1,2), 
                    labels = c("agree", "disagree"))
)

## Table 3
table3 <- 
  list("Work integrationProblems completing work requirements" =
         list("requirements, No problem" = ~ qwraps2::n_perc0(work_done == "no prob", na_rm = TRUE),
              "Mild to moderate problem" = ~ qwraps2::n_perc0(work_done == "medium prob", na_rm = TRUE),
              "Extreme problem" = ~ qwraps2::n_perc0(work_done == "extreme prob", na_rm = TRUE),
              "access, No problem" = ~ qwraps2::n_perc0(work_access == "no prob", na_rm = TRUE),
              "Mild to moderate problem" = ~ qwraps2::n_perc0(work_access == "medium prob", na_rm = TRUE),
              "Extreme problem" = ~ qwraps2::n_perc0(work_access == "extreme prob", na_rm = TRUE),
              "Completely" = ~ qwraps2::n_perc0(work_device == "completely", na_rm = TRUE),
              "To a large extent" = ~ qwraps2::n_perc0(work_device == "large extent", na_rm = TRUE),
              "To a small or some extent" = ~ qwraps2::n_perc0(work_device == "some extent", na_rm = TRUE),
              "Not at all" = ~ qwraps2::n_perc0(work_device == "not at all", na_rm = TRUE),
              "Do not have such a need" = ~ qwraps2::n_perc0(work_device == "do not need", na_rm = TRUE),
              "recognition, Strongly agree or agree" = ~ qwraps2::n_perc0(recognition == "agree", na_rm = TRUE),
              "Strongly disagree or disagree" = ~ qwraps2::n_perc0(recognition == "disagree", na_rm = TRUE),
              "salary, Strongly agree or agree" = ~ qwraps2::n_perc0(salary == "agree", na_rm = TRUE),
              "Strongly disagree or disagree" = ~ qwraps2::n_perc0(salary == "disagree", na_rm = TRUE))           
  )

total <- summary_table(work, table3)
write.csv(total, "table-3.csv", row.names = TRUE)

## END