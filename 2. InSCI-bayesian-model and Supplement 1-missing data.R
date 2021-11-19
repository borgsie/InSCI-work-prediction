# INSCI prediction: Bayesian model
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

# SET WORKING DIRECTORY HERE - setwd("~/Downloads")

d = read.csv("RTW Predictive modelling variables.csv") %>%
  clean_names()

# Data renaming, recoding
d_model <- d %>%
  mutate(
    activity = as.numeric(scale(ap_tot, center = T, scale = T)),
    #activity_cut = cut(ap_tot, breaks = c(0,17,31,55)),
    age = as.numeric(scale(age_ak, center = T, scale = T)),
    daily_ass = as.factor(q6b_3), #0=No, 1=Yes
    disabil_pension = as.factor(q74), #0=No, 1=Yes
    #education = as.factor(q7a),
    education = as.factor(recode_factor(q7a, `1` = "1", `2` = "1", `3` = "2", `4` = "3", `5` = "4", `6` = "5", `7` = "6")), 
    # Categories for education 1=primary/lower secondary, 2=higher secondary, 3=post-secondary, 4=short tert (diploma), 5=bachelor, 6=masters or higher
    gender = as.factor(q1), #1=Male, 2=Female
    general_health = factor(gen_hlth_3cat, levels = c('3', '2', '1')), #reordered so good is first: #1=v. poor/poor, 2=neither, 3=good/v.good 
    geography = as.factor(rurality_cat3), #1=capital/metro, 2=Lg/Sml rural, 3=other rural/remote
    hosp = as.factor(hospitalisations_cat), #0=no hops, 1=1 hospitalisation, 2=2 hosps, 3=3 or more hosps
    level_completeness = as.factor(lev_comp_2), #1=para incomp; 2=tetra incomp; 3=para comp; 4=tetra comp
    marital_status = as.factor(recode_factor(marital_4_cat, `1` = "1", `2` = "2", `4` = "2", `3` = "3")), 
    # Categories for marital status 1=single; 2=married or partnership; 3=separated, divorced or widowed
    pain = as.numeric(scale(q38, center = T, scale = T)),
    #pain_cut = cut(q38, breaks = c(0,3,8,10)),
    pre_job = as.factor(q71a), #0=no, 1=pre-injury job
    qol = factor(qo_l_cat3, levels = c('3', '2', '1')), #reordered so good is first, 1=poor/v.poor, 2=neither, 3=good/v.good 
    scim = as.numeric(scale(scim, center = T, scale = T)), #higher score is higher functioning
    #scim_cut = cut(scim, breaks = c(0,69.7,100)),
    #second_condition = as.numeric(scale(scs_tot, center = T, scale = T)),
    second_health = as.numeric(scale(scs_45, center = T, scale = T)), #number of severe/extreme secondary conditions
    #second_health_cut = cut(scs_45, breaks = c(0,1,5,13)),
    sf36 = as.numeric(scale(sf36_vit, center = T, scale = T)),
    sleep = factor(recode_factor(sleep, `3` = "1", `4` = "1", `1` = "2", `2` = "2")), #Recoded to 1=very good/fairly good (REF), 2=very bad/fairly bad
    social_att = as.factor(recode_factor(q93, `2` = "1", `3` = "2", `4` = "2", `1` = "3")), #cats now 1=no effect, 2=some/lot of influence, 3=N/A
    transport = as.numeric(scale(transport, center = T, scale = T)), #ranges from 2 to 8, higher numbers is more tranpsort issues
    trauma = as.factor(moi), #1=traumatic, 2=non-traumatic
    tsi = as.numeric(scale(doi_cont_ak, center = T, scale = T)),
    voc_rehab = factor(voc_rehab_4_cat, levels = c('2', '3', '4', '1')), #Reordered as 2=no didn't use (REF category), 3=small/some extent, 4=great deal, 1=didn't need VR 
    work = as.factor(q76) #0=No, 1=Yes
  )

d_model_dat <- d_model %>% select(
                      activity,
                      #activity_cut,                    
                      age,
                      daily_ass,
                      disabil_pension,
                      education,
                      gender,
                      general_health,
                      geography,
                      hosp,
                      level_completeness,
                      marital_status,
                      pain,
                      #pain_cut,
                      pre_job,
                      qol,
                      scim,
                      #scim_cut,
                      second_health,
                      #second_health_cut,
                      #seifa,
                      sleep,
                      sf36,
                      social_att,
                      transport,
                      trauma,
                      tsi,
                      voc_rehab,
                      work)


# Missing data
# https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
 d_model_dat %>% vis_miss() +
   theme(axis.text.x = element_text(angle = 90)) # %>% select()
 ggsave(file = "insci-missing-data.png", units="in", width = 10, height = 8, dpi = 300)

 
# Missing data imputation
# Columns with missingness
missing_vars <- sapply(d_model_dat, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

missing_vars

# impute using random forests
imp_datasets <- mice(d_model_dat, m = 5, method = "rf", seed = 123)

# look at where data is imputed to
stripplot(imp_datasets, activity, pch = 19, xlab = "Imputation number")
#stripplot(imp_datasets, activity_cut, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, disabil_pension, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, daily_ass, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, education, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, general_health, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, geography, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, hosp, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, level_completeness, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, marital_status, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, pre_job, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, pain, pch = 19, xlab = "Imputation number") # check due to % missings
stripplot(imp_datasets, qol, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, scim, pch = 19, xlab = "Imputation number") # check due to % missings
stripplot(imp_datasets, second_health, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, sf36, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, sleep, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, social_att, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, transport, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, trauma, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, tsi, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, voc_rehab, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, work, pch = 19, xlab = "Imputation number")

# Bayesian model
fit <- brm_multiple(work ~ .,
           data = imp_datasets,
           prior = c(set_prior(horseshoe(df = 1, par_ratio = 0.2), class = "b"), # lasso(df = 1, scale = 1)
                     set_prior("student_t(3, 0, 0.5)", class = "Intercept")), 
           family = bernoulli(link = "logit"),
           chains = 8, 
           cores = 8,
           iter = 5000,
           thin = 5,
           seed = 123,
           control = list(adapt_delta = 0.99, max_treedepth = 15))

# Check Rhats
round(fit$rhats, 3)

# Save model
save(fit, file = "fit_insci_07-06-21.RData")

# Predictive check and chains
pp_check(fit, re_formula = NULL, nsamples = 100)

# Check chain convergence
plot(fit)

# Model summary
summary(fit)

# Plot
conditional_effects(fit)

# Priors
prior_summary(fit_b)

# R2
print(bayes_R2(fit), digits = 2)

## END