library(tidyverse)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(msm)
library(jtools)
library(performance)
library(estimatr)
library(modelsummary)
library(car)
library(lmtest)
library(scales)
library(sandwich)

source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_Data/04.analysis_data_final.RData")

df <- df %>% mutate(clinics_pc_scaled = scale(clinics_pc))

# for state fixed effects, need to remove federal district which is structured
# as a state with a single municipality
df_state_fe <- df %>% filter(state != "Distrito Federal") %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

# rescale clinics for shorter timeframe
df_state_fe_18 <- df %>% filter(state != "Distrito Federal" 
                                & as.numeric(as.character(year)) >= 2018) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

# scale clinics for full df with restricted timeline
df_18 <- df %>% filter(as.numeric(as.character(year)) >= 2018) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))


################################################################################
##
## models estimating vaccination
##
## 1a: 2006-2023, state FE
## 1b: 2018-2023, state FE
##
## 1c: 2006-2023, muni FE
## 1d: 2018-2023, muni FE
##
################################################################################

# model 1a
mod.1a <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + gini + pct_urban + 
               clinics_pc_scaled + cbr + state + year, 
             data = df_state_fe)
save(mod.1a, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1a.RData")

mod.1a_vcov <- vcovCL(mod.1a, cluster = ~ muni_code_6)
save(mod.1a_vcov, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1a_vcov.RData")


# model 1b
mod.1b <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + gini + pct_urban + 
               clinics_pc_scaled + cbr + state + year, 
             data = df_state_fe_18)
save(mod.1b, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1b.RData")

mod.1b_vcov <- vcovCL(mod.1b, cluster = ~ muni_code_6)
save(mod.1b_vcov, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1b_vcov.RData")


# model 1c (~45 minutes)
mod.1c <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + gini + pct_urban + 
               clinics_pc_scaled + cbr + muni_code_6 + year, 
             data = df)
save(mod.1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1c.RData")

mod.1c_vcov <- vcovCL(mod.1c, cluster = ~ muni_code_6)
save(mod.1c_vcov, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1c_vcov.RData")


# model 1d (~20 minutes)
# gini is removed since there is no municipality-level variation in recent years
mod.1d <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + pct_urban + 
               clinics_pc_scaled + cbr + muni_code_6 + year, 
             data = df_18)
save(mod.1d, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1d.RData")

mod.1d_vcov <- vcovCL(mod.1d, cluster = ~ muni_code_6)
save(mod.1d_vcov, file = "~/Brazil-measles/data/generated_data/models/vaccination/mod.1d_vcov.RData")


################################################################################
##
## table
##
################################################################################

# adjusted R squared
mod.1a_r2 <- summary(mod.1a)$adj.r.squared
mod.1b_r2 <- summary(mod.1b)$adj.r.squared
mod.1c_r2 <- summary(mod.1c)$adj.r.squared
mod.1d_r2 <- summary(mod.1d)$adj.r.squared

nobs(mod.1d)

mod1_tab <- merge(lm_tab(mod.1a, mod.1a_vcov) %>% rename(Estimate1 = Estimate, SE1 = SE), 
               lm_tab(mod.1b, mod.1b_vcov) %>% rename(Estimate2 = Estimate, SE2 = SE),
               by = "Term", all = T) %>% 
  merge(lm_tab(mod.1c, mod.1c_vcov) %>% rename(Estimate3 = Estimate, SE3 = SE),
        by = "Term", all = T) %>% 
  merge(lm_tab(mod.1d, mod.1d_vcov) %>% rename(Estimate4 = Estimate, SE4 = SE), 
        by = "Term", all = T) %>%
  filter(!str_detect(Term, "muni")) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       Estimate1 = as.character(nobs(mod.1a)),
                       Estimate2 = as.character(nobs(mod.1b)),
                       Estimate3 = as.character(nobs(mod.1c)),
                       Estimate4 = as.character(nobs(mod.1d)))) %>% 
  bind_rows(data.frame(Term = "Adj. $R^2$", 
                       Estimate1 = format(round(mod.1a_r2, 3), nsmall = 3),
                       Estimate2 = format(round(mod.1b_r2, 3), nsmall = 3),
                       Estimate3 = format(round(mod.1c_r2, 3), nsmall = 3),
                       Estimate4 = format(round(mod.1d_r2, 3), nsmall = 3))) %>% 
  arrange(match(Term, c("(Intercept)", "cbr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., "")),
         Term = str_replace_all(Term, "state|year", "\\\\hspace{3mm}"),
         SE4 = paste(SE4, "\\\\"),
         Term = recode(Term,
                       `(Intercept)` = "Intercept",
                       cbr = "CBR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))

write.table(mod1_tab, file = "", row.names = F, quote = F, sep = " & ")



# CIs for text
as.data.frame(coeftest(mod.1a, vcov = mod.1a_vcov)[,]) %>% 
  rownames_to_column(var = "term") %>% dplyr::select(term, Estimate) %>% 
  merge(as.data.frame(coefci(mod.1a, vcov = mod.1a_vcov)) %>% 
          rownames_to_column(var = "term"),
        by = "term") %>% 
  mutate(across(-term, ~round(.x, 2)))
#         Intercept    96.13  90.86 101.40
#               cbr    -0.23  -0.27  -0.18
# clinics_pc_scaled    -0.12  -0.25   0.02
#              gini    -0.06  -0.08  -0.04
#       log(gdp_pc)     0.53   0.26   0.81
#         pct_urban    -0.01  -0.02  -0.01
#      poverty_rate    -0.04  -0.06  -0.03

as.data.frame(coeftest(mod.1b, vcov = mod.1b_vcov)[,]) %>% 
  rownames_to_column(var = "term") %>% dplyr::select(term, Estimate) %>% 
  merge(as.data.frame(coefci(mod.1b, vcov = mod.1b_vcov)) %>% 
          rownames_to_column(var = "term"),
        by = "term") %>% 
  mutate(across(-term, ~round(.x, 2)))
#         Intercept    88.82  78.36  99.27
#               cbr    -0.44  -0.53  -0.34 - point estimate outside 1a CI - nearly doubles
# clinics_pc_scaled     0.50   0.25   0.74 - point estimate outside 1a CI - changes direction, is sig
#              gini    -0.13  -0.18  -0.09 - point estimate outside 1a CI - doubles
#       log(gdp_pc)     0.68   0.17   1.18 - point estimate within 1a CI  - similar
#         pct_urban    -0.05  -0.06  -0.03 - point estimate outside 1a CI - 5x bigger
#      poverty_rate    -0.06  -0.08  -0.03 - point estimate within 1a CI  - similar

