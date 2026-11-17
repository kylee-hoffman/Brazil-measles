library(tidyverse)
library(pscl)
library(parameters)
library(stringr)
library(scales)
library(ggeffects)
library(MASS)
library(sandwich)
library(lmtest)
library(fixest)
library(ggplot2)
library(ggpubr)
library(gtools)
library(rsq)

source("~/Brazil-measles/code/utils.R")

################################################################################
##
## baseline models estimating NMID mortality
##
## 3a: d1, state FE
## 3b: d2, state FE
## 3c: d3, state FE
##
## 3d: d1, muni FE
## 3e: d2, muni FE
## 3f: d3, muni FE
## 
################################################################################

load("~/Brazil-measles/data/generated_data/04.analysis_data_final.RData")

df <- df %>% mutate(clinics_pc_scaled = scale(clinics_pc))

# for models with state FE
df_state_fe <- df %>% 
  filter(state != "Distrito Federal") %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

################################################################################
##
## baseline models: all ages
##
################################################################################

# 3a: 1-year duration, state FE
mod3a <- glm(nm_deaths_total ~ amnesia_prev_d1 + poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               state + year + offset(log(pop_total)), 
             family = quasipoisson, data = df_state_fe)

mod3a_vcov <- vcovCL(mod3a, cluster = ~ muni_code_6)

save(mod3a, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3a.RData")
save(mod3a_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3a_vcov.RData")

# 3b: 2-year duration, state FE
mod3b <- glm(nm_deaths_total ~ amnesia_prev_d2 + poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               state + year + offset(log(pop_total)), 
             family = quasipoisson, data = df_state_fe)

mod3b_vcov <- vcovCL(mod3b, cluster = ~ muni_code_6)

save(mod3b, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3b.RData")
save(mod3b_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3b_vcov.RData")

# 3c: 3-year duration, state FE
mod3c <- glm(nm_deaths_total ~ amnesia_prev_d3 + poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               state + year + offset(log(pop_total)), 
             family = quasipoisson, data = df_state_fe)

mod3c_vcov <- vcovCL(mod3c, cluster = ~ muni_code_6)

save(mod3c, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3c.RData")
save(mod3c_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3c_vcov.RData")


# 3d: 1-year duration, municipality FE
mod3d <- glm(nm_deaths_total ~ amnesia_prev_d1 + poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               muni_code_6 + year + offset(log(pop_total)), 
             family = quasipoisson, data = df)

mod3d_vcov <- vcovCL(mod3d, cluster = ~ muni_code_6)

save(mod3d, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3d.RData")
save(mod3d_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3d_vcov.RData")


# 3e: 2-year duration, municipality FE
mod3e <- glm(nm_deaths_total ~ amnesia_prev_d2 + poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               muni_code_6 + year + offset(log(pop_total)), 
             family = quasipoisson, data = df)

mod3e_vcov <- vcovCL(mod3e, cluster = ~ muni_code_6)

save(mod3e, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3e.RData")
save(mod3e_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3e_vcov.RData")


# 3f: 3-year duration, municipality FE
mod3f <- glm(nm_deaths_total ~ amnesia_prev_d3 + poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               muni_code_6 + year + offset(log(pop_total)), 
             family = quasipoisson, data = df)

mod3f_vcov <- vcovCL(mod3f, cluster = ~ muni_code_6)

save(mod3f, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3f.RData")
save(mod3f_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3f_vcov.RData")




# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3a.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3a_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3b.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3b_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3c.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3c_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3d.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3d_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3e.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3e_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3f.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3f_vcov.RData")

tab3a <- irr_tab_fun(mod3a, mod3a_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3b <- irr_tab_fun(mod3b, mod3b_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3c <- irr_tab_fun(mod3c, mod3c_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3d <- irr_tab_fun(mod3d, mod3d_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3e <- irr_tab_fun(mod3e, mod3e_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3f <- irr_tab_fun(mod3f, mod3f_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod3_tab <- bind_cols(tab3a, tab3b, tab3c, tab3d, tab3e, tab3f) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod3a)),
                       d1_within = as.character(nobs(mod3d)),
                       d2_between = as.character(nobs(mod3b)),
                       d2_within = as.character(nobs(mod3e)),
                       d3_between = as.character(nobs(mod3c)),
                       d3_within = as.character(nobs(mod3f)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod3a[["deviance"]] / mod3a[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod3d[["deviance"]] / mod3d[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod3b[["deviance"]] / mod3b[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod3e[["deviance"]] / mod3e[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod3c[["deviance"]] / mod3c[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod3f[["deviance"]] / mod3f[["null.deviance"]]), 3), nsmall = 3))) %>%
  arrange(match(Term, c("amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))

write.table(mod3_tab, file = "", quote = F, row.names = F, sep = " & ")






################################################################################
##
## baseline models: ages 1-9
##
################################################################################

# some municipalities report no NMID deaths in ages 1-9
# need to filter out for muni fixed effects
df_1to9_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(nm_deaths_1to9 > 0) > 0) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

# 3a: 1-year duration, state FE
mod3a_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d1 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_state_fe)

mod3a_1to9_vcov <- vcovCL(mod3a_1to9, cluster = ~ muni_code_6)

save(mod3a_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to9.RData")
save(mod3a_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to9_vcov.RData")

# 3b: 2-year duration, state FE
mod3b_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d2 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_state_fe)

mod3b_1to9_vcov <- vcovCL(mod3b_1to9, cluster = ~ muni_code_6)

save(mod3b_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to9.RData")
save(mod3b_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to9_vcov.RData")


# 3c: 3-year duration, state FE
mod3c_1to9 <- glm.nb(nm_deaths_1to9 ~ amnesia_prev_d3 + poverty_rate + gini + 
                       cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                       state + year + offset(log(pop_1to9)),
                     data = df_state_fe)

mod3c_1to9_vcov <- vcovCL(mod3c_1to9, cluster = ~ muni_code_6)

save(mod3c_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to9.RData")
save(mod3c_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to9_vcov.RData")


# 3d: 1-year duration, municipality FE
mod3d_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d1 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_1to9_muni_fe)

mod3d_1to9_vcov <- vcovCL(mod3d_1to9, cluster = ~ muni_code_6)

save(mod3d_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to9.RData")
save(mod3d_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to9_vcov.RData")


# 3e: 2-year duration, municipality FE
mod3e_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d2 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_1to9_muni_fe)

mod3e_1to9_vcov <- vcovCL(mod3e_1to9, cluster = ~ muni_code_6)

save(mod3e_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to9.RData")
save(mod3e_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to9_vcov.RData")


# 3f: 3-year duration, municipality FE
mod3f_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d3 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_1to9_muni_fe)

mod3f_1to9_vcov <- vcovCL(mod3f_1to9, cluster = ~ muni_code_6)

save(mod3f_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to9.RData")
save(mod3f_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to9_vcov.RData")



# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to9_vcov.RData")


tab3a_1to9 <- irr_tab_fun(mod3a_1to9, mod3a_1to9_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3b_1to9 <- irr_tab_fun(mod3b_1to9, mod3b_1to9_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3c_1to9 <- irr_tab_fun(mod3c_1to9, mod3c_1to9_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3d_1to9 <- irr_tab_fun(mod3d_1to9, mod3d_1to9_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3e_1to9 <- irr_tab_fun(mod3e_1to9, mod3e_1to9_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3f_1to9 <- irr_tab_fun(mod3f_1to9, mod3f_1to9_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod3_1to9_tab <- bind_cols(tab3a_1to9, tab3b_1to9, tab3c_1to9, 
                           tab3d_1to9, tab3e_1to9, tab3f_1to9) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod3a_1to9)),
                       d1_within = as.character(nobs(mod3d_1to9)),
                       d2_between = as.character(nobs(mod3b_1to9)),
                       d2_within = as.character(nobs(mod3e_1to9)),
                       d3_between = as.character(nobs(mod3c_1to9)),
                       d3_within = as.character(nobs(mod3f_1to9)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod3a_1to9[["deviance"]] / mod3a_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod3d_1to9[["deviance"]] / mod3d_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod3b_1to9[["deviance"]] / mod3b_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod3e_1to9[["deviance"]] / mod3e_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod3c_1to9[["deviance"]] / mod3c_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod3f_1to9[["deviance"]] / mod3f_1to9[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))

write.table(mod3_1to9_tab, file = "", quote = F, row.names = F, sep = " & ")



################################################################################
##
## baseline models, ages 1-19
##
################################################################################

# some municipalities report no NMID deaths in ages 1-19
# need to filter out for muni fixed effects
df_1to19_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(nm_deaths_1to19 > 0) > 0) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))


# 3a: 1-year duration, state FE
mod3a_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d1 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_state_fe)

mod3a_1to19_vcov <- vcovCL(mod3a_1to19, cluster = ~ muni_code_6)

save(mod3a_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to19.RData")
save(mod3a_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to19_vcov.RData")

# 3b: 2-year duration, state FE
mod3b_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d2 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_state_fe)

mod3b_1to19_vcov <- vcovCL(mod3b_1to19, cluster = ~ muni_code_6)

save(mod3b_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to19.RData")
save(mod3b_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to19_vcov.RData")


# 3c: 3-year duration, state FE
mod3c_1to19 <- glm.nb(nm_deaths_1to19 ~ amnesia_prev_d3 + poverty_rate + gini + 
                       cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                       state + year + offset(log(pop_1to19)),
                     data = df_state_fe)

mod3c_1to19_vcov <- vcovCL(mod3c_1to19, cluster = ~ muni_code_6)

save(mod3c_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to19.RData")
save(mod3c_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to19_vcov.RData")


# 3d: 1-year duration, municipality FE
mod3d_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d1 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_1to19_muni_fe)

mod3d_1to19_vcov <- vcovCL(mod3d_1to19, cluster = ~ muni_code_6)

save(mod3d_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to19.RData")
save(mod3d_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to19_vcov.RData")


# 3e: 2-year duration, municipality FE
mod3e_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d2 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_1to19_muni_fe)

mod3e_1to19_vcov <- vcovCL(mod3e_1to19, cluster = ~ muni_code_6)

save(mod3e_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to19.RData")
save(mod3e_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to19_vcov.RData")


# 3f: 3-year duration, municipality FE
mod3f_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d3 + poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_1to19_muni_fe)

mod3f_1to19_vcov <- vcovCL(mod3f_1to19, cluster = ~ muni_code_6)

save(mod3f_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to19.RData")
save(mod3f_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to19_vcov.RData")



# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to19_vcov.RData")

tab3a_1to19 <- irr_tab_fun(mod3a_1to19, mod3a_1to19_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3b_1to19 <- irr_tab_fun(mod3b_1to19, mod3b_1to19_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3c_1to19 <- irr_tab_fun(mod3c_1to19, mod3c_1to19_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3d_1to19 <- irr_tab_fun(mod3d_1to19, mod3d_1to19_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3e_1to19 <- irr_tab_fun(mod3e_1to19, mod3e_1to19_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3f_1to19 <- irr_tab_fun(mod3f_1to19, mod3f_1to19_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod3_1to19_tab <- bind_cols(tab3a_1to19, tab3b_1to19, tab3c_1to19, 
                           tab3d_1to19, tab3e_1to19, tab3f_1to19) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod3a_1to19)),
                       d1_within = as.character(nobs(mod3d_1to19)),
                       d2_between = as.character(nobs(mod3b_1to19)),
                       d2_within = as.character(nobs(mod3e_1to19)),
                       d3_between = as.character(nobs(mod3c_1to19)),
                       d3_within = as.character(nobs(mod3f_1to19)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod3a_1to19[["deviance"]] / mod3a_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod3d_1to19[["deviance"]] / mod3d_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod3b_1to19[["deviance"]] / mod3b_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod3e_1to19[["deviance"]] / mod3e_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod3c_1to19[["deviance"]] / mod3c_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod3f_1to19[["deviance"]] / mod3f_1to19[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))

write.table(mod3_1to19_tab, file = "", quote = F, row.names = F, sep = " & ")





################################################################################
##
## baseline models, ages 1-29
##
################################################################################

# some municipalities report no NMID deaths in ages 1-29
# need to filter out for muni fixed effects
df_1to29_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(nm_deaths_1to29 > 0) > 0) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))


# 3a: 1-year duration, state FE
mod3a_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d1 + poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     state + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_state_fe)

mod3a_1to29_vcov <- vcovCL(mod3a_1to29, cluster = ~ muni_code_6)

save(mod3a_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3a_1to29.RData")
save(mod3a_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3a_1to29_vcov.RData")

# 3b: 2-year duration, state FE
mod3b_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d2 + poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     state + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_state_fe)

mod3b_1to29_vcov <- vcovCL(mod3b_1to29, cluster = ~ muni_code_6)

save(mod3b_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3b_1to29.RData")
save(mod3b_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3b_1to29_vcov.RData")


# 3c: 3-year duration, state FE
mod3c_1to29 <- glm.nb(nm_deaths_1to29 ~ amnesia_prev_d3 + poverty_rate + gini + 
                        cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                        state + year + offset(log(pop_1to29)),
                      data = df_state_fe)

mod3c_1to29_vcov <- vcovCL(mod3c_1to29, cluster = ~ muni_code_6)

save(mod3c_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3c_1to29.RData")
save(mod3c_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3c_1to29_vcov.RData")


# 3d: 1-year duration, municipality FE
mod3d_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d1 + poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     muni_code_6 + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_1to29_muni_fe)

mod3d_1to29_vcov <- vcovCL(mod3d_1to29, cluster = ~ muni_code_6)

save(mod3d_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3d_1to29.RData")
save(mod3d_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3d_1to29_vcov.RData")

rm(mod3d_1to29, mod3d_1to29_vcov)
gc()


# 3e: 2-year duration, municipality FE
mod3e_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d2 + poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     muni_code_6 + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_1to29_muni_fe)

mod3e_1to29_vcov <- vcovCL(mod3e_1to29, cluster = ~ muni_code_6)

save(mod3e_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3e_1to29.RData")
save(mod3e_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3e_1to29_vcov.RData")

rm(mod3e_1to29, mod3e_1to29_vcov)
gc()

# 3f: 3-year duration, municipality FE
mod3f_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d3 + poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     muni_code_6 + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_1to29_muni_fe)

mod3f_1to29_vcov <- vcovCL(mod3f_1to29, cluster = ~ muni_code_6)

save(mod3f_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3f_1to29.RData")
save(mod3f_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod3f_1to29_vcov.RData")



# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3a_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3a_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3b_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3b_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3c_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3c_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3d_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3d_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3e_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3e_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3f_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3f_1to29_vcov.RData")

tab3a_1to29 <- irr_tab_fun(mod3a_1to29, mod3a_1to29_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3b_1to29 <- irr_tab_fun(mod3b_1to29, mod3b_1to29_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3c_1to29 <- irr_tab_fun(mod3c_1to29, mod3c_1to29_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3d_1to29 <- irr_tab_fun(mod3d_1to29, mod3d_1to29_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3e_1to29 <- irr_tab_fun(mod3e_1to29, mod3e_1to29_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab3f_1to29 <- irr_tab_fun(mod3f_1to29, mod3f_1to29_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod3_1to29_tab <- bind_cols(tab3a_1to29, tab3b_1to29, tab3c_1to29, 
                            tab3d_1to29, tab3e_1to29, tab3f_1to29) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod3a_1to29)),
                       d1_within = as.character(nobs(mod3d_1to29)),
                       d2_between = as.character(nobs(mod3b_1to29)),
                       d2_within = as.character(nobs(mod3e_1to29)),
                       d3_between = as.character(nobs(mod3c_1to29)),
                       d3_within = as.character(nobs(mod3f_1to29)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod3a_1to29[["deviance"]] / mod3a_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod3d_1to29[["deviance"]] / mod3d_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod3b_1to29[["deviance"]] / mod3b_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod3e_1to29[["deviance"]] / mod3e_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod3c_1to29[["deviance"]] / mod3c_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod3f_1to29[["deviance"]] / mod3f_1to29[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))

write.table(mod3_1to29_tab, file = "", quote = F, row.names = F, sep = " & ")






################################################################################
##
## models estimating NMID mortality with SES interaction
##
## 4a: d1, state FE
## 4b: d2, state FE
## 4c: d3, state FE
##
## 4d: d1, muni FE
## 4e: d2, muni FE
## 4f: d3, muni FE
## 
################################################################################

################################################################################
##
## interaction models, all ages
##
################################################################################
# 4a: 1-year duration, state FE
mod4a <- glm(nm_deaths_total ~ amnesia_prev_d1 * poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               state + year + offset(log(pop_total)), 
             family = quasipoisson, data = df_state_fe)

mod4a_vcov <- vcovCL(mod4a, cluster = ~ muni_code_6)

save(mod4a, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4a.RData")
save(mod4a_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4a_vcov.RData")


# 4b: 2-year duration, state FE
mod4b <- glm(nm_deaths_total ~ amnesia_prev_d2 * poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               state + year + offset(log(pop_total)), 
             family = quasipoisson, data = df_state_fe)

mod4b_vcov <- vcovCL(mod4b, cluster = ~ muni_code_6)

save(mod4b, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4b.RData")
save(mod4b_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4b_vcov.RData")


# 4c: 3-year duration, state FE
mod4c <- glm(nm_deaths_total ~ amnesia_prev_d3 * poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               state + year + offset(log(pop_total)), 
             family = quasipoisson, data = df_state_fe)

mod4c_vcov <- vcovCL(mod4c, cluster = ~ muni_code_6)

save(mod4c, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4c.RData")
save(mod4c_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4c_vcov.RData")


# 4d: 1-year duration, municipality FE
mod4d <- glm(nm_deaths_total ~ amnesia_prev_d1 * poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               muni_code_6 + year + offset(log(pop_total)), 
             family = quasipoisson, data = df)

mod4d_vcov <- vcovCL(mod4d, cluster = ~ muni_code_6)

save(mod4d, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4d.RData")
save(mod4d_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4d_vcov.RData")


# 4e: 2-year duration, municipality FE
mod4e <- glm(nm_deaths_total ~ amnesia_prev_d2 * poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               muni_code_6 + year + offset(log(pop_total)), 
             family = quasipoisson, data = df)

mod4e_vcov <- vcovCL(mod4e, cluster = ~ muni_code_6)

save(mod4e, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4e.RData")
save(mod4e_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4e_vcov.RData")


# 4f: 3-year duration, municipality FE
mod4f <- glm(nm_deaths_total ~ amnesia_prev_d3 * poverty_rate + gini + 
               cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
               muni_code_6 + year + offset(log(pop_total)), 
             family = quasipoisson, data = df)

mod4f_vcov <- vcovCL(mod4f, cluster = ~ muni_code_6)

save(mod4f, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4f.RData")
save(mod4f_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4f_vcov.RData")



# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4a.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4a_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4b.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4b_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4c.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4c_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4d.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4d_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4e.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4e_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4f.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4f_vcov.RData")


tab4a <- irr_tab_fun(mod4a, mod4a_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4b <- irr_tab_fun(mod4b, mod4b_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4c <- irr_tab_fun(mod4c, mod4c_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4d <- irr_tab_fun(mod4d, mod4d_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4e <- irr_tab_fun(mod4e, mod4e_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4f <- irr_tab_fun(mod4f, mod4f_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod4_tab <- bind_cols(tab4a, tab4b, tab4c, tab4d, tab4e, tab4f) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod4a)),
                       d1_within = as.character(nobs(mod4d)),
                       d2_between = as.character(nobs(mod4b)),
                       d2_within = as.character(nobs(mod4e)),
                       d3_between = as.character(nobs(mod4c)),
                       d3_within = as.character(nobs(mod4f)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod4a[["deviance"]] / mod4a[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod4d[["deviance"]] / mod4d[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod4b[["deviance"]] / mod4b[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod4e[["deviance"]] / mod4e[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod4c[["deviance"]] / mod4c[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod4f[["deviance"]] / mod4f[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1:poverty_rate", "amnesia_prev_d1", "cbr", "cdr", "imr", 
                        "clinics_pc_scaled", "gini", "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       `amnesia_prev_d1:poverty_rate` = "Amnesia $\\times$ Poverty",
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))

write.table(mod4_tab, file = "", quote = F, row.names = F, sep = " & ")



# predictions for text
predict_response(mod4b, 
                 terms = list(amnesia_prev_d2 = 9.30741,
                              poverty_rate = c(17.1, 48.5, 78.78)), 
                 margin = "marginalmeans", 
                 ci_level = 0.95,
                 vcov = mod4b_vcov,
                 type = "fixed",
                 rg.limit = 50000,
                 condition = c(pop_total = 10000))




################################################################################
##
## interaction models, ages 1-9
##
################################################################################

df_1to9_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(nm_deaths_1to9 > 0) > 0) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

# 4a: 1-year duration, state FE
mod4a_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d1 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_state_fe)

mod4a_1to9_vcov <- vcovCL(mod4a_1to9, cluster = ~ muni_code_6)

save(mod4a_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to9.RData")
save(mod4a_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to9_vcov.RData")


# 4b: 2-year duration, state FE
mod4b_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d2 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_state_fe)

mod4b_1to9_vcov <- vcovCL(mod4b_1to9, cluster = ~ muni_code_6)

save(mod4b_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to9.RData")
save(mod4b_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to9_vcov.RData")


# 4c: 3-year duration, state FE
mod4c_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d3 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_state_fe)

mod4c_1to9_vcov <- vcovCL(mod4c_1to9, cluster = ~ muni_code_6)

save(mod4c_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to9.RData")
save(mod4c_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to9_vcov.RData")


# 4d: 1-year duration, municipality FE
mod4d_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d1 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_1to9_muni_fe)

mod4d_1to9_vcov <- vcovCL(mod4d_1to9, cluster = ~ muni_code_6)

save(mod4d_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to9.RData")
save(mod4d_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to9_vcov.RData")


# 4e: 2-year duration, municipality FE
mod4e_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d2 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_1to9_muni_fe)

mod4e_1to9_vcov <- vcovCL(mod4e_1to9, cluster = ~ muni_code_6)

save(mod4e_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to9.RData")
save(mod4e_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to9_vcov.RData")


# 4f: 3-year duration, municiplaity FE
mod4f_1to9 <- glm(nm_deaths_1to9 ~ amnesia_prev_d3 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to9)), 
                  family = quasipoisson, data = df_1to9_muni_fe)

mod4f_1to9_vcov <- vcovCL(mod4f_1to9, cluster = ~ muni_code_6)

save(mod4f_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to9.RData")
save(mod4f_1to9_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to9_vcov.RData")


# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to9_vcov.RData")


tab4a_1to9 <- irr_tab_fun(mod4a_1to9, mod4a_1to9_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4b_1to9 <- irr_tab_fun(mod4b_1to9, mod4b_1to9_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4c_1to9 <- irr_tab_fun(mod4c_1to9, mod4c_1to9_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4d_1to9 <- irr_tab_fun(mod4d_1to9, mod4d_1to9_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4e_1to9 <- irr_tab_fun(mod4e_1to9, mod4e_1to9_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4f_1to9 <- irr_tab_fun(mod4f_1to9, mod4f_1to9_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod4_1to9_tab <- bind_cols(tab4a_1to9, tab4b_1to9, tab4c_1to9, 
                           tab4d_1to9, tab4e_1to9, tab4f_1to9) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod4a_1to9)),
                       d1_within = as.character(nobs(mod4d_1to9)),
                       d2_between = as.character(nobs(mod4b_1to9)),
                       d2_within = as.character(nobs(mod4e_1to9)),
                       d3_between = as.character(nobs(mod4c_1to9)),
                       d3_within = as.character(nobs(mod4f_1to9)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod4a_1to9[["deviance"]] / mod4a_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod4d_1to9[["deviance"]] / mod4d_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod4b_1to9[["deviance"]] / mod4b_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod4e_1to9[["deviance"]] / mod4e_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod4c_1to9[["deviance"]] / mod4c_1to9[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod4f_1to9[["deviance"]] / mod4f_1to9[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1:poverty_rate", "amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       `amnesia_prev_d1:poverty_rate` = "Amnesia $\\times$ Poverty",
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))


write.table(mod4_1to9_tab, file = "", quote = F, row.names = F, sep = " & ")





################################################################################
##
## interaction models, ages 1-19
##
################################################################################

df_1to19_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(nm_deaths_1to19 > 0) > 0) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

# 4a: 1-year duration, state FE
mod4a_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d1 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_state_fe)

mod4a_1to19_vcov <- vcovCL(mod4a_1to19, cluster = ~ muni_code_6)

save(mod4a_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to19.RData")
save(mod4a_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to19_vcov.RData")


# 4b: 2-year duration, state FE
mod4b_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d2 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_state_fe)

mod4b_1to19_vcov <- vcovCL(mod4b_1to19, cluster = ~ muni_code_6)

save(mod4b_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to19.RData")
save(mod4b_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to19_vcov.RData")


# 4c: 3-year duration, state FE
mod4c_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d3 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    state + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_state_fe)

mod4c_1to19_vcov <- vcovCL(mod4c_1to19, cluster = ~ muni_code_6)

save(mod4c_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to19.RData")
save(mod4c_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to19_vcov.RData")


# 4d: 1-year duration, municipality FE
mod4d_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d1 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_1to19_muni_fe)

mod4d_1to19_vcov <- vcovCL(mod4d_1to19, cluster = ~ muni_code_6)

save(mod4d_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to19.RData")
save(mod4d_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to19_vcov.RData")


# 4e: 2-year duration, municipality FE
mod4e_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d2 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_1to19_muni_fe)

mod4e_1to19_vcov <- vcovCL(mod4e_1to19, cluster = ~ muni_code_6)

save(mod4e_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to19.RData")
save(mod4e_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to19_vcov.RData")


# 4f: 3-year duration, municipality FE
mod4f_1to19 <- glm(nm_deaths_1to19 ~ amnesia_prev_d3 * poverty_rate + gini + 
                    cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                    muni_code_6 + year + offset(log(pop_1to19)), 
                  family = quasipoisson, data = df_1to19_muni_fe)

mod4f_1to19_vcov <- vcovCL(mod4f_1to19, cluster = ~ muni_code_6)

save(mod4f_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to19.RData")
save(mod4f_1to19_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to19_vcov.RData")


# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to19_vcov.RData")


tab4a_1to19 <- irr_tab_fun(mod4a_1to19, mod4a_1to19_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4b_1to19 <- irr_tab_fun(mod4b_1to19, mod4b_1to19_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4c_1to19 <- irr_tab_fun(mod4c_1to19, mod4c_1to19_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4d_1to19 <- irr_tab_fun(mod4d_1to19, mod4d_1to19_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4e_1to19 <- irr_tab_fun(mod4e_1to19, mod4e_1to19_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4f_1to19 <- irr_tab_fun(mod4f_1to19, mod4f_1to19_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod4_1to19_tab <- bind_cols(tab4a_1to19, tab4b_1to19, tab4c_1to19, 
                           tab4d_1to19, tab4e_1to19, tab4f_1to19) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod4a_1to19)),
                       d1_within = as.character(nobs(mod4d_1to19)),
                       d2_between = as.character(nobs(mod4b_1to19)),
                       d2_within = as.character(nobs(mod4e_1to19)),
                       d3_between = as.character(nobs(mod4c_1to19)),
                       d3_within = as.character(nobs(mod4f_1to19)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod4a_1to19[["deviance"]] / mod4a_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod4d_1to19[["deviance"]] / mod4d_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod4b_1to19[["deviance"]] / mod4b_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod4e_1to19[["deviance"]] / mod4e_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod4c_1to19[["deviance"]] / mod4c_1to19[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod4f_1to19[["deviance"]] / mod4f_1to19[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1:poverty_rate", "amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       `amnesia_prev_d1:poverty_rate` = "Amnesia $\\times$ Poverty",
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))


write.table(mod4_1to19_tab, file = "", quote = F, row.names = F, sep = " & ")





################################################################################
##
## interaction models, ages 1-29
##
################################################################################

df_1to29_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(nm_deaths_1to29 > 0) > 0) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

# 4a: 1-year duration, state FE
mod4a_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d1 * poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     state + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_state_fe)

mod4a_1to29_vcov <- vcovCL(mod4a_1to29, cluster = ~ muni_code_6)

save(mod4a_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4a_1to29.RData")
save(mod4a_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4a_1to29_vcov.RData")


# 4b: 2-year duration, state FE
mod4b_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d2 * poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     state + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_state_fe)

mod4b_1to29_vcov <- vcovCL(mod4b_1to29, cluster = ~ muni_code_6)

save(mod4b_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4b_1to29.RData")
save(mod4b_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4b_1to29_vcov.RData")


# 4c: 3-year duration, state FE
mod4c_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d3 * poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     state + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_state_fe)

mod4c_1to29_vcov <- vcovCL(mod4c_1to29, cluster = ~ muni_code_6)

save(mod4c_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4c_1to29.RData")
save(mod4c_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4c_1to29_vcov.RData")


# 4d: 1-year duration, municipality FE
mod4d_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d1 * poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     muni_code_6 + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_1to29_muni_fe)

mod4d_1to29_vcov <- vcovCL(mod4d_1to29, cluster = ~ muni_code_6)

save(mod4d_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4d_1to29.RData")
save(mod4d_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4d_1to29_vcov.RData")


# 4e: 2-year duration, municipality FE
mod4e_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d2 * poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     muni_code_6 + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_1to29_muni_fe)

mod4e_1to29_vcov <- vcovCL(mod4e_1to29, cluster = ~ muni_code_6)

save(mod4e_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4e_1to29.RData")
save(mod4e_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4e_1to29_vcov.RData")


# 4f: 3-year duration, municipality FE
mod4f_1to29 <- glm(nm_deaths_1to29 ~ amnesia_prev_d3 * poverty_rate + gini + 
                     cdr + cbr + imr + pct_urban + log(gdp_pc) + clinics_pc_scaled + 
                     muni_code_6 + year + offset(log(pop_1to29)), 
                   family = quasipoisson, data = df_1to29_muni_fe)

mod4f_1to29_vcov <- vcovCL(mod4f_1to29, cluster = ~ muni_code_6)

save(mod4f_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4f_1to29.RData")
save(mod4f_1to29_vcov, file = "~/Brazil-measles/data/generated_data/models/NMID/mod4f_1to29_vcov.RData")


# table
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4a_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4a_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4b_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4b_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4c_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4c_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4d_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4d_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4e_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4e_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod4f_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4f_1to29_vcov.RData")


tab4a_1to29 <- irr_tab_fun(mod4a_1to29, mod4a_1to29_vcov) %>%
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4b_1to29 <- irr_tab_fun(mod4b_1to29, mod4b_1to29_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4c_1to29 <- irr_tab_fun(mod4c_1to29, mod4c_1to29_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_between") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4d_1to29 <- irr_tab_fun(mod4d_1to29, mod4d_1to29_vcov) %>% 
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d1_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4e_1to29 <- irr_tab_fun(mod4e_1to29, mod4e_1to29_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d2_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))

tab4f_1to29 <- irr_tab_fun(mod4f_1to29, mod4f_1to29_vcov) %>%  
  pivot_longer(cols = c(IRR, Robust.CI), values_to = "d3_within") %>% 
  dplyr::select(-name) %>% filter(!str_detect(Term, "Intercept|state|year"))


mod4_1to29_tab <- bind_cols(tab4a_1to29, tab4b_1to29, tab4c_1to29, 
                            tab4d_1to29, tab4e_1to29, tab4f_1to29) %>%
  dplyr::select(Term = `Term...1`, d1_between, d1_within, d2_between, d2_within,
                d3_between, d3_within) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       d1_between = as.character(nobs(mod4a_1to29)),
                       d1_within = as.character(nobs(mod4d_1to29)),
                       d2_between = as.character(nobs(mod4b_1to29)),
                       d2_within = as.character(nobs(mod4e_1to29)),
                       d3_between = as.character(nobs(mod4c_1to29)),
                       d3_within = as.character(nobs(mod4f_1to29)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       d1_between = format(round(1 - (mod4a_1to29[["deviance"]] / mod4a_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d1_within = format(round(1 - (mod4d_1to29[["deviance"]] / mod4d_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d2_between = format(round(1 - (mod4b_1to29[["deviance"]] / mod4b_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d2_within = format(round(1 - (mod4e_1to29[["deviance"]] / mod4e_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d3_between = format(round(1 - (mod4c_1to29[["deviance"]] / mod4c_1to29[["null.deviance"]]), 3), nsmall = 3),
                       d3_within = format(round(1 - (mod4f_1to29[["deviance"]] / mod4f_1to29[["null.deviance"]]), 3), nsmall = 3))) %>% 
  arrange(match(Term, c("amnesia_prev_d1:poverty_rate", "amnesia_prev_d1", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate"))) %>% 
  mutate(across(everything(), ~replace_na(., " ")),
         d3_within = paste(d3_within, "\\\\"),
         Term = recode(Term,
                       `amnesia_prev_d1:poverty_rate` = "Amnesia $\\times$ Poverty",
                       amnesia_prev_d1 = "Amnesia",
                       cdr = "CDR",
                       cbr = "CBR",
                       imr = "IMR",
                       clinics_pc_scaled = "Clinics PC",
                       gini = "Gini",
                       `log(gdp_pc)` = "log(GDP PC)",
                       pct_urban = "\\%Urban",
                       poverty_rate = "Poverty"))


write.table(mod4_1to29_tab, file = "", quote = F, row.names = F, sep = " & ")
