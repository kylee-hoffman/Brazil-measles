library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(MASS)
library(ggeffects)
library(sandwich)
library(clubSandwich)
library(parameters)
library(performance)
library(legendry)
library(ggnewscale)
library(scales)
library(stringr)

source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/04.analysis_data_final.RData")

df <- df %>%
  mutate(travel = factor(travel),
         herd_immunity_lag2 = factor(ifelse(mcv_d1_cov_tc_lag2 >= 93, 1, 0)),
         year_n = as.numeric(as.character(year)),
         period = cut(year_n, c(2005, 2010, 2013, 2017, 2018, 2019, 2020, 2021, 2023),
                      labels = c("2006-2010", "2011-2013", "2014-17", "2018", "2019", "2020", 
                                 "2021", "2022-23")),
         clinics_pc_scaled = scale(clinics_pc))


# for models with state FE
df_state_fe <- df %>% 
  filter(!state %in% c("Mato Grosso", "Tocantins", "Acre", "Distrito Federal")) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

df_state_fe_18 <- df_state_fe %>% 
  filter(as.numeric(as.character(year)) >= 2018) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc)) # rescale for diff time period


# for models with municipality FE
# filter out municipalities with < 2 positive case observations
df_muni_fe <- df %>% 
  group_by(muni_code_6) %>% 
  filter(sum(mv_cases_total > 0) > 1) %>% ungroup() %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))

df_muni_fe_18 <- df %>% 
  filter(as.numeric(as.character(year)) >= 2018) %>% 
  group_by(muni_code_6) %>% 
  filter(sum(mv_cases_total > 0) > 1) %>% ungroup() %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))


################################################################################
##
## try poisson model
##
################################################################################

poisson.mod <- glm(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + 
                     poverty_rate + pct_urban + gini + cbr + travel + state + year +
                     offset(log(pop_total)),
                   family = poisson, data = df_state_fe)

summary(poisson.mod)

check_overdispersion(poisson.mod) # very overdispersed


# quasipoisson
mod5a <- glm(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + 
                poverty_rate + pct_urban + gini + cbr + cdr + imr + travel + state + year +
                offset(log(pop_total)),
              family = quasipoisson, data = df_state_fe)

mod5a_vcov <- vcovCL(mod5a, cluster = ~ muni_code_6)

save(mod5a, file = "~/Brazil-measles/data/generated_data/models/cases/mod5a.RData")
save(mod5a_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5a_vcov.RData")



mod5b <- glm(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + 
               poverty_rate + pct_urban + gini + cbr + cdr + imr + travel + state + year +
               offset(log(pop_total)),
             family = quasipoisson, data = df_state_fe_18)

mod5b_vcov <- vcovCL(mod5b, cluster = ~ muni_code_6)

save(mod5b, file = "~/Brazil-measles/data/generated_data/models/cases/mod5b.RData")
save(mod5b_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5b_vcov.RData")



mod5c <- glm(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + 
               poverty_rate + pct_urban + gini + cbr + cdr + imr + muni_code_6 + period +
               offset(log(pop_total)),
             family = quasipoisson, data = df_muni_fe)

mod5c_vcov <- vcovCL(mod5c, cluster = ~ muni_code_6)

save(mod5c, file = "~/Brazil-measles/data/generated_data/models/cases/mod5c.RData")
save(mod5c_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5c_vcov.RData")


mod5d <- glm(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled +
               poverty_rate + pct_urban + cbr + cdr + imr + muni_code_6 + period +
               offset(log(pop_total)),
             family = quasipoisson, 
             data = df_muni_fe_18)

mod5d_vcov <- vcovCL(mod5d, cluster = ~ muni_code_6)

save(mod5d, file = "~/Brazil-measles/data/generated_data/models/cases/mod5d.RData")
save(mod5d_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5d_vcov.RData")




# table
mod5_irrs <- merge(irr_tab_fun(mod5a, mod5a_vcov) %>% rename(IRR1 = IRR, Robust.CI1 = Robust.CI),
                  irr_tab_fun(mod5b, mod5b_vcov) %>% rename(IRR2 = IRR, Robust.CI2 = Robust.CI),,
                  by = "Term", all = T) %>% 
  merge(irr_tab_fun(mod5c, mod5c_vcov) %>% rename(IRR3 = IRR, Robust.CI3 = Robust.CI),
        by = "Term", all = T) %>% 
  merge(irr_tab_fun(mod5d, mod5d_vcov) %>% rename(IRR4 = IRR, Robust.CI4 = Robust.CI),
        by = "Term", all = T) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       IRR1 = as.character(nobs(mod5a)),
                       IRR2 = as.character(nobs(mod5b)),
                       IRR3 = as.character(nobs(mod5c)),
                       IRR4 = as.character(nobs(mod5d)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       IRR1 = format(round(1 - (mod5a[["deviance"]] / mod5a[["null.deviance"]]), 2), nsmall = 2),
                       IRR2 = format(round(1 - (mod5b[["deviance"]] / mod5b[["null.deviance"]]), 2), nsmall = 2),
                       IRR3 = format(round(1 - (mod5c[["deviance"]] / mod5c[["null.deviance"]]), 2), nsmall = 2),
                       IRR4 = format(round(1 - (mod5d[["deviance"]] / mod5d[["null.deviance"]]), 2), nsmall = 2))) %>% 
  mutate(across(everything(), ~replace_na(., "")),
         Term = str_replace_all(Term, "period|year", "\\\\hspace{3mm}"),
         Robust.CI4 = paste(Robust.CI4, "\\\\")) %>% 
  filter(!str_detect(Term, "Intercept")) %>% 
  arrange(match(Term, c("mcv_d1_cov_tc_lag2", "cbr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate", "travel1")))

write.table(mod5_irrs, file = "", row.names = F, quote = F, sep = " & \t")



################################################################################
##
## swapping in herd immunity flag
##
################################################################################

mod5a_herd <- glm(mv_cases_total ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + 
               poverty_rate + pct_urban + gini + cbr + cdr + imr + travel + state + year +
               offset(log(pop_total)),
             family = quasipoisson, data = df_state_fe)

mod5a_herd_vcov <- vcovCL(mod5a_herd, cluster = ~ muni_code_6)

save(mod5a_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod5a_herd.RData")
save(mod5a_herd_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5a_herd_vcov.RData")



mod5b_herd <- glm(mv_cases_total ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + 
               poverty_rate + pct_urban + gini + cbr + cdr + imr + travel + state + year +
               offset(log(pop_total)),
             family = quasipoisson, data = df_state_fe_18)

mod5b_herd_vcov <- vcovCL(mod5b_herd, cluster = ~ muni_code_6)

save(mod5b_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod5b_herd.RData")
save(mod5b_herd_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5b_herd_vcov.RData")



mod5c_herd <- glm(mv_cases_total ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + 
               poverty_rate + pct_urban + gini + cbr + cdr + imr + muni_code_6 + period +
               offset(log(pop_total)),
             family = quasipoisson, data = df_muni_fe)

mod5c_herd_vcov <- vcovCL(mod5c_herd, cluster = ~ muni_code_6)

save(mod5c_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod5c_herd.RData")
save(mod5c_herd_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5c_herd_vcov.RData")


mod5d_herd <- glm(mv_cases_total ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled +
               poverty_rate + pct_urban + cbr + cdr + imr + muni_code_6 + period +
               offset(log(pop_total)),
             family = quasipoisson, 
             data = df_muni_fe_18)

mod5d_herd_vcov <- vcovCL(mod5d_herd, cluster = ~ muni_code_6)

save(mod5d_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod5d_herd.RData")
save(mod5d_herd_vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod5d_herd_vcov.RData")


# table
mod5_herd_irrs <- merge(irr_tab_fun(mod5a_herd, mod5a_herd_vcov) %>% rename(IRR1 = IRR, Robust.CI1 = Robust.CI),
                   irr_tab_fun(mod5b_herd, mod5b_herd_vcov) %>% rename(IRR2 = IRR, Robust.CI2 = Robust.CI),,
                   by = "Term", all = T) %>% 
  merge(irr_tab_fun(mod5c_herd, mod5c_herd_vcov) %>% rename(IRR3 = IRR, Robust.CI3 = Robust.CI),
        by = "Term", all = T) %>% 
  merge(irr_tab_fun(mod5d_herd, mod5d_herd_vcov) %>% rename(IRR4 = IRR, Robust.CI4 = Robust.CI),
        by = "Term", all = T) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       IRR1 = as.character(nobs(mod5a_herd)),
                       IRR2 = as.character(nobs(mod5b_herd)),
                       IRR3 = as.character(nobs(mod5c_herd)),
                       IRR4 = as.character(nobs(mod5d_herd)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       IRR1 = format(round(1 - (mod5a_herd[["deviance"]] / mod5a_herd[["null.deviance"]]), 2), nsmall = 2),
                       IRR2 = format(round(1 - (mod5b_herd[["deviance"]] / mod5b_herd[["null.deviance"]]), 2), nsmall = 2),
                       IRR3 = format(round(1 - (mod5c_herd[["deviance"]] / mod5c_herd[["null.deviance"]]), 2), nsmall = 2),
                       IRR4 = format(round(1 - (mod5d_herd[["deviance"]] / mod5d_herd[["null.deviance"]]), 2), nsmall = 2))) %>% 
  arrange(match(Term, c("herd_immunity_lag21", "cbr", "cdr", "imr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate", "travel1"))) %>% 
  mutate(across(everything(), ~replace_na(., "")),
         Term = str_replace_all(Term, "period|year", "\\\\hspace{3mm}"),
         Robust.CI4 = paste(Robust.CI4, "\\\\"),
         Term = dplyr::recode(Term,
                              herd_immunity_lag21 = "Herd immunity",
                              cbr = "CBR",
                              cdr = "CDR",
                              imr = "IMR",
                              clinics_pc_scaled = "Clinics PC",
                              gini = "Gini",
                              `log(gdp_pc)` = "log(GDP PC)",
                              pct_urban = "\\%Urban",
                              poverty_rate = "Poverty",
                              travel1 = "Travel")) %>% 
  filter(!str_detect(Term, "Intercept"))

write.table(mod5_herd_irrs, file = "", row.names = F, quote = F, sep = " & ")
