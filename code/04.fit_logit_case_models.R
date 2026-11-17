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


df <- df %>% mutate(has_case = factor(ifelse(mv_cases_total > 0, 1, 0)),
                    herd_immunity_lag2 = factor(ifelse(mcv_d1_cov_tc_lag2 >= 93, 1, 0)),
                    travel = factor(travel),
                    year_n = as.numeric(as.character(year)),
                    period = cut(year_n, c(2005, 2010, 2013, 2017, 2018, 2019, 2020, 2021, 2023),
                                 labels = c("2006-2010", "2011-2013", "2014-17", "2018", "2019", "2020", 
                                            "2021", "2022-23")),
                    clinics_pc_scaled = scale(clinics_pc))


# for models with state FE
# check how many munis in each state have ever experienced more than 1 case
df %>% group_by(state) %>% 
  summarise(n_munis = n_distinct(muni_code_6[mv_cases_total > 1])) %>% print(n = 27)
# states with 0 munis that have ever experienced more than 1 case: Mato Grosso, Acre, Tocantins
# also the federal district consists of just 1 muni

df_state_fe <- df %>% filter(!state %in% c("Mato Grosso", "Tocantins", "Acre", "Distrito Federal")) %>% 
  mutate(clinics_pc_scaled = scale(clinics_pc))


# check how many munis in each state have ever experienced more than 1 case, 2018+
df %>% group_by(state) %>% 
  filter(as.numeric(as.character(year)) >= 2018) %>% 
  summarise(n_munis = n_distinct(muni_code_6[mv_cases_total > 1])) %>% print(n = 27)
# same as full time series, no more filtering needed

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
##
## binomial models estimating whether there's a case or not
##
##
################################################################################

# 2a: between-municipalities, all years, restricted to states with >1 observation of positive cases
mod2a <- glm(has_case ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + poverty_rate + 
               pct_urban + gini + cbr + imr + cdr + travel + state + year,
             data = df_state_fe, family = binomial(link = "logit"))

mod2a.vcov <- vcovCL(mod2a, cluster = ~ muni_code_6)

save(mod2a, file = "~/Brazil-measles/data/generated_data/models/cases/mod2a.RData")
save(mod2a.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2a_vcov.RData")


# 2b: between-municipalities, only 2018-2023, restricted to states with >1 observation of positive cases
mod2b <- glm(has_case ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + poverty_rate + 
               pct_urban + gini + cbr + imr + cdr + travel + state + year,
             data = df_state_fe_18, family = binomial(link = "logit"))

mod2b.vcov <- vcovCL(mod2b, cluster = ~ muni_code_6)

save(mod2b, file = "~/Brazil-measles/data/generated_data/models/cases/mod2b.RData")
save(mod2b.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2b_vcov.RData")



# 2c: within-municipality, all years, restricted to munis with >1 observation of positive cases
mod2c <- glm(has_case ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + poverty_rate + 
               pct_urban + gini + cbr + imr + cdr + muni_code_6 + period,
             data = df_muni_fe, family = binomial(link = "logit"))

mod2c.vcov <- vcovCL(mod2c, cluster = ~ muni_code_6)

save(mod2c, file = "~/Brazil-measles/data/generated_data/models/cases/mod2c.RData")
save(mod2c.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2c_vcov.RData")



# 2d: within-municipality, only 2018-2023, restricted to munis with >1 observation of positive cases
mod2d <- glm(has_case ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc_scaled + # crazy coefficient
               poverty_rate + pct_urban + cbr + imr + cdr + muni_code_6 + period,
             data = df_muni_fe_18, family = binomial(link = "logit"))

mod2d.vcov <- vcovCL(mod2d, cluster = ~ muni_code_6)

save(mod2d, file = "~/Brazil-measles/data/generated_data/models/cases/mod2d.RData")
save(mod2d.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2d_vcov.RData")




# mcfadden's pseudo R2
1 - (mod2a[["deviance"]] / mod2a[["null.deviance"]])
1 - (mod2b[["deviance"]] / mod2b[["null.deviance"]])
1 - (mod2c[["deviance"]] / mod2c[["null.deviance"]])
1 - (mod2d[["deviance"]] / mod2d[["null.deviance"]])


mod2_ors <- merge(or_tab_fun(mod2a, mod2a.vcov) %>% rename(OR1 = OR, Robust.CI1 = Robust.CI),
                  or_tab_fun(mod2b, mod2b.vcov) %>% rename(OR2 = OR, Robust.CI2 = Robust.CI),,
                  by = "Term", all = T) %>% 
  merge(or_tab_fun(mod2c, mod2c.vcov) %>% rename(OR3 = OR, Robust.CI3 = Robust.CI),
        by = "Term", all = T) %>% 
  merge(or_tab_fun(mod2d, mod2d.vcov) %>% rename(OR4 = OR, Robust.CI4 = Robust.CI),
        by = "Term", all = T) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       OR1 = as.character(nobs(mod2a)),
                       OR2 = as.character(nobs(mod2b)),
                       OR3 = as.character(nobs(mod2c)),
                       OR4 = as.character(nobs(mod2d)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       OR1 = format(round(1 - (mod2a[["deviance"]] / mod2a[["null.deviance"]]), 2), nsmall = 2),
                       OR2 = format(round(1 - (mod2b[["deviance"]] / mod2b[["null.deviance"]]), 2), nsmall = 2),
                       OR3 = format(round(1 - (mod2c[["deviance"]] / mod2c[["null.deviance"]]), 2), nsmall = 2),
                       OR4 = format(round(1 - (mod2d[["deviance"]] / mod2d[["null.deviance"]]), 2), nsmall = 2))) %>% 
  mutate(across(everything(), ~replace_na(., "")),
         Term = str_replace_all(Term, "period|year", "\\\\hspace{3mm}"),
         Robust.CI4 = paste(Robust.CI4, "\\\\")) %>% 
  arrange(match(Term, c("mcv_d1_cov_tc_lag2", "cbr", "clinics_pc_scaled", "gini", 
                        "log(gdp_pc)", "pct_urban", "poverty_rate", "travel1")))

write.table(mod2_ors, file = "", row.names = F, quote = F, sep = " & ")


################################################################################
##
## swapping in binary flag for herd immunity
##
################################################################################
# 2a: between-municipalities, all years, restricted to states with >1 observation of positive cases
mod2a_herd <- glm(has_case ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + poverty_rate + 
               pct_urban + gini + cbr + imr + cdr + travel + state + year,
             data = df_state_fe, family = binomial(link = "logit"))

mod2a_herd.vcov <- vcovCL(mod2a_herd, cluster = ~ muni_code_6)

save(mod2a_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod2a_herd.RData")
save(mod2a_herd.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2a_herd_vcov.RData")


# 2b: between-municipalities, only 2018-2023, restricted to states with >1 observation of positive cases
mod2b_herd <- glm(has_case ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + poverty_rate + 
               pct_urban + gini + cbr + imr + cdr + travel + state + year,
             data = df_state_fe_18, family = binomial(link = "logit"))

mod2b_herd.vcov <- vcovCL(mod2b_herd, cluster = ~ muni_code_6)

save(mod2b_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod2b_herd.RData")
save(mod2b_herd.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2b_herd_vcov.RData")


# 2c: within-municipality, all years, restricted to munis with >1 observation of positive cases
mod2c_herd <- glm(has_case ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + poverty_rate + 
               pct_urban + gini + cbr + imr + cdr + muni_code_6 + period,
             data = df_muni_fe, family = binomial(link = "logit"))

mod2c_herd.vcov <- vcovCL(mod2c_herd, cluster = ~ muni_code_6)

save(mod2c_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod2c_herd.RData")
save(mod2c_herd.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2c_herd_vcov.RData")



# 2d: within-municipality, only 2018-2023, restricted to munis with >1 observation of positive cases
mod2d_herd <- glm(has_case ~ herd_immunity_lag2 + log(gdp_pc) + clinics_pc_scaled + # crazy coefficient
               poverty_rate + pct_urban + cbr + imr + cdr + muni_code_6 + period,
             data = df_muni_fe_18, family = binomial(link = "logit"))

mod2d_herd.vcov <- vcovCL(mod2d_herd, cluster = ~ muni_code_6)

save(mod2d_herd, file = "~/Brazil-measles/data/generated_data/models/cases/mod2d_herd.RData")
save(mod2d_herd.vcov, file = "~/Brazil-measles/data/generated_data/models/cases/mod2d_herd_vcov.RData")


# table
mod2_herd_ors <- merge(or_tab_fun(mod2a_herd, mod2a_herd.vcov) %>% rename(OR1 = OR, Robust.CI1 = Robust.CI),
                  or_tab_fun(mod2b_herd, mod2b_herd.vcov) %>% rename(OR2 = OR, Robust.CI2 = Robust.CI),,
                  by = "Term", all = T) %>% 
  merge(or_tab_fun(mod2c_herd, mod2c_herd.vcov) %>% rename(OR3 = OR, Robust.CI3 = Robust.CI),
        by = "Term", all = T) %>% 
  merge(or_tab_fun(mod2d_herd, mod2d_herd.vcov) %>% rename(OR4 = OR, Robust.CI4 = Robust.CI),
        by = "Term", all = T) %>% 
  bind_rows(data.frame(Term = "Obs.", 
                       OR1 = as.character(nobs(mod2a_herd)),
                       OR2 = as.character(nobs(mod2b_herd)),
                       OR3 = as.character(nobs(mod2c_herd)),
                       OR4 = as.character(nobs(mod2d_herd)))) %>% 
  bind_rows(data.frame(Term = "Pseudo $R^2$", 
                       OR1 = format(round(1 - (mod2a_herd[["deviance"]] / mod2a_herd[["null.deviance"]]), 2), nsmall = 2),
                       OR2 = format(round(1 - (mod2b_herd[["deviance"]] / mod2b_herd[["null.deviance"]]), 2), nsmall = 2),
                       OR3 = format(round(1 - (mod2c_herd[["deviance"]] / mod2c_herd[["null.deviance"]]), 2), nsmall = 2),
                       OR4 = format(round(1 - (mod2d_herd[["deviance"]] / mod2d_herd[["null.deviance"]]), 2), nsmall = 2))) %>% 
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
                              travel1 = "Travel"))

write.table(mod2_herd_ors, file = "", row.names = F, quote = F, sep = " & ")
