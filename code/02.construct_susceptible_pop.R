library(tidyverse)
library(zoo)

source("~/Brazil-measles/code/utils.R")

load('~/Brazil-measles/data/muni-year_panel_01-24.RData')

# coverage from 1994-2000
monovalent <- read_datasus("~/Brazil-measles/data/vaccination/monovalent_94-00cpni.csv",
                           line_skip = 0, drop_cols = "x_total", values_to = "monovalent_coverage")

mmr1 <- read_datasus("~/Brazil-measles/data/vaccination/mmr1_99-00cpni.csv",
                     line_skip = 0, drop_cols = "x_total", values_to = "MMR1_coverage")

cov94_00 <- merge(monovalent, mmr1, by = c("muni_code_6", "year"), all = TRUE) %>% 
  mutate(mcv_d1_cov = ifelse(is.na(MMR1_coverage), monovalent_coverage, 
                             MMR1_coverage + monovalent_coverage),
         mcv_d1_cov_tc = pmin(mcv_d1_cov, 100)) %>% # top coded to 100
  dplyr::select(-c(MMR1_coverage, monovalent_coverage, mcv_d1_cov))

rm(monovalent, mmr1)


# lag coverage 1-9 years
cov_lags <- df %>%
  dplyr::select(muni_code_6, year, mcv_d1_cov_tc) %>% 
  merge(cov94_00, by = c("muni_code_6", "year", "mcv_d1_cov_tc"), all = T) %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  group_by(muni_code_6) %>% 
  mutate(mcv_d1_cov_tc_lag1 = lag(mcv_d1_cov_tc, 1, order_by = year),
         mcv_d1_cov_tc_lag2 = lag(mcv_d1_cov_tc, 2, order_by = year),
         mcv_d1_cov_tc_lag3 = lag(mcv_d1_cov_tc, 3, order_by = year),
         mcv_d1_cov_tc_lag4 = lag(mcv_d1_cov_tc, 4, order_by = year),
         mcv_d1_cov_tc_lag5 = lag(mcv_d1_cov_tc, 5, order_by = year),
         mcv_d1_cov_tc_lag6 = lag(mcv_d1_cov_tc, 6, order_by = year),
         mcv_d1_cov_tc_lag7 = lag(mcv_d1_cov_tc, 7, order_by = year),
         mcv_d1_cov_tc_lag8 = lag(mcv_d1_cov_tc, 8, order_by = year),
         mcv_d1_cov_tc_lag9 = lag(mcv_d1_cov_tc, 9, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(muni_code_6, year, contains("mcv_d1_cov_tc_lag"))


# pop sizes ages 1-9
pop <- read_datasus(file = "~/Brazil-measles/data/pop/pop1_ibge_cnv.csv",
                    line_skip = 4, drop_cols = NULL, values_to = "pop1") %>% 
  merge(read_datasus(file = "~/Brazil-measles/data/pop/pop2_ibge_cnv.csv",
                     line_skip = 4, drop_cols = NULL, values_to = "pop2"),
        by = c("muni_code_6", "year")) %>%
  merge(read_datasus("~/Brazil-measles/data/pop/pop3_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop3"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop4_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop4"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop5_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop5"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop6_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop6"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop7_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop7"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop8_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop8"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop9_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop9"), 
        by = c("muni_code_6", "year")) %>% 
  mutate(year = as.numeric(year))


births <- read_datasus("~/Brazil-measles/data/births/births_sinasc_cnv.csv", 
                       line_skip = 3, drop_cols = "total", values_to = "births") %>% 
  mutate(year = as.numeric(year))


# cured cases in ages 0-9
cured_cases_u10 <- read_cases("~/Brazil-measles/data/measles_cases/cured/ages0-9_01-06_sinanwin_cnv.csv", 
                          6, "total") %>% 
  merge(read_cases("~/Brazil-measles/data/measles_cases/cured/ages0-9_07-25_sinannet_cnv.csv", 
                   6, c("x2001", "x2006", "em_branco_ign", "total")),
        by = "municipio_de_residencia") %>% 
  mutate(x2004 = 0) %>% # no cases in 2004 so no original column
  pivot_longer(cols = -"municipio_de_residencia", values_to = "cured_cases_u10") %>% 
  mutate(year = as.numeric(substring(name, 2)),
         muni_code_6 = str_split_fixed(municipio_de_residencia, ' ', 2)[, 1]) %>% 
  dplyr::select(muni_code_6, year, cured_cases_u10)



# deaths before 6 months
deaths_u6mo <- read.delim('~/Brazil-measles/data/mortality/u6mo_sim_cnv.csv', 
           sep = ";", dec = ",", fileEncoding = "Latin1",
           skip = 4, nrow = 5598) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  dplyr::select(-Total) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "deaths") %>% 
  mutate(year = as.numeric(substring(year, 2, 5)),
         deaths_u6mo = as.numeric(case_when(deaths == "-" ~ "0",
                                            TRUE ~ deaths)),
         muni_code_6 = str_split_fixed(Município, ' ', 2)[, 1]) %>% 
  dplyr::select(c(muni_code_6, year, deaths_u6mo))


susc_pop <- merge(cov_lags, pop, by = c("muni_code_6", "year")) %>% 
  merge(cured_cases_u10, by = c("muni_code_6", "year")) %>% 
  merge(deaths_u6mo, by = c("muni_code_6", "year")) %>% 
  merge(births, by = c("muni_code_6", "year")) %>% 
  group_by(muni_code_6) %>%
  arrange(year) %>%
  mutate(cumul_prev_cases_u10 = lag(rollapply(cured_cases_u10, # sum cured cases
                                              width = 10,       # in ages 0-9 from 
                                              FUN = sum,        # previous 10 yrs
                                              align = "right", 
                                              partial = TRUE), 
                                    default = 0)) %>% 
  ungroup()

# (births - deahts u6mo) + unvaccinated 1 yos + unvaccinated 2 yos + ... +
# unvaccinated 9 yos - cumulative previous cases in the u10 pop
susc_pop <- susc_pop %>% 
  mutate(susceptible_pop_u10 = pmax(0, (births - deaths_u6mo) + 
                                      pop1 * (100 - mcv_d1_cov_tc_lag1) / 100 +
                                      pop2 * (100 - mcv_d1_cov_tc_lag2) / 100 +
                                      pop3 * (100 - mcv_d1_cov_tc_lag3) / 100 +
                                      pop4 * (100 - mcv_d1_cov_tc_lag4) / 100 +
                                      pop5 * (100 - mcv_d1_cov_tc_lag5) / 100 +
                                      pop6 * (100 - mcv_d1_cov_tc_lag6) / 100 +
                                      pop7 * (100 - mcv_d1_cov_tc_lag7) / 100 +
                                      pop8 * (100 - mcv_d1_cov_tc_lag8) / 100 +
                                      pop9 * (100 - mcv_d1_cov_tc_lag9) / 100 -
                                      cumul_prev_cases_u10)) %>% 
  dplyr::select(muni_code_6, year, susceptible_pop_u10) %>% 
  mutate(year = factor(year))

df <- df %>% 
  merge(susc_pop, by = c("muni_code_6", "year"))


save(df, file = '~/Brazil-measles/data/muni-year_panel_01-24.RData')
