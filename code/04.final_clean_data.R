library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library('sf')
library(janitor)

load("~/Brazil-measles/data/health_data.RData")
load("~/Brazil-measles/data/muni_pop_96_23.RData")
load("~/Brazil-measles/data/SES_data.RData")

# merge all data
data_clean <- merge(health_data, pop, by = c("muni_code_6", "year"), all = TRUE) %>% 
  left_join(geom, by = "muni_code_6") %>% 
  filter(muni_code != 5101837) %>% # does not seem to have any inhabitants
  mutate(measles_cases_p100k = measles_cases / population * 100000,
         measles_deaths_p100k = measles_deaths / population * 100000,
         UBS_p100k = UBS / population * 100000,
         nurses_p100k = nurses / population * 100000,
         doctors_p100k = doctors / population * 100000,
         outbreak = case_when(
           year %in% 2013:2015 ~ "2013-2015",
           year %in% 2018:2021 ~ "2018-2021",
           TRUE ~ NA
         ),
         coverage = case_when(
           year < 2000 ~ monovalent_coverage,
           year %in% 2000:2003 ~ pmax(monovalent_coverage, MMR1_coverage, na.rm = T),
           year %in% 2004:2012 ~ MMR1_coverage,
           TRUE ~ MMR2_coverage
         ),
         coverage2 = case_when(
           year %in% 2000:2003 ~ monovalent_coverage + MMR1_coverage,
           TRUE ~ coverage
         ),
         goal = case_when(
           coverage2 >= 95 ~ 1,
           TRUE ~ 0
         )) %>% 
  group_by(muni_code) %>% 
  mutate(coverage_lag2 = lag(coverage2, 2, order_by = year),
         coverage_lag3 = lag(coverage2, 3, order_by = year),
         coverage_lag4 = lag(coverage2, 4, order_by = year),
         coverage_lag5 = lag(coverage2, 5, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(muni_code, year, 
                measles_cases, outbreak,
                measles_deaths, mumps_deaths, whooping_deaths, 
                measles_cases_p100k, measles_deaths_p100k,
                coverage, coverage2, goal,
                coverage_lag2, coverage_lag3, coverage_lag4, coverage_lag5,
                UBS, UBS_p100k, nurses, nurses_p100k, doctors, doctors_p100k,
                pct_urban_1991, pct_urban_2000, pct_urban_2010, 
                pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010, 
                educ_pct_8_yrs_1991, educ_pct_8_yrs_2000, pct_complete_educ_2010, 
                MMR1_coverage, MMR2_coverage, monovalent_coverage,
                population, muni_code_6, muni_name, state_name, state_code, region)


# check duplicates
data_clean %>%
  group_by(muni_code, year) %>%
  filter(n() > 1)

# check muni codes...
which(!data_clean$muni_code %in% geom$muni_code)

df <- data_clean
rm(data_clean)

save(df, file = "~/Brazil-measles/data/clean_brazil_data.RData")


