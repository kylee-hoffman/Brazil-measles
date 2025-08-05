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

# additional SES data from https://pmc.ncbi.nlm.nih.gov/articles/PMC9092813/
load("~/Brazil-measles/data/basics_master_data.RData") 

basics_data <- basics_data %>% 
  filter(SCOPE == 7) %>% 
  rename(muni_code_6 = LOCAL_CODE,
         year = YEAR,
         sanitation = SANITATION,
         CDR = CMR) %>%
  mutate(year = factor(year),
         birth_rate = BIRTH_RATE * 100) %>%  # rate originally per 1,000
  dplyr::select(muni_code_6, year, IMR, CDR, sanitation, GDP_PC, GINI, 
              MHDI, MHDI_E, MHDI_L, MHDI_I, birth_rate)

# merge all data
df <- merge(health_data, pop, by = c("muni_code_6", "year"), all = TRUE) %>% 
  left_join(basics_data %>% mutate(year = as.numeric(as.character(year))),
            by = c("muni_code_6", "year")) %>% 
  left_join(geom, by = "muni_code_6") %>% 
  filter(muni_code != 5101837) %>%  # does not seem to have any inhabitants
  mutate(pop_density = population / AREA_KM2,
         measles_cases_p100k = measles_cases / population * 100000,
         measles_deaths_p100k = measles_deaths / population * 100000,
         UBS_p100k = UBS / population * 100000,
         nurses_p100k = nurses / population * 100000,
         doctors_p100k = doctors / population * 100000,
         outbreak = case_when(
           year %in% 2013:2015 ~ "2013-2015",
           year %in% 2018:2021 ~ "2018-2021",
           TRUE ~ "0"),
         
         coverage_dose1 = ifelse(is.na(monovalent_coverage) & is.na(MMR1_coverage), NA_real_, 
                                 rowSums(cbind(monovalent_coverage, MMR1_coverage), na.rm = TRUE)),
         
         coverage_dose2 = ifelse(is.na(MMR2_coverage) & is.na(tetra_coverage), NA_real_, 
                                 rowSums(cbind(MMR2_coverage, tetra_coverage), na.rm = TRUE)),
         
         goal = ifelse(coverage_dose2 >= 95, 1, 0)) %>% 
  group_by(muni_code) %>% 
  mutate(coverage_dose1_lag1 = dplyr::lag(coverage_dose1, 1, order_by = year),
         coverage_dose1_lag2 = dplyr::lag(coverage_dose1, 2, order_by = year),
         coverage_dose1_lag3 = dplyr::lag(coverage_dose1, 3, order_by = year),
         coverage_dose1_lag4 = dplyr::lag(coverage_dose1, 4, order_by = year),
         coverage_dose1_lag5 = dplyr::lag(coverage_dose1, 5, order_by = year),
         coverage_dose1_lag6 = dplyr::lag(coverage_dose1, 6, order_by = year),
         coverage_dose1_lag7 = dplyr::lag(coverage_dose1, 7, order_by = year),
         coverage_dose1_lag8 = dplyr::lag(coverage_dose1, 8, order_by = year),
         coverage_dose1_lag9 = dplyr::lag(coverage_dose1, 9, order_by = year),
         
         coverage_dose2_lag1 = dplyr::lag(coverage_dose2, 1, order_by = year),
         coverage_dose2_lag2 = dplyr::lag(coverage_dose2, 2, order_by = year),
         coverage_dose2_lag3 = dplyr::lag(coverage_dose2, 3, order_by = year),
         coverage_dose2_lag4 = dplyr::lag(coverage_dose2, 4, order_by = year),
         coverage_dose2_lag5 = dplyr::lag(coverage_dose2, 5, order_by = year),
         coverage_dose2_lag6 = dplyr::lag(coverage_dose2, 6, order_by = year),
         coverage_dose2_lag7 = dplyr::lag(coverage_dose2, 7, order_by = year),
         coverage_dose2_lag8 = dplyr::lag(coverage_dose2, 8, order_by = year),
         coverage_dose2_lag9 = dplyr::lag(coverage_dose2, 9, order_by = year),
         
         goal_lag1 = dplyr::lag(goal, 1, order_by = year),
         goal_lag2 = dplyr::lag(goal, 2, order_by = year),
         goal_lag3 = dplyr::lag(goal, 3, order_by = year),
         goal_lag4 = dplyr::lag(goal, 4, order_by = year),
         goal_lag5 = dplyr::lag(goal, 5, order_by = year),
         goal_lag6 = dplyr::lag(goal, 6, order_by = year),
         goal_lag7 = dplyr::lag(goal, 7, order_by = year),
         goal_lag8 = dplyr::lag(goal, 8, order_by = year),
         goal_lag9 = dplyr::lag(goal, 9, order_by = year),
         
         birth_rate_lag1 = dplyr::lag(birth_rate, 1, order_by = year),
         birth_rate_lag2 = dplyr::lag(birth_rate, 2, order_by = year),
         birth_rate_lag3 = dplyr::lag(birth_rate, 3, order_by = year),
         birth_rate_lag4 = dplyr::lag(birth_rate, 4, order_by = year),
         birth_rate_lag5 = dplyr::lag(birth_rate, 5, order_by = year),
         birth_rate_lag6 = dplyr::lag(birth_rate, 6, order_by = year),
         birth_rate_lag7 = dplyr::lag(birth_rate, 7, order_by = year),
         birth_rate_lag8 = dplyr::lag(birth_rate, 8, order_by = year),
         birth_rate_lag9 = dplyr::lag(birth_rate, 9, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(muni_code, year, measles_cases,
                coverage_dose1, coverage_dose2,
                measles_deaths, nonmeasles_deaths, mumps_deaths, whooping_deaths, 
                coverage_dose1_lag1, coverage_dose1_lag2, coverage_dose1_lag3, coverage_dose1_lag4, coverage_dose1_lag5, 
                coverage_dose1_lag6, coverage_dose1_lag7, coverage_dose1_lag8, coverage_dose1_lag9,
                coverage_dose2_lag1, coverage_dose2_lag2, coverage_dose2_lag3, coverage_dose2_lag4, coverage_dose2_lag5, 
                coverage_dose2_lag6, coverage_dose2_lag7, coverage_dose2_lag8, coverage_dose2_lag9,
                birth_rate_lag1, birth_rate_lag2, birth_rate_lag3, birth_rate_lag4, birth_rate_lag5, 
                birth_rate_lag6, birth_rate_lag7, birth_rate_lag8, birth_rate_lag9,
                goal_lag1, goal_lag2, goal_lag3, goal_lag4, goal_lag5, goal_lag6, goal_lag7, goal_lag8, goal_lag9,
                UBS_p100k, nurses_p100k, doctors_p100k,
                pop_density,pct_urban_2000, pct_urban_2010, 
                pct_low_inc_2000, pct_low_inc_2010, educ_pct_8_yrs_2000, pct_complete_educ_2010, 
                MMR1_coverage, MMR2_coverage, tetra_coverage, monovalent_coverage, goal, 
                outbreak, measles_cases_p100k, measles_deaths_p100k,
                sanitation, CDR, IMR, birth_rate, GDP_PC, GINI, MHDI, MHDI_E, MHDI_L, MHDI_I,
                population, muni_code_6, muni_name, state_name, region)


# check duplicates
df %>%
  group_by(muni_code, year) %>%
  filter(n() > 1)

# check muni codes...
which(!df$muni_code %in% geom$muni_code)

rm(basics_data)

save(df, file = "~/Brazil-measles/data/clean_brazil_data.RData")
