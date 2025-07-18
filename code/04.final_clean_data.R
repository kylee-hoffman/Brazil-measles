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
data_clean <- merge(health_data, pop, by = c("muni_code_6", "year"), all = TRUE) %>% 
  left_join(basics_data %>% mutate(year = as.numeric(as.character(year))),
            by = c("muni_code_6", "year")) %>% 
  left_join(geom, by = "muni_code_6") %>% 
  filter(muni_code != 5101837) %>%  # does not seem to have any inhabitants
  mutate(measles_cases_p100k = measles_cases / population * 100000,
         measles_deaths_p100k = measles_deaths / population * 100000,
         UBS_p100k = UBS / population * 100000,
         nurses_p100k = nurses / population * 100000,
         doctors_p100k = doctors / population * 100000,
         outbreak = case_when(
           year %in% 2013:2015 ~ "2013-2015",
           year %in% 2018:2021 ~ "2018-2021",
           TRUE ~ "0"),
         
         coverage = case_when(
           year < 2000 ~ monovalent_coverage,
           year %in% 2000:2003 ~ pmax(monovalent_coverage, MMR1_coverage, na.rm = T),
           year %in% 2004:2012 ~ MMR1_coverage,
           TRUE ~ MMR2_coverage),
         
         coverage2 = case_when(
           year %in% 2000:2003 ~ monovalent_coverage + MMR1_coverage,
           TRUE ~ coverage),
         
        # coverage2 = ifelse(coverage2 > 200, NA, coverage2), # some municipalities reported ~7000% coverage for MMR2
         
         goal = case_when(
           coverage2 >= 95 ~ 1,
           TRUE ~ 0)) %>% 
  group_by(muni_code) %>% 
  mutate(coverage_lag2 = dplyr::lag(coverage2, 2, order_by = year),
         coverage_lag3 = dplyr::lag(coverage2, 3, order_by = year),
         coverage_lag4 = dplyr::lag(coverage2, 4, order_by = year),
         coverage_lag5 = dplyr::lag(coverage2, 5, order_by = year),
         coverage_lag6 = dplyr::lag(coverage2, 6, order_by = year),
         
         goal_lag2 = dplyr::lag(goal, 2, order_by = year),
         goal_lag3 = dplyr::lag(goal, 3, order_by = year),
         goal_lag4 = dplyr::lag(goal, 4, order_by = year),
         goal_lag5 = dplyr::lag(goal, 5, order_by = year),
         goal_lag6 = dplyr::lag(goal, 6, order_by = year),
         
         MMR1_coverage_lag2 = dplyr::lag(MMR1_coverage, 2, order_by = year),
         MMR1_coverage_lag3 = dplyr::lag(MMR1_coverage, 3, order_by = year),
         MMR1_coverage_lag4 = dplyr::lag(MMR1_coverage, 4, order_by = year),
         MMR1_coverage_lag5 = dplyr::lag(MMR1_coverage, 5, order_by = year),
         MMR1_coverage_lag6 = dplyr::lag(MMR1_coverage, 6, order_by = year),

         MMR2_coverage_lag2 = dplyr::lag(MMR2_coverage, 2, order_by = year),
         MMR2_coverage_lag3 = dplyr::lag(MMR2_coverage, 3, order_by = year),
         MMR2_coverage_lag4 = dplyr::lag(MMR2_coverage, 4, order_by = year),
         MMR2_coverage_lag5 = dplyr::lag(MMR2_coverage, 5, order_by = year),
         MMR2_coverage_lag6 = dplyr::lag(MMR2_coverage, 6, order_by = year),
         
         tetra_coverage_lag2 = dplyr::lag(tetra_coverage, 2, order_by = year),
         tetra_coverage_lag3 = dplyr::lag(tetra_coverage, 3, order_by = year),
         tetra_coverage_lag4 = dplyr::lag(tetra_coverage, 4, order_by = year),
         tetra_coverage_lag5 = dplyr::lag(tetra_coverage, 5, order_by = year),
         tetra_coverage_lag6 = dplyr::lag(tetra_coverage, 6, order_by = year),

         birth_rate_lag2 = dplyr::lag(birth_rate, 2, order_by = year),
         birth_rate_lag3 = dplyr::lag(birth_rate, 3, order_by = year),
         birth_rate_lag4 = dplyr::lag(birth_rate, 4, order_by = year),
         birth_rate_lag5 = dplyr::lag(birth_rate, 5, order_by = year),
         birth_rate_lag6 = dplyr::lag(birth_rate, 6, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(muni_code, year, measles_cases,
                MMR1_coverage_lag2, MMR1_coverage_lag3, MMR1_coverage_lag4, MMR1_coverage_lag5, MMR1_coverage_lag6,
                MMR2_coverage_lag2, MMR2_coverage_lag3, MMR2_coverage_lag4, MMR2_coverage_lag5, MMR2_coverage_lag6,
                tetra_coverage_lag2, tetra_coverage_lag3, tetra_coverage_lag4, tetra_coverage_lag5, tetra_coverage_lag6,
                goal_lag2, goal_lag3, goal_lag4, goal_lag5, goal_lag6,
                coverage_lag2, coverage_lag3, coverage_lag4, coverage_lag5, coverage_lag6,
                measles_deaths, nonmeasles_deaths, mumps_deaths, whooping_deaths, 
                birth_rate_lag2, birth_rate_lag3, birth_rate_lag4, birth_rate_lag5, birth_rate_lag6,
                UBS_p100k, nurses_p100k, doctors_p100k,
                pct_urban_2000, pct_urban_2010, 
                pct_low_inc_2000, pct_low_inc_2010, 
                educ_pct_8_yrs_2000, pct_complete_educ_2010, 
                MMR1_coverage, MMR2_coverage, tetra_coverage, monovalent_coverage, coverage, coverage2, goal, 
                outbreak, measles_cases_p100k, measles_deaths_p100k,
                sanitation, CDR, IMR, birth_rate, GDP_PC, GINI, MHDI, MHDI_E, MHDI_L, MHDI_I,
                population, muni_code_6, muni_name, state_name, region)


# check duplicates
data_clean %>%
  group_by(muni_code, year) %>%
  filter(n() > 1)

# check muni codes...
which(!data_clean$muni_code %in% geom$muni_code)

df <- data_clean
rm(data_clean, basics_data)

save(df, file = "~/Brazil-measles/data/clean_brazil_data.RData")
