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
  mutate(UBS_p100k = UBS / population * 100000,
         nurses_p100k = nurses / population * 100000,
         doctors_p100k = doctors / population * 100000) %>% 
  dplyr::select(muni_code, year, 
                MMR1_coverage, MMR2_coverage, monovalent_coverage,
                measles_cases, 
                measles_deaths, mumps_deaths, whooping_deaths, 
                UBS, UBS_p100k, nurses, nurses_p100k, doctors, doctors_p100k,
                pct_urban_1991, pct_urban_2000, pct_urban_2010, 
                pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010, 
                educ_pct_8_yrs_1991, educ_pct_8_yrs_2000, pct_complete_educ_2010, 
                population, muni_code_6, muni_name, state_name, state_code, region)


# check duplicates
data_clean %>%
  group_by(muni_code, year) %>%
  filter(n() > 1)

# check muni codes...
which(!data_clean$muni_code %in% geom$muni_code)


save(data_clean, file = "~/Brazil-measles/data/clean_brazil_data.RData")


  
#### shapefile for maps

wide_sf <- data_clean %>% 
  rename(cases = measles_cases,
         deaths = measles_deaths,
         covg = MMR2_coverage,
         case_nm = measles_cases_p100000) %>% 
  select(muni_code, year, covg, case_nm) %>% 
  pivot_wider(names_from = year,
              values_from = c(covg, case_nm),
              names_sep = "") %>% 
  select(-c(covg1996, covg1997, covg1998, covg1999)) %>% 
  merge(geom, by = "muni_code") %>% 
  rename(pov1991 = pct_low_inc_1991,
         pov2000 = pct_low_inc_2000,
         pov2010 = pct_low_inc_2010,
         educ1991 = educ_pct_8_yrs_1991,
         educ2000 = educ_pct_8_yrs_2000,
         educ2010 = pct_complete_educ_2010)

st_write(wide_sf, "~/Brazil-measles/data/geom/geom_clean.shp")
