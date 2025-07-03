library(tidyr)
library(dplyr)


load("~/Brazil-measles/data/clean_brazil_data.RData")
load("~/Brazil-measles/data/SES_data.RData")

muni_data <- geom %>% 
  dplyr::select(-geometry)

rm(geom)

geom <- read_sf('~/Brazil-measles/data/geometry/brazil_muni_sf/BR_Municipios_2024.shp') %>% 
  rename(muni_code = CD_MUN) %>% 
  dplyr::select(muni_code, geometry)

geom$muni_code <- as.numeric(geom$muni_code)


#### shapefile for maps

wide_incidence <- df %>% 
  mutate(incid = measles_cases / population * 100000) %>% 
  dplyr::select(muni_code, year, incid) %>% 
  pivot_wider(names_from = year,
              values_from = incid,
              names_prefix = "X") %>% 
  dplyr::select(-c(X1996, X1997, X1998, X1999, X2000)) %>% 
  merge(muni_data, by = "muni_code") %>% 
  right_join(geom, by = "muni_code") %>% 
  rename(pov1991 = pct_low_inc_1991,
         pov2000 = pct_low_inc_2000,
         pov2010 = pct_low_inc_2010,
         muni_nm = muni_name) %>% 
  dplyr::select(-c(educ_pct_below_1_yr_1991, educ_pct_1_3_yrs_1991, educ_pct_4_7_yrs_1991, educ_pct_8_yrs_1991,
                   educ_pct_below_1_yr_2000, educ_pct_1_3_yrs_2000, educ_pct_4_7_yrs_2000, educ_pct_8_yrs_2000,
                   pct_no_educ_2010, pct_1_cycle_educ_2010, pct_complete_educ_2010,
                   pct_urban_1991, pct_urban_2000, pct_urban_2010, state_code, state_name, muni_code_6))



wide_mortality <- df %>% 
  mutate(mort = measles_deaths / population * 100000) %>% 
  dplyr::select(muni_code, year, mort) %>% 
  pivot_wider(names_from = year,
              values_from = mort,
              names_prefix = "X") %>%
  dplyr::select(-c(X1996, X1997, X1998, X1999, X2000)) %>% 
  right_join(geom, by = "muni_code")



wide_coverage <- df %>% 
  dplyr::select(muni_code, year, coverage2) %>% 
  pivot_wider(names_from = year,
              values_from = coverage2,
              names_prefix = "X") %>%
  right_join(geom, by = "muni_code")



st_write(wide_incidence, "~/Brazil-measles/data/geometry/incidence/incidence.shp")
st_write(wide_mortality, "~/Brazil-measles/data/geometry/mortality/mortality.shp")
st_write(wide_coverage, "~/Brazil-measles/data/geometry/coverage/coverage.shp")

rm(wide_coverage, wide_incidence, wide_mortality, muni_data)

# aggregate outbreak cases
outbreak_13_15 <- df %>% 
  filter(outbreak == "2013-2015") %>% 
  group_by(muni_code) %>% 
  summarise(ob_13_15 = sum(measles_cases) / population[year == 2014][1] * 100000)


outbreak_18_21 <- df %>% 
  filter(outbreak == "2018-2021") %>% 
  group_by(muni_code) %>% 
  summarise(ob_18_21 = sum(measles_cases) / population[year == 2020][1] * 100000)

outbreak_cases_p100k <- merge(outbreak_13_15, outbreak_18_21, by = "muni_code") %>% 
  right_join(geom, by = "muni_code")


st_write(outbreak_cases_p100k, "~/Brazil-measles/data/geometry/outbreak/outbreak_cases_p100k.shp")
