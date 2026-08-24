library(tidyverse)
library(janitor)
library(stringr)
library(readxl)
library("sf")
library(openxlsx)

source("~/Brazil-measles/code/utils.R")

################################################################################
##
## pop estimates
##
## Demographic and Socioeconomic -> Resident Population -> 
## Study of population estimates by municipality, sex and age - 2000-2021
## Reroute to: Resident Population - Study of Population Estimates by Muni, 
##                                   Age and Sex 2000-2025
## https://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvs2024br.def
## Line: municipality; Column: year; content: resident population
##
################################################################################
pop <- read_datasus(file = "~/Brazil-measles/data/pop/total_pop_ibge_cnv.csv", 
                     line_skip = 3, drop_cols = NULL, values_to = "pop_total") %>% 
  merge(read_datasus(file = "~/Brazil-measles/data/pop/pop_1-9_ibge_cnv.csv",
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to9"),
        by = c("muni_code_6", "year")) %>%
  merge(read_datasus("~/Brazil-measles/data/pop/pop_1-14_ibge_cnv.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to14"), 
        by = c("muni_code_6", "year")) %>% 
  mutate(prop_1to9 = pop_1to9 / pop_total) %>% 
  dplyr::select(muni_code_6, year, pop_total, prop_1to9)



################################################################################
## all deaths -> crude death rate
##
## Vital statistics -> Mortality – since 1996 according to ICD-10 -> overall
## https://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# Line: municipality; column: year of death; content: deaths by residence
################################################################################
deaths <- read_datasus('~/Brazil-measles/data/mortality/all_deaths_sim_cnv.csv', 
                       line_skip = 3, drop_cols = "total", values_to = "all_deaths")

cdr <- merge(deaths, pop, by = c("muni_code_6", "year")) %>%
  mutate(cdr = all_deaths / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, cdr)

rm(deaths)
#setdiff(pop$muni_code_6, cdr$muni_code_6) # none



################################################################################
## infant births and deaths -> crude birth rate and infant mortality rate
##
## Vital statistics -> Live births - since 1994 -> Live births
## http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/cnv/nvbr.def
## Line: municipality; column: year of birth; content: births by residence
##
## Vital statistics -> Mortality - since 1996 according to ICD-10 -> Infant Deaths
## Line: municipality; column: year of death; content: deaths by residence
## http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/inf10br.def
##
################################################################################
births <- read_datasus("~/Brazil-measles/data/births/births_sinasc_cnv.csv", 
                       line_skip = 3, drop_cols = "total", values_to = "births")

infant_deaths <- read.delim('~/Brazil-measles/data/mortality/infant_deaths_sim_cnv.csv', 
                            sep = ";", dec = ",", fileEncoding = "Latin1",
                            skip = 3, nrow = 5598) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  dplyr::select(-Total) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "infant_deaths") %>% 
  mutate(year = as.numeric(substring(year, 2, 5)),
         infant_deaths = as.numeric(case_when(
           infant_deaths == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ infant_deaths)),
         muni_code_6 = as.numeric(str_split_fixed(Município, ' ', 2)[, 1])) %>% 
  dplyr::select(c("muni_code_6", "year", "infant_deaths"))


cbr <- merge(births, pop, by = c("muni_code_6", "year")) %>%
  mutate(cbr = births / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, cbr)

imr <- merge(infant_deaths, births, by = c("muni_code_6", "year")) %>% 
  mutate(imr = infant_deaths / births * 1000)


rm(births)
#setdiff(pop$muni_code_6, cdr$muni_code_6) # none



################################################################################
## All measles cases -> measles incidence
##
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def
##
################################################################################
cases <- read_cases("~/Brazil-measles/data/measles_cases/total/cases_01-06_sinanwin_cnv.csv", 
                        4, "total") %>% 
  merge(read_cases("~/Brazil-measles/data/measles_cases/total/cases_07-25_sinannet_cnv.csv", 
                   4, c("x_1975", "x2001", "x2006", "em_branco_ign", "total")),
        by = "municipio_de_residencia") %>% 
  mutate(x2004 = 0) %>% # no cases in 2004 so no original column
  pivot_longer(cols = -"municipio_de_residencia", values_to = "mv_cases_total") %>% 
  mutate(year = as.numeric(substring(name, 2)),
         muni_code_6 = as.numeric(str_split_fixed(municipio_de_residencia, ' ', 2)[, 1])) %>%
  merge(pop %>% filter(year > 2000), by = c("muni_code_6", "year")) %>% 
  mutate(mv_incid_total = mv_cases_total / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, mv_cases_total, mv_incid_total)



################################################################################
## cured measles cases -> amnesia prevalence
##
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def
##
################################################################################
cases_cured <- read_cases("~/Brazil-measles/data/measles_cases/cured/total_01-06_sinanwin_cnv.csv", 
                         5, "total") %>% 
  merge(read_cases("~/Brazil-measles/data/measles_cases/cured/total_07-25_sinannet_cnv.csv", 
                   5, c("x2001", "x2006", "em_branco_ign", "total")),
        by = "municipio_de_residencia") %>% 
  mutate(x2004 = 0) %>% # no cases in 2004 so no original column
  pivot_longer(cols = -"municipio_de_residencia", values_to = "mv_cases_cured") %>% 
  mutate(year = as.numeric(substring(name, 2)),
         muni_code_6 = as.numeric(str_split_fixed(municipio_de_residencia, ' ', 2)[, 1])) %>% 
  merge(pop %>% filter(year > 2000), by = c("muni_code_6", "year")) %>% 
  mutate(mv_incid_cured = mv_cases_cured / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, mv_cases_cured, mv_incid_cured)


amnesia <- cases_cured %>% 
  group_by(muni_code_6) %>% 
  mutate(amnesia_d1_ct = mv_cases_cured + dplyr::lag(mv_cases_cured, 1, order_by = year),
         amnesia_d2_ct = amnesia_d1_ct + dplyr::lag(mv_cases_cured, 2, order_by = year),
         amnesia_d3_ct = amnesia_d2_ct + dplyr::lag(mv_cases_cured, 3, order_by = year)) %>% 
  ungroup() %>% 
  merge(pop %>% filter(year > 2000), by = c("muni_code_6", "year"), all = T) %>% 
  mutate(amnesia_prev_d1 = amnesia_d1_ct / pop_total * 1000,
         amnesia_prev_d2 = amnesia_d2_ct / pop_total * 1000,
         amnesia_prev_d3 = amnesia_d3_ct / pop_total * 1000) %>%
  dplyr::select(muni_code_6, year, amnesia_prev_d1, amnesia_prev_d2, amnesia_prev_d3)

#setdiff(pop$muni_code_6, cases$muni_code_6) # none
#setdiff(pop$muni_code_6, amnesia$muni_code_6) # none



################################################################################
##
## NMID deaths 
##
## http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
##
################################################################################
nmid <- read_datasus('~/Brazil-measles/data/mortality/nmid_total_sim_cnv.csv', 
                     line_skip = 4, drop_cols = "total", values_to = "nm_deaths") %>% 
  merge(pop, by = c("muni_code_6", "year")) %>% 
  mutate(nm_mx = nm_deaths / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, nm_deaths, nm_mx)

#setdiff(pop$muni_code_6, nmid$muni_code_6) # none


################################################################################
##
## measles vaccination coverage
##
## http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/cpnibr.def
## technical notes from tabnet:
## "To calculate vaccination coverage by type of disease, doses (single dose or 3rd dose)
## of the vaccines with the aforementioned components must be added together.
## Example: for vaccination coverage against measles, the doses administered (1st dose) of
## the triple viral, double viral and monovalent measles vaccines must be added together." 
##
################################################################################
monovalent <- read_datasus("~/Brazil-measles/data/vaccination/monovalent_cpni.csv",
                           line_skip = 0, drop_cols = "x_total", values_to = "monovalent_coverage")

mmr1 <- read_datasus("~/Brazil-measles/data/vaccination/mmr1_cpni.csv",
                     line_skip = 0, drop_cols = "x_total", values_to = "MMR1_coverage")

# coverage for 2023-2025
# https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_
#   COBERTURA_RESIDENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA.html#
mmr_2023 <- read_coverage("~/Brazil-measles/data/vaccination/2023_vaccination.xlsx", year = "2023")
mmr_2024 <- read_coverage("~/Brazil-measles/data/vaccination/2024_vaccination.xlsx", year = "2024")
mmr_2025 <- read_coverage("~/Brazil-measles/data/vaccination/2025_vaccination.xlsx", year = "2025")


coverage <- bind_rows(mmr1, mmr_2023, mmr_2024, mmr_2025) %>%
  merge(monovalent, by = c("muni_code_6", "year"), all = TRUE) %>% 
  mutate(mcv_d1_cov = ifelse(is.na(monovalent_coverage), MMR1_coverage, 
                             MMR1_coverage + monovalent_coverage),
         mcv_d1_cov_tc = pmin(mcv_d1_cov, 100)) %>% # top coded to 100
  dplyr::select(-c(MMR1_coverage, monovalent_coverage))



################################################################################
##
## UBS clinics
##
## http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/estabbr.def
## CNES - Establishments by Type -> Quantity per Year/month by Municipality
## Type of Establishment: HEALTH CENTER/BASIC UNIT
## selecting only January records
################################################################################
clinics <- read_datasus("~/Brazil-measles/data/healthcare/ubs_cnes_cnv.csv",
                     line_skip = 4, drop_cols = NULL, values_to = "clinics")


################################################################################
##
## GDP per capita
##
## https://www.ibge.gov.br/en/statistics/economic/national-accounts/19567-gross-
##  domestic-product-of-municipalities.html?=&t=sobre
################################################################################
gdp_pc <- bind_rows(read_gdp("~/Brazil-measles/data/GDP_PC/gdp_2002-2009.xlsx"),
                    read_gdp("~/Brazil-measles/data/GDP_PC/gdp_2010-2023.xlsx"))

#gdp_0209 <- read_gdp("~/Brazil-measles/data/GDP_PC/PIB dos Munic¡pios - base de dados 2002-2009.xlsx")
# find munis in pop data that are not in list:
#setdiff(pop$muni_code_6, gdp_0209$muni_code_6)
# munis missing gdp_pc for 2002-2012: 150475, 421265, 422000, 431454, 500627
#rm(gdp_0209)


################################################################################
##
## Region and state, land area
##
################################################################################
geom <- read_sf('~/Brazil-measles/data/geography/municipalities/BR_Municipios_2025.shp') %>% 
  as.data.frame() %>% 
  mutate(muni_code_6 = substring(CD_MUN, 1, 6)) %>% 
  filter(!str_detect(NM_MUN, "Área Operacional") & !str_detect(CD_MUN, "510183")) %>% # water area
  dplyr::select(state = NM_UF, region = NM_REGIAO, muni_code_6, land_area_km2 = AREA_KM2)


################################################################################
##
## Literacy rates
##
################################################################################

# 2001 - 2009
lit00 <- read_xlsx('~/Brazil-measles/data/literacy/ibge_literacy_rate_2000.xlsx', 
                   skip = 5, n_max = 5507) %>% 
  clean_names() %>% 
  mutate(muni_code_6 = substr(x1, 1, 6)) %>% 
  dplyr::select(muni_code_6, literacy_rate = total) %>% 
  merge(geom %>% dplyr::select(muni_code_6), by = "muni_code_6", all.y = T) # include NA munis

lit01_09 <- bind_rows(lapply(2001:2009, function(yr) mutate(lit00, year = yr))) %>% 
  dplyr::select(muni_code_6, year, literacy_rate)


# 2010 - 2019
lit10 <- read_xlsx('~/Brazil-measles/data/literacy/ibge_literacy_rate_2010.xlsx', 
                   skip = 4, n_max = 5565) %>% 
  clean_names() %>% 
  mutate(muni_code_6 = substr(x1, 1, 6)) %>% 
  dplyr::select(muni_code_6, literacy_rate = total) %>% 
  merge(geom %>% dplyr::select(muni_code_6), by = "muni_code_6", all.y = T)

lit10_19 <- bind_rows(lapply(2010:2019, function(yr) mutate(lit10, year = yr))) %>% 
  dplyr::select(muni_code_6, year, literacy_rate)


# 2020 - 2024
lit22 <- read_xlsx('~/Brazil-measles/data/literacy/ibge_literacy_rate_2022.xlsx',
                   skip = 5, n_max = 5570) %>% 
  clean_names() %>% 
  mutate(muni_code_6 = substr(x1, 1, 6)) %>% 
  dplyr::select(muni_code_6, literacy_rate = total)

lit20_24 <- bind_rows(lapply(2020:2024, function(yr) mutate(lit22, year = yr))) %>% 
  dplyr::select(muni_code_6, year, literacy_rate)

lit <- bind_rows(lit01_09, lit10_19, lit20_24) %>% 
  mutate(year = as.character(year))

rm(lit00, lit01_09, lit10, lit10_19, lit22, lit20_24)

#is.na(lit$literacy_rate) %>% table() # 617 NA



################################################################################
##
## poverty rates
##
################################################################################

# 2001 - 2009 and 2010 - 2019
pov00_10 <- read.delim('~/Brazil-measles/data/poverty/ibge_poverty_rate_2000_2010.csv',
                       sep = ";", fileEncoding = "Latin1", dec = ",", skip = 3, nrows = 5597) %>% 
  clean_names() %>% 
  dplyr::select(-total) %>%
  filter(!str_detect(municipio, "IGNORADO|Total|EXTINTO|510183")) %>% # 510183 was est. in 2025
  mutate(across(c(x2000, x2010), ~ na_if(.x, "...")),
         across(c(x2000, x2010), ~ as.numeric(gsub(",", ".", .x))),
         muni_code_6 = str_split_i(municipio, " ", 1))

pov00 <- pov00_10 %>% dplyr::select(muni_code_6, poverty_rate = x2000)

pov01_09 <- bind_rows(lapply(2001:2009, function(yr) mutate(pov00, year = yr))) %>% 
  dplyr::select(muni_code_6, year, poverty_rate)

pov10 <- pov00_10 %>% dplyr::select(muni_code_6, poverty_rate = x2010)

pov10_19 <- bind_rows(lapply(2010:2019, function(yr) mutate(pov10, year = yr))) %>% 
  dplyr::select(muni_code_6, year, poverty_rate)
  
# 2020 - 2024
pov22 <- read.xlsx('~/Brazil-measles/data/poverty/ibge_poverty_rate_2022.xlsx',
                   sheet = "Tabela", startRow = 6, rows = 6:16716, fillMergedCells = T) %>% 
  mutate(Total = as.numeric(recode(Total, "-" = "0"))) %>% 
  group_by(X1) %>% 
  reframe(muni_code_6 = substr(unique(X1), 1, 6),
          poverty_rate = sum(Total)) %>% 
  dplyr::select(muni_code_6, poverty_rate)
  
pov20_24 <- bind_rows(lapply(2020:2024, function(yr) mutate(pov22, year = yr))) %>% 
  dplyr::select(muni_code_6, year, poverty_rate)

pov <- bind_rows(pov01_09, pov10_19, pov20_24) %>% 
  mutate(year = as.character(year))

rm(pov00_10, pov01_09, pov00, pov10, pov10_19, pov22, pov20_24)

################################################################################
##
## percent urban
##
################################################################################


# 2001 - 2009 and 2010 - 2019
urb00_10 <- read.xlsx('~/Brazil-measles/data/urbanity/ibge_pct_urban_2000_2010.xlsx',
                      sheet = "Tabela", rows = 4:5571, fillMergedCells = T) %>% 
  filter(!is.na(X1)) %>% 
  mutate(`2000` = recode(`2000`, "-" = "0"),
         across(c(`2000`, `2010`), ~ as.numeric(na_if(.x, "..."))),
         muni_code_6 = substr(X1, 1, 6)) %>% 
  merge(geom %>% dplyr::select(muni_code_6), by = "muni_code_6", all.y = T) # include NA munis

urb00 <- urb00_10 %>% dplyr::select(muni_code_6, pct_urban = `2000`)

urb01_09 <- bind_rows(lapply(2001:2009, function(yr) mutate(urb00, year = yr))) %>% 
  dplyr::select(muni_code_6, year, pct_urban)

urb10 <- urb00_10 %>% dplyr::select(muni_code_6, pct_urban = `2010`)

urb10_19 <- bind_rows(lapply(2010:2019, function(yr) mutate(urb10, year = yr))) %>% 
  dplyr::select(muni_code_6, year, pct_urban)

# 2020 - 2024
urb22 <- read.xlsx('~/Brazil-measles/data/urbanity/ibge_pct_urban_2022.xlsx',
                   sheet = "Tabela", rows = 5:5575, fillMergedCells = T) %>% 
  mutate(muni_code_6 = substr(X1, 1, 6)) %>% 
  dplyr::select(muni_code_6, pct_urban = Urbana)

urb20_24 <- bind_rows(lapply(2020:2024, function(yr) mutate(urb22, year = yr))) %>% 
  dplyr::select(muni_code_6, year, pct_urban)

urb <- bind_rows(urb01_09, urb10_19, urb20_24) %>% 
  mutate(year = as.character(year))

rm(urb00_10, urb01_09, urb00, urb10, urb10_19, urb22, urb20_24)


################################################################################
##
## combine data
##
################################################################################
df <- merge(pop, cases, by = c("muni_code_6", "year")) %>% 
  merge(cases_cured, by = c("muni_code_6", "year")) %>% 
  merge(coverage, by = c("muni_code_6", "year")) %>% 
  merge(nmid, by = c("muni_code_6", "year")) %>% 
  merge(amnesia, by = c("muni_code_6", "year")) %>% 
  merge(cdr, by = c("muni_code_6", "year")) %>% 
  merge(cbr, by = c("muni_code_6", "year")) %>% 
  merge(lit, by = c("muni_code_6", "year")) %>% 
  merge(pov, by = c("muni_code_6", "year")) %>% 
  merge(urb, by = c("muni_code_6", "year")) %>% 
  merge(gdp_pc, by = c("muni_code_6", "year"), all.x = T) %>% 
  merge(clinics, by = c("muni_code_6", "year"), all.x = T) %>% 
  left_join(geom, by = "muni_code_6") %>% 
  filter(year %in% 2001:2024)


# check that number of observations match panel structure
nrow(expand.grid(muni_code_6 = unique(pop$muni_code_6), year = 2001:2024))
# 133680

df <- df %>% 
  mutate(clinics_pc = clinics / pop_total * 1000,
         pop_den = pop_total / land_area_km2,
         gdp_pc = ifelse(gdp_pc == -1459.83, 1, gdp_pc), # only one muni-year with gdp_pc <= 0
         region = factor(region),
         muni_code_6 = factor(muni_code_6),
         year = factor(year),
         state = factor(state, # ordered by pop density
                        levels = c('Roraima', 'Amazonas', 'Amapá', 'Mato Grosso', 'Tocantins', 'Acre', 'Pará', 
                                   'Rondônia', 'Mato Grosso do Sul', 'Piauí', 'Goiás', 'Maranhão',  'Bahia', 
                                   'Minas Gerais', 'Rio Grande do Sul', 'Paraná', 'Ceará', 'Rio Grande do Norte', 
                                   'Santa Catarina', 'Paraíba', 'Espírito Santo',  'Sergipe', 'Pernambuco',  
                                   'Alagoas', 'São Paulo', 'Rio de Janeiro', 'Distrito Federal'))) %>% 
  group_by(muni_code_6) %>% 
  mutate(mcv_d1_cov_lag1 = lag(mcv_d1_cov, 1, order_by = year),
         mcv_d1_cov_lag2 = lag(mcv_d1_cov, 2, order_by = year),
         mcv_d1_cov_lag3 = lag(mcv_d1_cov, 3, order_by = year),
         mcv_d1_cov_lag4 = lag(mcv_d1_cov, 4, order_by = year),
         mcv_d1_cov_lag5 = lag(mcv_d1_cov, 5, order_by = year),
         mcv_d1_cov_tc_lag1 = lag(mcv_d1_cov_tc, 1, order_by = year),
         mcv_d1_cov_tc_lag2 = lag(mcv_d1_cov_tc, 2, order_by = year),
         mcv_d1_cov_tc_lag3 = lag(mcv_d1_cov_tc, 3, order_by = year),
         mcv_d1_cov_tc_lag4 = lag(mcv_d1_cov_tc, 4, order_by = year),
         mcv_d1_cov_tc_lag5 = lag(mcv_d1_cov_tc, 5, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(region, state, muni_code_6, year, nm_deaths, nm_mx, mv_incid_total, mv_incid_cured,
                amnesia_prev_d1, amnesia_prev_d2, amnesia_prev_d3, mv_cases_total, mv_cases_cured,
                mcv_d1_cov, mcv_d1_cov_lag1, mcv_d1_cov_lag2, mcv_d1_cov_lag3, mcv_d1_cov_lag4, mcv_d1_cov_lag5, 
                mcv_d1_cov_tc, mcv_d1_cov_tc_lag1, mcv_d1_cov_tc_lag2, mcv_d1_cov_tc_lag3, mcv_d1_cov_tc_lag4, 
                mcv_d1_cov_tc_lag5, cbr, cdr, gdp_pc, 
                clinics_pc, pop_den, prop_1to9, pop_total, pct_urban, poverty_rate, literacy_rate)

save(df, file = '~/Brazil-measles/data/muni-year_panel_01-24.RData')

rm(list=ls())





################################################################################
##
## geometry for maps
##
################################################################################
load("~/Brazil-measles/data/muni-year_panel_01-24.RData")

geom <- read_sf('~/Brazil-measles/data/geography/municipalities/BR_Municipios_2025.shp') %>% 
  mutate(muni_code_6 = substring(CD_MUN, 1, 6)) %>% 
  filter(!str_detect(NM_MUN, "Área Operacional")) %>% # water area
  dplyr::select(muni_code_6, geometry)



measles_cumulative <- df %>% 
  filter(year %in% 2018:2021) %>% 
  group_by(muni_code_6) %>% 
  summarise(cases = sum(mv_cases_total)) %>% 
  merge(df %>% filter(year == 2021) %>% dplyr::select(muni_code_6, pop_total), by = "muni_code_6") %>% 
  mutate(incid = cases / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, incid)


nm_mortality <- df %>% 
  dplyr::select(muni_code_6, year, nm_mx) %>% 
  filter(year %in% 2018:2024) %>% 
  pivot_wider(names_from = year,
              values_from = nm_mx,
              names_prefix = "mort") %>% 
  merge(measles_cumulative, by = "muni_code_6") %>% 
  right_join(geom, by = "muni_code_6")


coverage <- df %>% 
  dplyr::select(muni_code_6, year, mcv_d1_cov) %>% 
  pivot_wider(names_from = year,
              values_from = mcv_d1_cov,
              names_prefix = "X") %>% 
  right_join(geom, by = "muni_code_6")


st_write(coverage, "~/Brazil-measles/generated_data/geometry/coverage/muni_coverage.shp")


st_write(nm_mortality, "~/Brazil-measles/generated_data/geometry/brazil_muni.shp")

