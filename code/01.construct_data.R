library(tidyverse)
library(janitor)
library(stringr)
library(readxl)
library("sf")

source("~/brazil_measles/code/utils.R")

################################################################################
##
## pop estimates
##
## Demographic and Socioeconomic -> Resident Population -> 
## Study of population estimates by municipality, sex and age - 2000-2021
## Reroute to: Resident Population - Study of Population Estimates by Municipality, Age and Sex 2000-2025
## https://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvs2024br.def
## Line: municipality; Column: year; content: resident population
##
################################################################################
pop <- read_datasus(file = "~/brazil_measles/data/pop_data/total_pop_ibge_cnv_popsvs2024br181202136_25_170_168.csv", 
                     line_skip = 3, drop_cols = NULL, values_to = "pop_total") %>% 
  merge(read_datasus(file = "~/brazil_measles/data/pop_data/pop_1-9_ibge_cnv_popsvs2024br182030136_25_170_168.csv",
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to9"),
        by = c("muni_code_6", "year")) %>%
  merge(read_datasus("~/brazil_measles/data/pop_data/pop_1-14_ibge_cnv_popsvs2024br141922136_25_170_168.csv", 
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
deaths <- read_datasus('~/brazil_measles/data/mortality/all_deaths/sim_cnv_obt10br173810136_25_170_168.csv', 
                       line_skip = 3, drop_cols = "total", values_to = "all_deaths")

cdr <- merge(deaths, pop, by = c("muni_code_6", "year")) %>%
  mutate(cdr = all_deaths / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, cdr)

rm(deaths)
#setdiff(pop$muni_code_6, cdr$muni_code_6) # none


################################################################################
## Live births -> crude birth rate and infant mortality rate
##
## Vital statistics -> Live births - since 1994 -> Live births
## http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/cnv/nvbr.def
## Line: municipality; column: year of birth; content: births by residence
################################################################################
births <- read_datasus("~/brazil_measles/data/births/sinasc_cnv_nvbr144806136_25_170_168.csv", 
                       line_skip = 3, drop_cols = "total", values_to = "births")

cbr <- merge(births, pop, by = c("muni_code_6", "year")) %>%
  mutate(cbr = births / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, cbr)


# Vital statistics -> Mortality - since 1996 according to ICD-10 -> Infant Deaths
# Line: municipality; column: year of death; content: deaths by residence
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/inf10br.def
infant_deaths <- read.delim("~/Documents/UCBSUM26/infant_deaths.csv",
                            sep = ";", dec = ",", fileEncoding = "Latin1",
                            skip = 3, nrow = 5598) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  select(-Total) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "infant_deaths") %>% 
  mutate(year = as.numeric(substring(year, 2, 5)),
         infant_deaths = as.numeric(case_when(
           infant_deaths == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ infant_deaths)),
         muni_code_6 = as.numeric(str_split_fixed(Município, ' ', 2)[, 1])) %>% 
  dplyr::select(c("muni_code_6", "year", "infant_deaths"))

imr <- merge(infant_deaths, births, by = c("muni_code_6", "year")) %>% 
  mutate(imr = infant_deaths / live_births * 1000)


rm(births)
#setdiff(pop$muni_code_6, cdr$muni_code_6) # none


################################################################################
## All measles cases -> measles incidence
##
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def
################################################################################
cases <- read_cases("~/brazil_measles/data/measles_cases/all_cases/01-06sinanwin_cnv_exantbr145511136_25_170_168.csv", 
                        4, "total") %>% 
  merge(read_cases("~/brazil_measles/data/measles_cases/all_cases/07-25sinannet_cnv_exantbr150133136_25_170_168.csv", 
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
################################################################################
cases_cured <- read_cases("~/brazil_measles/data/measles_cases/cured_cases/01-06_sinanwin_cnv_exantbr200407136_25_170_168.csv", 
                         5, "total") %>% 
  merge(read_cases("~/brazil_measles/data/measles_cases/cured_cases/07-25_sinannet_cnv_exantbr195419136_25_170_168.csv", 
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
################################################################################
nmid <- read_datasus('~/brazil_measles/data/mortality/NMID_deaths_total/sim_cnv_obt10br161759136_25_170_168.csv', 
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
################################################################################
monovalent <- read_datasus("~/brazil_measles/data/vax_coverage/monovalent_cpnibr17848356222.csv",
                           line_skip = 0, drop_cols = "x_total", values_to = "monovalent_coverage")

mmr1 <- read_datasus("~/brazil_measles/data/vax_coverage/mmr1_cpnibr17848356722.csv",
                     line_skip = 0, drop_cols = "x_total", values_to = "MMR1_coverage")

# coverage for 2023-2025
# https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_
#   COBERTURA_RESIDENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA.html#
mmr_2023 <- read_coverage("~/brazil_measles/data/vax_coverage/2023_c6a5ca6a-e536-4052-a4bc-4c62953bd6da.xlsx", year = "2023")
mmr_2024 <- read_coverage("~/brazil_measles/data/vax_coverage/2024_53d553b5-98db-4df0-ad92-841f67d33d3c.xlsx", year = "2024")
mmr_2025 <- read_coverage("/Users/kyhoff/brazil_measles/data/vax_coverage/2025_ea5a1bc1-3890-45c8-a884-4dd941ae60e1.xlsx", year = "2025")


coverage <- bind_rows(mmr1, mmr_2023, mmr_2024, mmr_2025) %>%
  merge(monovalent, by = c("muni_code_6", "year"), all = TRUE) %>% 
  mutate(mcv_d1_cov = ifelse(is.na(monovalent_coverage), MMR1_coverage, MMR1_coverage + monovalent_coverage)) %>% 
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
clinics <- read_datasus("~/brazil_measles/data/healthcare/ubs_cnes_cnv_estabbr183633136_25_170_168.csv",
                     line_skip = 4, drop_cols = NULL, values_to = "clinics")


################################################################################
##
## GDP per capita
##
## https://www.ibge.gov.br/en/statistics/economic/national-accounts/19567-gross-
##  domestic-product-of-municipalities.html?=&t=sobre
################################################################################
gdp_pc <- bind_rows(read_gdp("~/brazil_measles/data/GDP_PC/PIB dos Munic¡pios - base de dados 2002-2009.xlsx"),
                    read_gdp("~/brazil_measles/data/GDP_PC/PIB dos Munic¡pios - base de dados 2010-2023.xlsx"))

#gdp_0209 <- read_gdp("~/brazil_measles/data/GDP_PC/PIB dos Munic¡pios - base de dados 2002-2009.xlsx")
# find munis in pop data that are not in list:
#setdiff(pop$muni_code_6, gdp_0209$muni_code_6)
# munis missing gdp_pc for 2002-2012: 150475, 421265, 422000, 431454, 500627
#rm(gdp_0209)



################################################################################
##
## Region and state, land area
##
################################################################################
geom <- read_sf('~/brazil_measles/data/geography/BR_Municipios_2025/BR_Municipios_2025.shp') %>% 
  as.data.frame() %>% 
  mutate(muni_code_6 = substring(CD_MUN, 1, 6)) %>% 
  filter(!str_detect(NM_MUN, "Área Operacional") & !str_detect(CD_MUN, "510183")) %>% # water area
  dplyr::select(state = NM_UF, region = NM_REGIAO, muni_code_6, land_area_km2 = AREA_KM2)


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
  merge(gdp_pc, by = c("muni_code_6", "year"), all.x = T) %>% 
  merge(clinics, by = c("muni_code_6", "year"), all.x = T) %>% 
  left_join(geom, by = "muni_code_6") %>% 
  filter(year %in% 2002:2024)


# check that number of observations match panel structure
nrow(expand.grid(muni_code_6 = unique(pop$muni_code_6), year = 2002:2024))
# 128110

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
  mutate(mcv_d1_cov_lag2 = lag(mcv_d1_cov, 2, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(region, state, muni_code_6, year, nm_deaths, nm_mx, mv_incid_total, mv_incid_cured,
                amnesia_prev_d1, amnesia_prev_d2, amnesia_prev_d3, mv_cases_total, mv_cases_cured,
                mcv_d1_cov, mcv_d1_cov_lag2, cbr, cdr, gdp_pc, clinics_pc, pop_den, prop_1to9, pop_total)

save(df, file = '~/brazil_measles/generated_data/muni-year_panel_02-24.RData')

rm(list=ls())





################################################################################
##
## geometry for maps
##
################################################################################
load("~/brazil_measles/generated_data/muni-year_panel_02-24.RData")

geom <- read_sf('~/brazil_measles/data/geography/BR_Municipios_2025/BR_Municipios_2025.shp') %>% 
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


st_write(coverage, "~/brazil_measles/generated_data/geometry/coverage/muni_coverage.shp")


st_write(nm_mortality, "~/brazil_measles/generated_data/geometry/brazil_muni.shp")

