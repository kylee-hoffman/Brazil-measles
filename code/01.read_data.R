library(tidyverse)
library(janitor)
library(stringr)
library(readxl)
library(openxlsx)
library("sf")

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
pop <- read_datasus(file = "~/Brazil-measles/data/pop/total_pop.csv", 
                     line_skip = 3, drop_cols = NULL, values_to = "pop_total") %>% 
  merge(read_datasus(file = "~/Brazil-measles/data/pop/pop_1-9.csv",
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to9"),
        by = c("muni_code_6", "year")) %>%
  merge(read_datasus("~/Brazil-measles/data/pop/pop_1-14.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to14"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop_1-19.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to19"), 
        by = c("muni_code_6", "year")) %>% 
  merge(read_datasus("~/Brazil-measles/data/pop/pop_1-29.csv", 
                     line_skip = 4, drop_cols = NULL, values_to = "pop_1to29"), 
        by = c("muni_code_6", "year")) %>% 
  dplyr::select(muni_code_6, year, pop_total, pop_1to9, pop_1to14, pop_1to19, pop_1to29)



################################################################################
## all deaths -> crude death rate
##
## Vital statistics -> Mortality – since 1996 according to ICD-10 -> overall
## https://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# Line: municipality; column: year of death; content: deaths by residence
################################################################################
cdr <- read_datasus('~/Brazil-measles/data/mortality/all_deaths.csv', 
                       line_skip = 3, drop_cols = "total", values_to = "all_deaths") %>% 
  merge(pop, by = c("muni_code_6", "year")) %>%
  mutate(cdr = all_deaths / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, cdr)

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

cbr <- read_datasus("~/Brazil-measles/data/births/births.csv", 
                       line_skip = 3, drop_cols = "total", values_to = "births") %>% 
  merge(pop, by = c("muni_code_6", "year")) %>%
  mutate(cbr = births / pop_total * 1000)

imr <- read_datasus('~/Brazil-measles/data/mortality/infant_deaths.csv', 
                               line_skip = 3, drop_cols = "total", values_to = "infant_deaths") %>% 
  merge(cbr, by = c("muni_code_6", "year")) %>% 
  mutate(imr = infant_deaths / births * 1000) %>% 
  dplyr::select(muni_code_6, year, imr)

imr[is.na(imr)] <- 0 # some muni-years have 0 births
imr[is.infinite(imr$imr), "imr"] <- 0 # some have 0 births and 0 infant deaths


cbr <- cbr %>% dplyr::select(muni_code_6, year, cbr)

#setdiff(pop$muni_code_6, cdr$muni_code_6) # none


################################################################################
## All measles cases -> measles incidence
##
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def
##
################################################################################
cases <- read_cases("~/Brazil-measles/data/measles_cases/total/cases_01-06.csv", 
                        4, "total") %>% 
  merge(read_cases("~/Brazil-measles/data/measles_cases/total/cases_07-25.csv", 
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
##
## NMID deaths - total and ages 1-9, 1-19, 1-29
##
## http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
##
################################################################################
nmid <- read_datasus('~/Brazil-measles/data/mortality/nmid_deaths_total.csv', 
                     line_skip = 4, drop_cols = "total", values_to = "nm_deaths_total") %>% 
  merge(pop, by = c("muni_code_6", "year")) %>% 
  mutate(nm_mx_total = nm_deaths_total / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, nm_deaths_total, nm_mx_total) %>% 
  merge(
    read_datasus('~/Brazil-measles/data/mortality/nmid_deaths_1-9.csv', 
                 line_skip = 5, drop_cols = "total", values_to = "nm_deaths_1to9") %>% 
      merge(pop, by = c("muni_code_6", "year")) %>% 
      mutate(nm_mx_1to9 = nm_deaths_1to9 / pop_1to9 * 1000) %>% 
      dplyr::select(muni_code_6, year, nm_deaths_1to9, nm_mx_1to9),
    by = c("muni_code_6", "year")) %>% 
  merge(
    read_datasus('~/Brazil-measles/data/mortality/nmid_deaths_1-14.csv', 
                 line_skip = 5, drop_cols = "total", values_to = "nm_deaths_1to14") %>% 
      merge(pop, by = c("muni_code_6", "year")) %>% 
      mutate(nm_mx_1to14 = nm_deaths_1to14 / pop_1to14 * 1000) %>% 
      dplyr::select(muni_code_6, year, nm_deaths_1to14, nm_mx_1to14),
    by = c("muni_code_6", "year")) %>% 
  merge(
    read_datasus('~/Brazil-measles/data/mortality/nmid_deaths_1-19.csv', 
                 line_skip = 5, drop_cols = "total", values_to = "nm_deaths_1to19") %>% 
      merge(pop, by = c("muni_code_6", "year")) %>% 
      mutate(nm_mx_1to19 = nm_deaths_1to19 / pop_1to19 * 1000) %>% 
      dplyr::select(muni_code_6, year, nm_deaths_1to19, nm_mx_1to19),
    by = c("muni_code_6", "year")) %>% 
  merge(
    read_datasus('~/Brazil-measles/data/mortality/nmid_deaths_1-29.csv', 
                 line_skip = 5, drop_cols = "total", values_to = "nm_deaths_1to29") %>% 
      merge(pop, by = c("muni_code_6", "year")) %>% 
      mutate(nm_mx_1to29 = nm_deaths_1to29 / pop_1to29 * 1000) %>% 
      dplyr::select(muni_code_6, year, nm_deaths_1to29, nm_mx_1to29),
    by = c("muni_code_6", "year"))

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
## coverage for 2023-2025:
## https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_
##   COBERTURA_RESIDENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA.html#
##
################################################################################

coverage_fun <- function() {
  monovalent <- read_datasus("~/Brazil-measles/data/vaccination/monovalent_cpni.csv",
                             line_skip = 0, drop_cols = "x_total", values_to = "monovalent_coverage")
  
  mmr1 <- read_datasus("~/Brazil-measles/data/vaccination/mmr1_cpni.csv",
                       line_skip = 0, drop_cols = "x_total", values_to = "MMR1_coverage")
  
  mmr_2023 <- read_coverage("~/Brazil-measles/data/vaccination/2023_vaccination.xlsx", year = "2023")
  mmr_2024 <- read_coverage("~/Brazil-measles/data/vaccination/2024_vaccination.xlsx", year = "2024")
  mmr_2025 <- read_coverage("~/Brazil-measles/data/vaccination/2025_vaccination.xlsx", year = "2025")
  
  
  coverage <- bind_rows(mmr1, mmr_2023, mmr_2024, mmr_2025) %>%
    merge(monovalent, by = c("muni_code_6", "year"), all = TRUE) %>% 
    mutate(mcv_d1_cov = ifelse(is.na(monovalent_coverage), MMR1_coverage, 
                               MMR1_coverage + monovalent_coverage),
           mcv_d1_cov_tc = pmin(mcv_d1_cov, 100)) %>% # top coded to 100
    dplyr::select(-c(MMR1_coverage, monovalent_coverage))
  
  return(coverage)
}

coverage <- coverage_fun()


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
## GINI index (2000 and 2010 only)
##
## hhttp://tabnet.datasus.gov.br/cgi/ibge/censo/cnv/ginibr.def
##
################################################################################
read_gini <- function() {
  gini <- read.delim('~/Brazil-measles/data/ginibr.csv', sep = ";", fileEncoding = "Latin1", 
                     dec = ",", skip = 2, nrows = 5565) %>% 
    clean_names() %>% 
    filter(!str_detect(municipio, "IGNORADO|Total|EXTINTO|510183")) %>% # 510183 was est. in 2025
    mutate(muni_code_6 = str_split_i(municipio, " ", 1),
           x2000 = na_if(x2000, "..."),
           x2000 = as.numeric(gsub(",", ".", x2000)))
  
  # apply 2000 values to 2000-2009
  gini00 <- gini %>% dplyr::select(muni_code_6, gini = x2000)
  
  gini00_09 <- bind_rows(lapply(2000:2009, function(yr) mutate(gini00, year = yr))) %>% 
    dplyr::select(muni_code_6, year, gini)
  
  # apply 2010 values to 2010-2024
  gini10 <- gini %>% dplyr::select(muni_code_6, gini = x2010)
  
  gini10_24 <- bind_rows(lapply(2010:2024, function(yr) mutate(gini10, year = yr))) %>% 
    dplyr::select(muni_code_6, year, gini)
  
  return(bind_rows(gini00_09, gini10_24))
}

gini <- read_gini()

################################################################################
##
## Region and state, land area
##
################################################################################
geom <- read_sf('~/Brazil-measles/data/geography/municipalities/BR_Municipios_2025.shp') %>% 
  as.data.frame() %>% 
  mutate(muni_code_6 = substring(CD_MUN, 1, 6)) %>% 
  filter(!str_detect(NM_MUN, "Área Operacional") & !str_detect(CD_MUN, "510183")) %>% # water area
  dplyr::select(state = NM_UF, region = NM_REGIAO, muni_code_6, muni = NM_MUN, land_area_km2 = AREA_KM2)



################################################################################
##
## poverty rates
##
################################################################################
read_poverty <- function() {
  # 2001 - 2009 and 2010 - 2019
  pov00_10 <- read.delim('~/Brazil-measles/data/poverty/ibge_poverty_rate_2000_2010.csv',
                         sep = ";", fileEncoding = "Latin1", dec = ",", skip = 3, nrows = 5597) %>% 
    clean_names() %>% 
    dplyr::select(-total) %>%
    filter(!str_detect(municipio, "IGNORADO|Total|EXTINTO|510183")) %>% # 510183 was est. in 2025
    mutate(across(c(x2000, x2010), ~ na_if(.x, "...")),
           across(c(x2000, x2010), ~ as.numeric(gsub(",", ".", .x))),
           muni_code_6 = str_split_i(municipio, " ", 1))
  
  # apply 2000 values to 2000-2009
  pov00 <- pov00_10 %>% dplyr::select(muni_code_6, poverty_rate = x2000)
  
  pov00_09 <- bind_rows(lapply(2000:2009, function(yr) mutate(pov00, year = yr))) %>% 
    dplyr::select(muni_code_6, year, poverty_rate)
  
  # apply 2010 values to 2010-2019
  pov10 <- pov00_10 %>% dplyr::select(muni_code_6, poverty_rate = x2010)
  
  pov10_19 <- bind_rows(lapply(2010:2019, function(yr) mutate(pov10, year = yr))) %>% 
    dplyr::select(muni_code_6, year, poverty_rate)
    
  
  # 2022 data
  pov22 <- read.xlsx('~/Brazil-measles/data/poverty/ibge_poverty_rate_2022.xlsx',
                     sheet = "Tabela", startRow = 6, rows = 6:16716, fillMergedCells = T) %>% 
    mutate(muni_code_6 = substr(X1, 1, 6),
           Total = as.numeric(gsub("-", "0", Total))) %>% 
    group_by(muni_code_6) %>%
    reframe(poverty_rate = sum(Total)) %>%
    dplyr::select(muni_code_6, poverty_rate)
    
  # apply 2022 data to 2020-2024
  pov20_24 <- bind_rows(lapply(2020:2024, function(yr) mutate(pov22, year = yr))) %>% 
    dplyr::select(muni_code_6, year, poverty_rate)
  
  return(bind_rows(pov00_09, pov10_19, pov20_24) %>% 
           mutate(year = as.character(year)))
}

poverty <- read_poverty()

################################################################################
##
## percent urban
##
################################################################################

read_urbanicity <- function() {
  # 2001 - 2009 and 2010 - 2019
  urb00_10 <- read.xlsx('~/Brazil-measles/data/urbanity/ibge_pct_urban_2000_2010.xlsx',
                        sheet = "Tabela", rows = 4:5571, fillMergedCells = T) %>% 
    clean_names() %>% 
    filter(!is.na(x1)) %>% 
    mutate(x2000 = gsub("-", "0", x2000),
           across(c(x2000, x2010), ~ as.numeric(na_if(.x, "..."))),
           muni_code_6 = substr(x1, 1, 6)) %>% 
    merge(geom %>% dplyr::select(muni_code_6), by = "muni_code_6", all.y = T) # include NA munis
  
  urb00 <- urb00_10 %>% dplyr::select(muni_code_6, pct_urban = x2000)
  
  urb01_09 <- bind_rows(lapply(2001:2009, function(yr) mutate(urb00, year = yr))) %>% 
    dplyr::select(muni_code_6, year, pct_urban)
  
  urb10 <- urb00_10 %>% dplyr::select(muni_code_6, pct_urban = x2010)
  
  urb10_19 <- bind_rows(lapply(2010:2019, function(yr) mutate(urb10, year = yr))) %>% 
    dplyr::select(muni_code_6, year, pct_urban)
  
  # 2020 - 2024
  urb22 <- read.xlsx('~/Brazil-measles/data/urbanity/ibge_pct_urban_2022.xlsx',
                     sheet = "Tabela", rows = 5:5575, fillMergedCells = T) %>% 
    mutate(muni_code_6 = substr(X1, 1, 6)) %>% 
    dplyr::select(muni_code_6, pct_urban = Urbana)
  
  urb20_24 <- bind_rows(lapply(2020:2024, function(yr) mutate(urb22, year = yr))) %>% 
    dplyr::select(muni_code_6, year, pct_urban)
  
  return(bind_rows(urb01_09, urb10_19, urb20_24) %>% 
           mutate(year = as.character(year)))
}

urban <- read_urbanicity()


################################################################################
##
## combine data
##
################################################################################
df <- merge(pop, cases, by = c("muni_code_6", "year")) %>% 
  merge(coverage, by = c("muni_code_6", "year")) %>% 
  merge(nmid, by = c("muni_code_6", "year")) %>% 
  merge(cdr, by = c("muni_code_6", "year")) %>% 
  merge(cbr, by = c("muni_code_6", "year")) %>% 
  merge(imr, by = c("muni_code_6", "year")) %>% 
  merge(poverty, by = c("muni_code_6", "year")) %>% 
  merge(urban, by = c("muni_code_6", "year")) %>% 
  merge(gdp_pc, by = c("muni_code_6", "year"), all.x = T) %>% 
  merge(gini, by = c("muni_code_6", "year"), all.x = T) %>% 
  merge(clinics, by = c("muni_code_6", "year"), all.x = T) %>% 
  left_join(geom, by = "muni_code_6") %>% 
  filter(year %in% 2001:2023)


# check that number of observations match panel structure
nrow(expand.grid(muni_code_6 = unique(pop$muni_code_6), year = 2001:2023))
# 128110

# all cities with international airports
travel_munis <- geom %>% filter(muni %in% c("Aracaju", 'Belém', 'Belo Horizonte',
                                            'Boa Vista', 'Brasília', 'Campinas',
                                            'Campo Grande', 'Cuiabá', 'Curitiba',
                                            'Florianópolis', 'Fortaleza', 'Foz do Iguaçu',
                                            'Goiânia', 'João Pessoa', 'Maceió', 'Manaus',
                                            'Natal', 'Petrolina', 'Porto Alegre', 'Porto Velho',
                                            'Recife', 'Rio Branco', 'Rio de Janeiro',
                                            'Salvador', 'São Luís', 'São Paulo',
                                            'Teresina', 'Uberlândia', 'Vitória')) %>% 
  filter(!(muni == "Belém" & state != "Pará") &
           !(muni == "Boa Vista" & state != "Roraima") &
           !(muni == "Campo Grande" & state != "Mato Grosso do Sul") &
           !(muni == "Rio Branco" & state != "Acre"))
  
# Natal airport opened in 2014
# Rio Branco airport opened in 2009
  
df <- df %>%
  mutate(travel = ifelse(muni_code_6 %in% travel_munis$muni_code_6, 1, 0),
         travel = factor(case_when(muni_code_6 == "240810" & as.numeric(as.character(year)) < 2014 ~ 0, # Natal
                            muni_code_6 == "120040" & as.numeric(as.character(year)) < 2009 ~ 0, # Rio Branco
                            .default = travel)),
         clinics_pc = clinics / pop_total * 1000,
         pop_den = pop_total / land_area_km2,
         gdp_pc = ifelse(gdp_pc == -1459.83, 1, gdp_pc), # only one muni-year with gdp_pc <= 0
         gini = gini * 100,
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
  mutate(mv_incid_total_lag1 = lag(mv_incid_total, 1, order_by = year),
         mv_incid_total_lag2 = lag(mv_incid_total, 2, order_by = year),
         mv_incid_total_lag3 = lag(mv_incid_total, 3, order_by = year),
         mv_incid_total_lag4 = lag(mv_incid_total, 4, order_by = year),
         mv_incid_total_lag5 = lag(mv_incid_total, 5, order_by = year),
         
         mcv_d1_cov_tc_lag1 = lag(mcv_d1_cov_tc, 1, order_by = year),
         mcv_d1_cov_tc_lag2 = lag(mcv_d1_cov_tc, 2, order_by = year),
         mcv_d1_cov_tc_lag3 = lag(mcv_d1_cov_tc, 3, order_by = year),
         mcv_d1_cov_tc_lag4 = lag(mcv_d1_cov_tc, 4, order_by = year),
         mcv_d1_cov_tc_lag5 = lag(mcv_d1_cov_tc, 5, order_by = year)) %>% 
  ungroup() %>% 
  dplyr::select(region, state, muni_code_6, year, nm_deaths_total, nm_mx_total, 
                nm_deaths_1to9, nm_mx_1to9, nm_deaths_1to14, nm_mx_1to14, 
                nm_deaths_1to19, nm_mx_1to19, nm_deaths_1to29, nm_mx_1to29, 
                mv_cases_total, mv_incid_total, mv_incid_total_lag1, mv_incid_total_lag2, 
                mv_incid_total_lag3, mv_incid_total_lag4, mv_incid_total_lag5,
                mcv_d1_cov, mcv_d1_cov_tc, mcv_d1_cov_tc_lag1, mcv_d1_cov_tc_lag2,
                mcv_d1_cov_tc_lag3, mcv_d1_cov_tc_lag4, mcv_d1_cov_tc_lag5, 
                cbr, cdr, imr, gdp_pc, travel, clinics_pc, pop_den, pop_total, 
                pop_1to9, pop_1to14, pop_1to19, pop_1to29, pct_urban, poverty_rate, gini)

save(df, file = '~/Brazil-measles/data/generated_data/01.muni-year_panel_01-24.RData')

rm(list=ls())





################################################################################
##
## data validation
##
################################################################################
load('~/Brazil-measles/data/generated_data/01.muni-year_panel_01-24.RData')

# find missing values
# colnames(df)[colSums(is.na(df)) > 0]
# [1] "amnesia_prev_d1"     "amnesia_prev_d2"     "amnesia_prev_d3"     "mcv_d1_cov_lag2"     "mcv_d1_cov_tc_lag2" 
# [6] "gdp_pc"              "clinics_pc"          "pct_urban"           "poverty_rate"        "literacy_rate"      
# [11] "susceptible_pop_u10"

# unique(df$muni_code_6[is.na(df$poverty_rate)])
# 150475 220095 220672 220779 240615 270375 290327 291955 320225 330285 421265 
# 422000 430003 430047 430107 430222 430223 430258 430461 430462 430465 430583 
# 430593 430613 430843 431065 431087 431123 431217 431346 431413 431417 431446 
# 431454 431531 431595 431673 431697 431861 431936 432146 432377 500390 500627 
# 510185 510325 510336 510343 510452 510454 510617 510619 510631 510757 510774
# 510776 510779 510788 510835 520485 520815 521015 521225


# municipalities that have existed since at least 2000
census00_munis <- read_xlsx('~/Brazil-measles/data/literacy/ibge_literacy_rate_2000.xlsx', 
                            skip = 5, n_max = 5507) %>% 
  clean_names() %>% 
  mutate(muni_code_6 = substr(x1, 1, 6)) %>% 
  dplyr::select(muni = x2, muni_code_6)

# only include those munis
# only include years with all data
df_00census_06_23 <- df %>% 
  filter(muni_code_6 %in% census00_munis$muni_code_6 & as.numeric(as.character(year)) %in% 2006:2023)

# colnames(df_00census_06_23)[colSums(is.na(df_00census_06_23)) > 0]
# susceptible_pop_u10
# only missing for muni 221038 2006-2009 (missing vaccination data from 1990s)

# we restrict analyses to municipalities that have existed since at least 2000.
df <- df_00census_06_23

anyNA(df)

save(df, file = '~/Brazil-measles/data/generated_data/02.analysis_data.RData')

# keep all years for descriptive plots
plot_df <- df %>% 
  filter(muni_code_6 %in% census00_munis$muni_code_6)

save(plot_df, file = '~/Brazil-measles/data/generated_data/03.plotting_data.RData')
