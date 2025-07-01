library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library('sf')
library(janitor)





# SES - poverty
# https://datasus.saude.gov.br/trabalho-e-renda-censos-1991-2000-e-2010
# "Proportion (%) of the resident population with a monthly per capita household income of
# up to half the minimum wage, in a given geographic area, in the year considered.
# Per capita household income was considered to be the sum of the monthly income of the
# household's residents, in reais, divided by the number of its residents.
# The minimum wage of the last year for which the series is being calculated becomes the
# reference for the entire series. This value is adjusted for everyone based on the INPC of
# July 2010, changing the value of the poverty line and consequently the proportion of poor
# people. The reference value, the 2010 minimum wage, is R$510.00."

# half of minimum wage is very close to Brazil's poverty line of ~3.66/hr, so good estimate for % in poverty

poverty <- read.csv("~/Brazil-measles/data/raw/poverty_data.csv") %>% 
  filter(municipality != "Total") %>%
  rename(pct_low_inc_1991 = X1991,
         pct_low_inc_2000 = X2000,
         pct_low_inc_2010 = X2010) %>%
  mutate(across(c(pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010), as.numeric), # "..." is NA
         muni_code_6 = as.numeric(str_split_fixed(municipality, ' ', 2)[, 1])) %>% 
  dplyr::select(-c(Total, municipality))


# SES - education
# 2010 categories not consistent with other years
# https://datasus.saude.gov.br/educacao-censos-1991-2000-e-2010

# manual cleaning in text editor prior to reading in: removing notes from bottom and top; renaming column to "muni"

read_clean_educ <- function(file, rename_map) {
  read.delim(file, dec = ",", sep = ";", fileEncoding = "Latin1") %>%
    filter(muni != "Total") %>% 
    rename(!!!rename_map) %>%
    mutate(muni_code_6 = as.numeric(str_split_fixed(muni, " ", 2)[, 1])) %>%
    dplyr::select(-any_of(c("Não.determinada", "Alfabetização.de.adultos", "Total", "muni")))
}

# this wacky setup reduces some clutter
educ <- tibble(
  file = c("~/Brazil-measles/data/raw/educ_data/educ_1991.csv",
           "~/Brazil-measles/data/raw/educ_data/educ_2000.csv",
           "~/Brazil-measles/data/raw/educ_data/educ_2010.csv"),
  rename_map = list(c(educ_pct_below_1_yr_1991 = "Menos.de.1.ano.de.estudo", # less than one year of schooling
                      educ_pct_1_3_yrs_1991 = "X1.a.3.anos.de.estudo",       # 1 to 3 years of study
                      educ_pct_4_7_yrs_1991 = "X4.a.7.anos.de.estudo",       # 4 to 7 years of study
                      educ_pct_8_yrs_1991 = "X8.anos.e.mais.de.estudo"),     # 8 years and more of study
                    
                    c(educ_pct_below_1_yr_2000 = "Menos.de.1.ano.de.estudo", # less than one year of schooling
                      educ_pct_1_3_yrs_2000 = "X1.a.3.anos.de.estudo",       # 1 to 3 years of study
                      educ_pct_4_7_yrs_2000 = "X4.a.7.anos.de.estudo",       # 4 to 7 years of study
                      educ_pct_8_yrs_2000 = "X8.anos.e.mais.de.estudo"),     # 8 years and more of study
                    
                    c(pct_no_educ_2010 = "Sem.instrução.1º.ciclo.fundamental.incompleto",   # No education/incomplete 1st cycle of elementary school
                      pct_1_cycle_educ_2010 = "X1º.ciclo.fundamental.completo.2º.ciclo.incompleto", # 1st complete elementary cycle/2nd incomplete cycle
                      pct_complete_educ_2010 = "X2º.ciclo.fundamental.completo.ou.mais"))) %>%      # Completed 2nd cycle of basic education or more
  mutate(data = map2(file, rename_map, read_clean_educ)) %>%
  pull(data) %>%
  reduce(full_join, by = "muni_code_6")




# urban pop
# source: https://sidra.ibge.gov.br/tabela/202
# dash: absolute 0
# 0: 0 from rounding
# ...: NA
urban <- read.csv("~/Brazil-measles/data/raw/urban_pop.csv", skip = 3, nrow = 11133) %>%  # remove notes from top and bottom and total row at bottom
  dplyr::select(-Município) %>%
  rename(muni_code = Cód.,
         status = Situação.do.domicílio) %>% 
  filter(!str_detect(X1991, "Total") & 
           status == "Urbana" &
           muni_code != 2399903) %>% # Cococi is an uninhabited ghost town 
  rename(pct_urban_1991 = X1991,
         pct_urban_2000 = X2000,
         pct_urban_2010 = X2010) %>% 
  mutate(muni_code_6 = substr(muni_code, 1, 6),
         across(everything(), ~as.numeric(.))) %>%  # "..." is NA
  dplyr::select(-c(status, muni_code))


# municipality geography
# 2023 shapefile
# https://www.ibge.gov.br/en/geosciences/territorial-organization/territorial-meshes/18890-municipal-mesh.html?edicao=24069&t=downloads

geom <- read_sf('~/Brazil-measles/data/geometry/brazil_muni_sf/BR_Municipios_2024.shp') %>% 
  rename(muni_code = CD_MUN,
         muni_name = NM_MUN, 
         state_name = NM_UF,
         state_code = CD_UF,
         region = NM_REGIA) %>% 
  mutate(region = dplyr::recode(region,
                         "Norte" = "north",
                         "Nordeste" = "northeast",
                         "Centro-oeste" = "centralwest",
                         "Sudeste" = "southeast",
                         "Sul" = "south"),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6)),
         muni_code = as.numeric(muni_code),
         state_code = as.numeric(state_code)) %>% 
  filter(muni_code_6 != 430000) %>% # 4300001 and 4300002 are not in any other list, may not exactly be inhabited
  dplyr::select(muni_code, muni_code_6, muni_name, state_name, state_code, region) %>% 
  merge(poverty, by = "muni_code_6", all = TRUE) %>% # merge with all non-panel data
  merge(educ, by = "muni_code_6", all = TRUE) %>% 
  merge(urban, by = "muni_code_6", all = TRUE) %>% 
  data.frame()

# check duplicates
geom %>%
  group_by(muni_code_6) %>%
  filter(n() > 1)


save(geom, file = "~/Brazil-measles/data/SES_data.RData")


rm(poverty, educ, urban)



