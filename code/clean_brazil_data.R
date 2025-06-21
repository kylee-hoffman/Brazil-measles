library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(datazoom.amazonia)
library(purrr)
library(tibble)

# measles cases according to municipality of residence by year
# "confirmed cases by Year 1st Symptom(s) according to Municipality of residence"

# sources:
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def (2001-2006)
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def (2007-2024)
# data selection: 
#     row = municipality, column = year of first symptoms, content = cases by residence
#     make sure to select display rows with 0 data

# notes from website:
# "The last confirmed case of Rubella in Brazil occurred in 2008
# "- In 2015 - 214 cases were confirmed, being: CE (211), SP (02) and RR (01); 
# - In 2016 and 2017, Brazil did not confirm any cases of measles; 
# - In 2018 - 9,325 cases were confirmed: AM (8,791), RR (361), PA (83), RS (47), RJ (20), PE (4), SE (4), BA (3), SP (9), RO (2) and DF (1); the genotypes identified were D8 and 1B3; 
# - In 2019 - 20,901 cases were confirmed, being: SP (17,816), PR (1,071), RJ (463), PA (405), PE (344), SC (297), MG (143), RS (100), BA (80), PB (66), AL (35), CE (19), GO (12), DF (11), RN (9), MA (8), SE (6), AM (4), ES (4), PI (3), MS (2), AP (2) and RR (1). The genotype identified was D8. 
# In 2020 - 8,100 cases were confirmed, being: PA (4,906), RJ (1,358), SP (879), PR (377), AP (296), SC (107), PE (38), RS (37), MG (22), MA (17), MS (10), DF (8), SE (8), AM (7), BA (7), CE (7), RO (6), GO (5), AL (3), MT (1), TO (1). The genotype identified was D8; 
# - In 2021 - 676 ​​cases were confirmed, being: AP (534), PA (116), AL (11), SP (9), RJ (3) and CE (3); The genotype identified was D8. 
# - In 2022 - 41 cases were confirmed, being: AP (30), SP (8), PA (1), RJ (2); The genotype identified was D8; 
# - In 2022, the last confirmed case of measles occurred on 06/05/2022, according to the date of onset of the rash.
# In 2024, 2 imported cases were confirmed, namely: RS (1) and MG (1); the genotypes identified were B3 and D8 (Victória lineage) respectively."

read_clean_case_data <- function(path, drop_cols = NULL) {
  read.delim(path, sep = ";", fileEncoding = "Latin1", skip = 3, nrow = 5596) %>%  # remove notes from top and bottom and total row at bottom
    select(-any_of(drop_cols)) %>%
    rename(muni = Município.de.residência) %>% 
    filter(!str_detect(muni, "IGNORADO")) %>% 
    pivot_longer(
      cols = -"muni",
      names_to = "year",
      values_to = "measles_cases") %>% 
    mutate(year = as.numeric(substring(year, 2)),
           measles_cases = as.numeric(case_when(
             measles_cases == "-" ~ "0", # dash indicates 0 deaths
             TRUE ~ measles_cases)),
           muni_code_6 = as.numeric(str_split_fixed(muni, ' ', 2)[, 1])) %>% 
    select(c("muni_code_6", "year", "measles_cases"))
}

measles_cases <- bind_rows(
  read_clean_case_data("~/Brazil-measles/data/case_data/measles_cases_2001_2006.csv",
                       drop_cols = "Total"),
  read_clean_case_data("~/Brazil-measles/data/case_data/measles_cases_2007_2024.csv",
                       drop_cols = c("X.1975", "X1979", "X1981", "X1985", "X1989", "X1990", 
                                     "X1992", "X1993", "X1994", "X1995", "X1996", "X2000", 
                                     "X2001", "X2002", "X2005", "X2006", "Total")))




# measles mortality
# "Deaths per Residence by Year of Death according to Municipality - ICD-10 Category: B05 Measles"

# source:
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# data selection: 
#     row = municipality, column = year, content = deaths by residence, cause - ICD-10 = B05 Measles
#     make sure to select display rows with 0 data
 
 measles_deaths <- 
   read.delim("~/Brazil-measles/data/mortality_data/measles_mortality.csv",
              sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5596) %>%  # remove notes from top and bottom and total row at bottom
    select(-Total) %>%
    rename(muni = Município) %>% 
    filter(!str_detect(muni, "IGNORADO")) %>% 
    pivot_longer(
      cols = -"muni",
      names_to = "year",
      values_to = "measles_deaths") %>% 
    mutate(year = as.numeric(substring(year, 2)),
           measles_deaths = as.numeric(case_when(
             measles_deaths == "-" ~ "0", # dash indicates 0 deaths
             TRUE ~ measles_deaths)),
           muni_code_6 = as.numeric(str_split_fixed(muni, ' ', 2)[, 1])) %>% 
    select(c("muni_code_6", "year", "measles_deaths"))


 # mumps mortality
 # "Deaths per Residence by Year of Death according to Municipality - ICD-10 Category: B26 Mumps"
 
 # source:
 # http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
 # data selection: 
 #     row = municipality, column = year, content = deaths by residence, ICD-10 Category = B26 Mumps
 #     make sure to select display rows with 0 data
 
mumps_deaths <- 
   read.delim("~/Brazil-measles/data/mortality_data/mumps_mortality.csv",
              sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5596) %>%  # remove notes from top and bottom and total row at bottom
   select(-Total) %>%
   rename(muni = Município) %>% 
   filter(!str_detect(muni, "IGNORADO")) %>% 
   pivot_longer(
     cols = -"muni",
     names_to = "year",
     values_to = "mumps_deaths") %>% 
   mutate(year = as.numeric(substring(year, 2)),
          mumps_deaths = as.numeric(case_when(
            mumps_deaths == "-" ~ "0", # dash indicates 0 deaths
            TRUE ~ mumps_deaths)),
          muni_code_6 = as.numeric(str_split_fixed(muni, ' ', 2)[, 1])) %>% 
   select(c("muni_code_6", "year", "mumps_deaths")) 
 


# whooping cough mortality
# "Deaths per Residence by Year of Death according to Municipality - ICD-10 Category: A37 Whooping Cough"

# source:
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# data selection: 
#     row = municipality, column = year, content = deaths by residence, ICD-10 Category = A37 Whooping Cough
#     make sure to select display rows with 0 data

whooping_deaths <- 
  read.delim("~/Brazil-measles/data/mortality_data/whooping_cough_mortality.csv",
             sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5596) %>%  # remove notes from top and bottom and total row at bottom
  select(-Total) %>%
  rename(muni = Município) %>% 
  filter(!str_detect(muni, "IGNORADO")) %>% 
  pivot_longer(
    cols = -"muni",
    names_to = "year",
    values_to = "whooping_deaths") %>% 
  mutate(year = as.numeric(substring(year, 2)),
         whooping_deaths = as.numeric(case_when(
           whooping_deaths == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ whooping_deaths)),
         muni_code_6 = as.numeric(str_split_fixed(muni, ' ', 2)[, 1])) %>% 
  select(c("muni_code_6", "year", "whooping_deaths")) 



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

poverty <- read.csv("~/Brazil-measles/data/poverty_data.csv") %>% 
  filter(municipality != "Total") %>%
  rename(pct_low_inc_1991 = X1991,
         pct_low_inc_2000 = X2000,
         pct_low_inc_2010 = X2010) %>%
  mutate(across(c(pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010), as.numeric), # "..." is NA
         muni_code_6 = as.numeric(str_split_fixed(municipality, ' ', 2)[, 1])) %>% 
  select(-c(Total, municipality))


# SES - education
# 2010 categories not consistent with other years
# https://datasus.saude.gov.br/educacao-censos-1991-2000-e-2010

# manual cleaning in text editor prior to reading in: removing notes from bottom and top; renaming column to "muni"

read_clean_educ <- function(file, rename_map) {
  read.delim(file, dec = ",", sep = ";", fileEncoding = "Latin1") %>%
    filter(muni != "Total") %>% 
    rename(!!!rename_map) %>%
    mutate(muni_code_6 = as.numeric(str_split_fixed(muni, " ", 2)[, 1])) %>%
    select(-any_of(c("Não.determinada", "Alfabetização.de.adultos", "Total", "muni")))
}

# this wacky setup reduces some clutter
educ <- tibble(
  file = c("~/Brazil-measles/data/educ_data/educ_1991.csv",
           "~/Brazil-measles/data/educ_data/educ_2000.csv",
           "~/Brazil-measles/data/educ_data/educ_2010.csv"),
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
urban <- read.csv("~/Brazil-measles/data/urban_pop.csv", skip = 3, nrow = 11131) %>%  # remove notes from top and bottom and total row at bottom
  select(-Município) %>%
  rename(muni_code = Cód.,
         status = Situação.do.domicílio) %>% 
  filter(!str_detect(X1991, "Total") & status == "Urbana") %>% 
  rename(pct_urban_1991 = X1991,
         pct_urban_2000 = X2000,
         pct_urban_2010 = X2010) %>% 
  select(-status) %>% 
  mutate(across(everything(), ~as.numeric(.))) # "..." is NA




# Simon's data:
load("~/Brazil-measles/data/vax_data.RData")
vax_demog_geog <- merged_data
rm(merged_data)

geog <- vax_demog_geog %>% 
  select(muni_id, muni_id_6, muni_nm, muni_nm_clean, uf_nm, uf_id, regiao_nm) %>% 
  rename(muni_code = muni_id,
         muni_code_6 = muni_id_6,
         muni_name = muni_nm, 
         muni_name_clean = muni_nm_clean, 
         state_name = uf_nm,
         state_code = uf_id,
         region = regiao_nm) %>% 
  mutate(region = recode(region,
                         "Norte" = "north",
                         "Nordeste" = "northeast",
                         "Centro Oeste" = "centralwest",
                         "Sudeste" = "southeast",
                         "Sul" = "south"),
         muni_code = as.numeric(muni_code),
         muni_code_6 = as.numeric(muni_code_6),
         state_code = as.numeric(state_code)) %>% 
  distinct()

mmr_vax <- vax_demog_geog %>% 
  select(LOCAL_CODE, YEAR, INDICATOR, PC_COVERAGE, FL_GOAL) %>% 
  filter(INDICATOR == "FL_Y1_MMR2") %>% 
  rename(muni_code_6 = LOCAL_CODE,
         MMR2_coverage = PC_COVERAGE, 
         MMR2_goal = FL_GOAL,
         year = YEAR) %>% 
  select(-INDICATOR)
  
dtp_vax_demog <- vax_demog_geog %>% 
    select(LOCAL_CODE, YEAR, INDICATOR, PC_COVERAGE, FL_GOAL,
           YEAR, POPULATION, BIRTH_RATE, IMR, CMR, MHDI, GINI, GDP_PC, SANITATION) %>% 
    filter(INDICATOR == "FL_Y1_DTP") %>% 
  rename(muni_code_6 = LOCAL_CODE,
         DTP_coverage = PC_COVERAGE, 
         DTP_goal = FL_GOAL,
         year = YEAR) %>% 
    select(-INDICATOR)

vax_data = merge(dtp_vax_demog, mmr_vax, by = c("year", "muni_code_6"), all = TRUE)



# merge all data

# when downloading data, years with zero deaths are excluding as columns
# All mortality data is 1996-2023, so change NAs here to 0
mortality <- merge(measles_deaths, mumps_deaths, 
              by = c("muni_code_6", "year"), all = TRUE) %>% 
  merge(whooping_deaths, by = c("muni_code_6", "year"), all = TRUE) %>% 
  mutate(across(c(measles_deaths, mumps_deaths, whooping_deaths), ~replace_na(., 0)))



data_clean <- merge(mortality, measles_cases, by = c("muni_code_6", "year"), all = TRUE) %>% 
  merge(vax_data, by = c("muni_code_6", "year"), all = TRUE) %>% 
  filter(year != 2024) %>% # only have case data for 2024
  left_join(geog, by = "muni_code_6") %>% 
  left_join(poverty, by = "muni_code_6") %>% 
  left_join(educ, by = "muni_code_6") %>% 
  left_join(urban, by = "muni_code") %>% 
  rename(population = POPULATION,
         CDR = CMR,
         sanitation = SANITATION,
         CBR = BIRTH_RATE) %>% 
  select(muni_code, muni_name_clean, year, MMR2_coverage, MMR2_goal, DTP_coverage, DTP_goal, 
         measles_cases, measles_deaths, mumps_deaths, whooping_deaths,
         pct_urban_1991, pct_urban_2000, pct_urban_2010,
         pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010, 
         educ_pct_8_yrs_1991, educ_pct_8_yrs_2000, pct_complete_educ_2010, 
         population, CBR, IMR, CDR, MHDI, GINI, GDP_PC, sanitation, 
         muni_code_6, muni_name, state_name, state_code, region)

save(data_clean, file = "~/Brazil-measles/data/clean_brazil_data.RData")

