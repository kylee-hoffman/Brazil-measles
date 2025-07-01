library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library('sf')
library(janitor)

# measles cases according to municipality of residence by year
# "confirmed cases by Year 1st Symptom(s) according to Municipality of residence"

# sources:
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def (2001-2006)
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def (2007-2024)
# data selection: 
#     row = municipality, column = year of first symptoms, content = cases by residence, filtered to measles
#     make sure to select display rows with 0 data

# notes from website:
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
  read.delim(path, sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5597) %>%  # remove notes from top and bottom and total row at bottom
    clean_names() %>% 
    dplyr::select(-any_of(drop_cols)) %>%
    rename(muni = municipio_de_residencia) %>% 
    filter(!str_detect(muni, "IGNORADO")) %>% 
    pivot_longer(cols = -"muni",
                 names_to = "year",
                 values_to = "measles_cases") %>% 
    mutate(year = as.numeric(substring(year, 2)),
           measles_cases = as.numeric(case_when(
             measles_cases == "-" ~ "0", # dash indicates 0 deaths
             TRUE ~ measles_cases)),
           muni_code_6 = as.numeric(str_split_fixed(muni, ' ', 2)[, 1])) %>% 
    dplyr::select(c("muni_code_6", "year", "measles_cases"))
}

measles_cases <- bind_rows(
  read_clean_case_data("~/Brazil-measles/data/raw/case_data/sinan_measles_cases_01_06.csv",
                       drop_cols = "total"),
  read_clean_case_data("~/Brazil-measles/data/raw/case_data/sinan_measles_cases_07_23.csv",
                       drop_cols = c("x_1975", "x1979", "x1981", "x1985", "x1989", "x1990", 
                                     "x1992", "x1993", "x1994", "x1995", "x1996", "x2000", 
                                     "x2001", "x2002", "x2005", "x2006", "total")))


# measles mortality
# "Deaths per Residence by Year of Death according to Municipality - ICD-10 Category: B05 Measles"

# source:
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# data selection: 
#     row = municipality, column = year, content = deaths by residence, cause - ICD-10 = B05 Measles
#     make sure to select display rows with 0 data
 
 measles_deaths <- 
   read.delim("~/Brazil-measles/data/raw/mortality_data/measles_mortality.csv",
              sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5597) %>%  # remove notes from top and bottom and total row at bottom
   dplyr::select(-Total) %>%
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
   dplyr::select(c("muni_code_6", "year", "measles_deaths"))

 # mumps mortality
 # "Deaths per Residence by Year of Death according to Municipality - ICD-10 Category: B26 Mumps"
 
 # source:
 # http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
 # data selection: 
 #     row = municipality, column = year, content = deaths by residence, ICD-10 Category = B26 Mumps
 #     make sure to select display rows with 0 data
 
mumps_deaths <- 
   read.delim("~/Brazil-measles/data/raw/mortality_data/mumps_mortality.csv",
              sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5597) %>%  # remove notes from top and bottom and total row at bottom
  dplyr::select(-Total) %>%
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
  dplyr::select(c("muni_code_6", "year", "mumps_deaths")) 
 


# whooping cough mortality
# "Deaths per Residence by Year of Death according to Municipality - ICD-10 Category: A37 Whooping Cough"

# source:
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# data selection: 
#     row = municipality, column = year, content = deaths by residence, ICD-10 Category = A37 Whooping Cough
#     make sure to select display rows with 0 data

whooping_deaths <- 
  read.delim("~/Brazil-measles/data/raw/mortality_data/whooping_cough_mortality.csv",
             sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 5597) %>%  # remove notes from top and bottom and total row at bottom
  dplyr::select(-Total) %>%
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
  dplyr::select(c("muni_code_6", "year", "whooping_deaths")) 

# merge all mortality
# when downloading data, years with zero deaths are excluding as columns
# All mortality data is 1996-2023, so change NAs here to 0
mortality <- merge(measles_deaths, mumps_deaths, 
                   by = c("muni_code_6", "year"), all = TRUE) %>% 
  merge(whooping_deaths, by = c("muni_code_6", "year"), all = TRUE) %>% 
  mutate(across(c(measles_deaths, mumps_deaths, whooping_deaths), ~replace_na(., 0)))

# check duplicates
mortality %>%
  group_by(muni_code_6, year) %>%
  filter(n() > 1)

rm(measles_deaths, mumps_deaths, whooping_deaths)







# healthcare

# UBS establishments
# CNES - Establishments by Type
# Quantity per Year/month compet. according to Municipality
# Type of Establishment: HEALTH CENTER/BASIC UNIT
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/estabbr.def
ubs <- read.delim("~/Brazil-measles/data/raw/cnes_UBS_count.csv",
                  sep = ";", dec = ",", fileEncoding = "Latin1",
                  skip = 4, nrow = 5597) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "UBS") %>% 
  mutate(year = as.numeric(substring(year, 2, 5)),
         UBS = as.numeric(case_when(
           UBS == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ UBS)),
         muni_code_6 = as.numeric(str_split_fixed(Município, ' ', 2)[, 1])) %>% 
  dplyr::select(c("muni_code_6", "year", "UBS"))

# CNES - Human Resources - Professionals - Individuals - according to CBO 2002
# Quantity per Year/month competed. according to Municipality
# Higher Education Occupations: Nurse
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/prid02br.def
nurse <- read.delim("~/Brazil-measles/data/raw/cnes_nurses_08_23.csv",
                    sep = ";", dec = ",", fileEncoding = "Latin1",
                    skip = 4, nrow = 5597) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "nurses") %>% 
  mutate(year = as.numeric(substring(year, 2, 5)),
         nurses = as.numeric(case_when(
           nurses == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ nurses)),
         muni_code_6 = as.numeric(str_split_fixed(Município, ' ', 2)[, 1])) %>% 
  dplyr::select(c("muni_code_6", "year", "nurses"))


# CNES - Human Resources - Professionals - Individuals - according to CBO 2002
# Quantity per Year/month competed. according to Municipality
# Higher Level Occupations: Clinical Physician
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/prid02br.def
doctor <- read.delim("~/Brazil-measles/data/raw/cnes_clinical_doctors_08_23.csv",
                     sep = ";", dec = ",", fileEncoding = "Latin1",
                     skip = 4, nrow = 5597) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "doctors") %>% 
  mutate(year = as.numeric(substring(year, 2, 5)),
         doctors = as.numeric(case_when(
           doctors == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ doctors)),
         muni_code_6 = as.numeric(str_split_fixed(Município, ' ', 2)[, 1])) %>% 
  dplyr::select(c("muni_code_6", "year", "doctors"))

healthcare <- merge(ubs, nurse, by = c("muni_code_6", "year"), all = T) %>% 
  merge(doctor, by = c("muni_code_6", "year"), all = T) %>% 
  dplyr::select(muni_code_6, year, UBS, nurses, doctors)

rm(ubs, nurse, doctor)




# vaccination

mono <- read.delim("~/Brazil-measles/data/raw/vaccination/monovalent_measles_coverage.csv",
                   sep = ";", dec = ",", fileEncoding = "Latin1") %>% 
  filter(Município != "Total" & !str_detect(Município, "EXTINTO")) %>% 
  dplyr::select(-X.Total) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "monovalent_coverage") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         muni_code_6 = substr(Município, 1, 6)) %>% 
  dplyr::select(-Município)

mmr1 <- read.delim("~/Brazil-measles/data/raw/vaccination/MMR1_coverage.csv",
                   sep = ";", dec = ",", fileEncoding = "Latin1") %>% 
  filter(Município != "Total" & !str_detect(Município, "EXTINTO")) %>% 
  dplyr::select(-X.Total) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "MMR1_coverage") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         muni_code_6 = substr(Município, 1, 6)) %>% 
  dplyr::select(-Município)

mmr2 <- read.delim("~/Brazil-measles/data/raw/vaccination/MMR2_coverage.csv",
                   sep = ";", dec = ",", fileEncoding = "Latin1") %>% 
  filter(Município != "Total" & !str_detect(Município, "EXTINTO")) %>% 
  dplyr::select(-X.Total) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "MMR2_coverage") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         muni_code_6 = substr(Município, 1, 6)) %>% 
  dplyr::select(-Município)


mmr_2023 <- read.csv("~/Brazil-measles/data/raw/vaccination/vaccines_2023.csv") %>% 
  clean_names() %>% 
  rename(MMR2_coverage = triplice_viral_2_dose,
         MMR1_coverage = triplice_viral_1_dose) %>% 
  filter(!(municipio_residencia %in% c("Totais", "", NA))) %>% 
  mutate(muni_code_6 = substr(municipio_residencia, 1, 6),
         across(c(MMR1_coverage, MMR2_coverage), ~as.numeric(gsub("[\\%,]", "", .))),
         year = 2023) %>% 
  dplyr::select(muni_code_6, year, MMR2_coverage, MMR1_coverage)

coverage <- merge(mono, mmr1, by = c("muni_code_6", "year"), all = TRUE) %>% 
  merge(mmr2, by = c("muni_code_6", "year"), all = TRUE) %>% 
  bind_rows(mmr_2023) %>% 
  mutate(muni_code_6 = as.numeric(muni_code_6))

rm(mono, mmr1, mmr2, mmr_2023)





# merge health data
health_data <- merge(mortality, measles_cases, by = c("muni_code_6", "year"), all = TRUE) %>% 
  merge(coverage, by = c("muni_code_6", "year"), all = TRUE) %>%
  left_join(healthcare, by = c("muni_code_6", "year"))


save(health_data, file = "~/Brazil-measles/data/health_data.RData") 

rm(coverage, healthcare, measles_cases, mortality, read_clean_case_data, read_clean_educ)


