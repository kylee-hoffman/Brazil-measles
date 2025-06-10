library(stringr)
library(tidyr)
library(dplyr)

# data from DATASUS
# manual cleaning includes changing semicolon-separated to comma-separated and removing notes columns

# mortality - measles
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# "Number of deaths that occurred, counted according to the place of residence of the deceased."
measles_deaths <- read.csv("~/Brazil-measles/data/measles_deaths_muni.csv")

measles_deaths[c("muni_code", "muni")] <- str_split_fixed(measles_deaths$municipality, ' ', 2)

measles_deaths <- measles_deaths %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "measles_deaths") %>% 
  filter(municipality != "Total" & !str_detect(municipality, "IGNORADO")) %>%
  select(c("muni_code", "muni", "year", "measles_deaths")) %>% 
  mutate(year = as.numeric(gsub("^X", "", year)),
         measles_deaths = as.numeric(case_when(
           measles_deaths == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ measles_deaths)))

# mortality - mumphs

# cases
# "confirmed cases by Year of Notification according to Municipality of notification
# "The last confirmed case of Rubella in Brazil occurred in 2008;
# "- In 2015 - 214 cases were confirmed, being: CE (211), SP (02) and RR (01); 
# - In 2016 and 2017, Brazil did not confirm any cases of measles; 
# - In 2018 - 9,325 cases were confirmed: AM (8,791), RR (361), PA (83), RS (47), RJ (20), PE (4), SE (4), BA (3), SP (9), RO (2) and DF (1); the genotypes identified were D8 and 1B3; 
# - In 2019 - 20,901 cases were confirmed, being: SP (17,816), PR (1,071), RJ (463), PA (405), PE (344), SC (297), MG (143), RS (100), BA (80), PB (66), AL (35), CE (19), GO (12), DF (11), RN (9), MA (8), SE (6), AM (4), ES (4), PI (3), MS (2), AP (2) and RR (1). The genotype identified was D8. 
# In 2020 - 8,100 cases were confirmed, being: PA (4,906), RJ (1,358), SP (879), PR (377), AP (296), SC (107), PE (38), RS (37), MG (22), MA (17), MS (10), DF (8), SE (8), AM (7), BA (7), CE (7), RO (6), GO (5), AL (3), MT (1), TO (1). The genotype identified was D8; 
# - In 2021 - 676 ​​cases were confirmed, being: AP (534), PA (116), AL (11), SP (9), RJ (3) and CE (3); The genotype identified was D8. 
# - In 2022 - 41 cases were confirmed, being: AP (30), SP (8), PA (1), RJ (2); The genotype identified was D8; 
# - In 2022, the last confirmed case of measles occurred on 06/05/2022, according to the date of onset of the rash.
# In 2024, 2 imported cases were confirmed, namely: RS (1) and MG (1); the genotypes identified were B3 and D8 (Victória lineage) respectively."
# -	- Numerical data equal to 0 not resulting from rounding.

measles_cases <- read.csv("~/Brazil-measles/data/measles_cases.csv")

measles_cases[c("muni_code", "muni")] <- str_split_fixed(measles_cases$municipality, ' ', 2)

measles_cases <- measles_cases %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "measles_cases") %>% 
  filter(municipality != "Total" & !str_detect(municipality, "IGNORADO")) %>%
  select(c("muni_code", "muni", "year", "measles_cases")) %>% 
  mutate(year = as.numeric(gsub("^X", "", year)),
         measles_cases = as.numeric(case_when(
           measles_cases == "-" ~ "0", # dash indicates 0 deaths
           TRUE ~ measles_cases)))

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
poverty <- read.csv("~/Brazil-measles/data/poverty_muni.csv")

poverty[c("muni_code", "muni")] <- str_split_fixed(poverty$municipality, ' ', 2)

poverty <- poverty %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "pct_low_inc") %>% 
  filter(municipality != "Total") %>%
  select(c("muni_code", "muni", "year", "pct_low_inc")) %>% 
  mutate(year = as.numeric(gsub("^X", "", year)),
         pct_low_inc = as.numeric(case_when(
           pct_low_inc == "..." ~ NA,
           TRUE ~ pct_low_inc)))

# SES - education
# dash means 0%
# 2010 categories not consistent with other years
# https://datasus.saude.gov.br/educacao-censos-1991-2000-e-2010
# putting this on hold until decision made about standardizing b/w years
