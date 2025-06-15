library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(datazoom.amazonia)
library(purrr)
library(tibble)

# data from DATASUS

# municipality-level measles mortality by month
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def
# "Number of deaths that occurred, counted according to the place of residence of the deceased."
# manual cleaning in text editor prior to reading in: removing notes from bottom and top; renaming column to "muni"

read_datasus_mortality_csv_fun <- function(folder) {
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  combined_df <- do.call(rbind, lapply(csv_files, function(file) {
    df <- read.delim(file, sep = ";", fileEncoding = "Latin1") %>% 
      select(-Total) %>% 
      pivot_longer(
        cols = -"muni",
        names_to = "month",
        values_to = "measles_deaths") %>% 
      filter(muni != "Total" & !str_detect(muni, "IGNORADO")) %>%
      mutate(year = as.numeric(str_extract(tools::file_path_sans_ext(basename(file)), "\\d{4}")),
             measles_deaths = as.numeric(case_when(
               as.character(measles_deaths) == "-" ~ "0", # dash indicates 0 deaths
               TRUE ~ as.character(measles_deaths))),
             month = as.numeric(recode(month,
                                       "Janeiro" = "1",
                                       "Fevereiro" = "2",
                                       "Março" = "3",
                                       "MarAo" = "3",
                                       "Abril" = "4",
                                       "Maio" = "5",
                                       "Junho" = "6",
                                       "Julho" = "7",
                                       "Agosto" = "8",
                                       "Setembro" = "9",
                                       "Outubro" = "10",
                                       "Novembro" = "11",
                                       "Dezembro" = "12",
                                       .default = month)),
             muni_code = str_split_fixed(muni, ' ', 2)[, 1]) %>% 
      select(c("muni_code", "year", "month", "measles_deaths"))
    
    return(df)
  }))
  return(combined_df)
}

measles_deaths = read_datasus_mortality_csv_fun("~/Brazil-measles/data/mortality_data/measles")


# microdata may also be interesting
# SLOW
simdo <- load_datasus(
  dataset = "datasus_sim_do",
  time_period = 1996:2021,
  states = "all",
  raw_data = FALSE,
  keep_all = TRUE,
  language = "eng")

simdo_filtered <- simdo %>% 
  select(c("dtobito", "causabas", "racacor", "sexo", "dtnasc",
           "codmunres", "idade_anos", "file_name", "code_muni", "name_muni",
           "code_state", "abbrev_state", "t_infectious", "ocup",
           "linhaa", "linhab", "linhac", "linhad", "linhaii", "contador")) %>% 
  filter(str_detect(causabas, "^B05|A37|B06|B26|J20|A87|B01|B04|J09|J10|J11|J12") &
         !str_detect(file_name, "DOBR")) %>% # every record is duplicated from country and state files
  rename(death_date = dtobito,
         cod = causabas,
         race = racacor,
         sex = sexo,
         dob  = dtnasc,
         age_death = idade_anos,
         res_muni = codmunres,
         line_a = linhaa,
         line_b = linhab,
         line_c = linhac,
         line_d = linhad,
         line_ii = linhaii) %>% 
  mutate(month_death = lubridate::month(death_date),
         year_death = lubridate::year(death_date))

write_csv(simdo_filtered, "~/Brazil-measles/data/inf_dis_mort_96_21.csv")

save(simdo_96_06, file = "~/Brazil-measles/data/all_deaths_96_06.RData")
save(simdo_07_15, file = "~/Brazil-measles/data/all_deaths_07_15.RData")
save(simdo_16_21, file = "~/Brazil-measles/data/all_deaths_16_21.RData")


# cases by month of notification according to municipality by year

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
# -	= Numerical data equal to 0 not resulting from rounding.

# manual cleaning in text editor prior to reading in: removing notes from bottom and top; renaming column to "muni"

read_datasus_csv_fun <- function(folder) {
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  combined_df <- do.call(rbind, lapply(csv_files, function(file) {
    df <- read.delim(file, sep = ";") %>% 
      select(-Total) %>% 
      pivot_longer(
        cols = -"muni",
        names_to = "month",
        values_to = "measles_cases") %>% 
      filter(muni != "Total" & !str_detect(muni, "IGNORADO")) %>%
      mutate(year = as.numeric(str_extract(tools::file_path_sans_ext(basename(file)), "\\d{4}")),
             measles_cases = as.numeric(case_when(
               measles_cases == "-" ~ "0", # dash indicates 0 deaths
               TRUE ~ measles_cases)),
             month = as.numeric(recode(month,
                            "Jan" = "1",
                            "Fev" = "2",
                            "Mar" = "3",
                            "Abr" = "4",
                            "Mai" = "5",
                            "Jun" = "6",
                            "Jul" = "7",
                            "Ago" = "8",
                            "Set" = "9",
                            "Out" = "10",
                            "Nov" = "11",
                            "Dez" = "12",
                            .default = month)),
             muni_code = str_split_fixed(muni, ' ', 2)[, 1]) %>% 
      select(c("muni_code", "year", "month", "measles_cases"))
    
    return(df)
  }))
  return(combined_df)
}

measles_cases=read_datasus_csv_fun("~/Brazil-measles/data/case_data")


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
poverty <- read.csv("~/Brazil-measles/data/poverty_data.csv") %>% 
  filter(municipality != "Total") %>%
  rename(pct_low_inc_1991 = X1991,
         pct_low_inc_2000 = X2000,
         pct_low_inc_2010 = X2010) %>%
  mutate(across(c(pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010), as.numeric)) %>% # "..." is NA
  separate(municipality, into = c("muni_code", "muni"), sep = " ", extra = "merge") %>% 
  select(-Total)


# SES - education
# 2010 categories not consistent with other years
# https://datasus.saude.gov.br/educacao-censos-1991-2000-e-2010

# manual cleaning in text editor prior to reading in: removing notes from bottom and top; renaming column to "muni"

read_clean_educ <- function(file, rename_map) {
  read.delim(file, dec = ",", sep = ";", fileEncoding = "Latin1") %>%
    rename(!!!rename_map) %>%
    mutate(muni_code = str_split_fixed(muni, " ", 2)[, 1]) %>%
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
  reduce(full_join, by = "muni_code")



# merge everything

data <- merge(measles_deaths, measles_cases, 
              by = c("muni_code", "year", "month"), all = TRUE) %>% 
  merge(poverty, by = "muni_code") %>% 
  merge(educ, by = "muni_code") %>% 
  #mutate(measles_deaths = replace_na(measles_deaths, 0), # fill in 0 for years with no records
  #       measles_cases = replace_na(measles_cases, 0)) %>% 
  select(muni_code, muni, year, month, measles_cases, measles_deaths,
         pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010,
         educ_pct_8_yrs_1991, educ_pct_8_yrs_2000, pct_complete_educ_2010)

# things to consider: case data is 2007-2024, deaths are 1997-2023
# for deaths, NA before 2023 can essentially be changed to 0, but needs to still be NA for 2024
# for cases, NA after 2007 can be changed to 0.
# those NAs only exist because I did not check the box for "display zero lines" on DATASUS portal,
# so the 0 values were all left off, but those year/muni combos were added in from the SES data