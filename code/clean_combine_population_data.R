library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library('sf')
library(janitor)
library(stringi)
library("readxl")





muni_id <- data.frame(read_sf('~/Brazil-measles/data/brazil_muni_sf/BR_Municipios_2024.shp')) %>% 
  rename(muni_code = CD_MUN,
         muni_name = NM_MUN, 
         state_code = CD_UF) %>% 
  mutate(muni_code_6 = as.numeric(substr(muni_code, 1, 6)),
         muni_code = as.numeric(muni_code),
         state_code = as.numeric(state_code),
         muni_name_clean = stri_trans_general(muni_name, id = "Latin-ASCII"),
         muni_name_clean = gsub(" do| de| dos| das", "", muni_name_clean),
         muni_name_clean = gsub("-", " ", muni_name_clean)) %>% 
  dplyr::select(muni_code, muni_code_6, muni_name, muni_name_clean, state_code)






# population data files

pop96 <- read_excel("~/Brazil-measles/data/raw/population/pop_count_1996.xlsx",
                    skip = 5, n_max = 4974) %>% 
  dplyr::select(...1, Total) %>% 
  rename(muni_code = ...1,
         population = Total) %>% 
  mutate(year = 1996,
         muni_code = as.numeric(muni_code),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6)))



pop97 <- read_excel("~/Brazil-measles/data/raw/population/pop_estimates_1997.xls", skip = 2, n_max = 5508) %>% 
  clean_names() %>% 
  rename(population = populacao_estimada,
         state_code = cod_uf) %>% 
  mutate(year = 1997,
         muni_name_clean = stri_trans_general(nome_do_municipio, id = "Latin-ASCII"),
         muni_name_clean = gsub(" do| de| dos| das", "", muni_name_clean),
         muni_name_clean = gsub("-", " ", muni_name_clean)) %>% 
  dplyr::select(muni_name_clean, state_code, year, population)


pop98 <- read_excel("~/Brazil-measles/data/raw/population/pop_estimates_1998.xls", skip = 2, n_max = 5507) %>% 
  clean_names() %>% 
  rename(population = populacao_estimada,
         state_code = cod_uf) %>% 
  mutate(year = 1998,
         muni_name_clean = stri_trans_general(nome_do_municipio, id = "Latin-ASCII"),
         muni_name_clean = gsub(" do| de| dos| das", "", muni_name_clean),
         muni_name_clean = gsub("-", " ", muni_name_clean)) %>% 
  dplyr::select(muni_name_clean, state_code, year, population)

pop96_99 <- read_excel("~/Brazil-measles/data/raw/population/pop_estimates_1999.xls", skip = 2, n_max = 5507) %>% 
  clean_names() %>% 
  rename(population = populacao_estimada,
         state_code = cod_uf) %>% 
  mutate(year = 1999,
         muni_name_clean = stri_trans_general(nome_do_municipio, id = "Latin-ASCII"),
         muni_name_clean = gsub(" do| de| dos| das", "", muni_name_clean),
         muni_name_clean = gsub("-", " ", muni_name_clean)) %>% 
  bind_rows(pop97, pop98) %>% 
  mutate(muni_name_clean = case_when(
    state_code == 23 & muni_name_clean == "Itapage" ~ "Itapaje",
    state_code == 15 & muni_name_clean == "Santa Isabel Para" ~ "Santa Izabel Para",
    state_code == 28 & muni_name_clean == "Gracho Cardoso" ~ "Graccho Cardoso",
    state_code == 33 & muni_name_clean == "Parati" ~ "Paraty",
    state_code == 35 & muni_name_clean == "Florinia" ~ "Florinea",
    state_code == 51 & muni_name_clean == "Poxoreo" ~ "Poxoreu",
    state_code == 42 & muni_name_clean == "Picarras" ~ "Balneario Picarras",
    state_code == 26 & muni_name_clean == "Iguaraci" ~ "Iguaracy",
    state_code == 35 & muni_name_clean == "Moji Cruzes" ~ "Mogi Cruzes",
    state_code == 35 & muni_name_clean == "Moji Mirim" ~ "Mogi Mirim",
    state_code == 31 & muni_name_clean == "Sao Thome Letras" ~ "Sao Tome Letras",
    state_code == 35 & muni_name_clean == "Sao Luis Paraitinga" ~ "Sao Luiz Paraitinga",
    state_code == 33 & muni_name_clean == "Trajano Morais" ~ "Trajano Moraes",
    state_code == 25 & muni_name_clean == "Santa Teresinha" ~ "Santa Teresinha",
    state_code == 42 & muni_name_clean == "Presidente Castelo Branco" ~ "Presidente Castello Branco",
    state_code == 43 & muni_name_clean == "Santana Livramento" ~ "Sant'Ana Livramento",
    state_code == 25 & muni_name_clean == "Serido" ~ "Sao Vicente Serido",
    state_code == 17 & muni_name_clean == "Fortaleza Tabocao" ~ "Tabocao",
    TRUE ~ muni_name_clean)) %>% 
  left_join(muni_id[, c("muni_name_clean", "muni_code", "muni_code_6", "state_code")], by = c("state_code", "muni_name_clean"))%>%
  bind_rows(pop96) %>% 
  dplyr::select(muni_code_6, year, population)




pop00_23 <- read.delim("~/Brazil-measles/data/raw/population/pop_estimates_00_23.csv",
                       sep = ";", fileEncoding = "Latin1", skip = 3, nrow = 5597) %>% 
  filter(!str_detect(Município, "IGNORADO")) %>% 
  pivot_longer(cols = -"Município",
               names_to = "year",
               values_to = "population") %>% 
  mutate(year = as.numeric(substring(year, 2)),
         population = as.numeric(case_when(
           population == "-" ~ "0", # dash indicates 0
           TRUE ~ population)),
         muni_code_6 = as.numeric(str_split_fixed(Município, ' ', 2)[, 1])) %>% 
  dplyr::select(c("muni_code_6", "year", "population"))

pop <- bind_rows(pop96_99, pop00_23)


save(pop, file = "~/Brazil-measles/data/muni_pop_96_23.RData")

rm(pop00_23, pop96, pop97, pop98, pop99, pop96_99, muni_id)
