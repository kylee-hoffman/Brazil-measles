library(tidyr)
library(dplyr)

load("~/Brazil-measles/data/clean_brazil_data.RData")

vars <- data.frame(variable = names(data_clean)) %>% 
  mutate(description = case_when(
    variable == "muni_code" ~ "7 digit municipality code (numeric)",
    variable == "year" ~ "year (numeric)",
    variable == "MMR1_coverage" ~ "MMR vaccine first dose coverage",
    variable == "MMR2_coverage" ~ "MMR vaccine second dose coverage",
    variable == "monovalent_coverage" ~ "monovalent measles vaccine coverage",
    variable == "MMR2_goal" ~ "Binary: 1 if MMR second dose coverage equal to or greater than goal of 95%, 0 otherwise; 1996-2021 (numeric)",
    variable == "DTP_coverage" ~ "DTP vaccine coverage; 1996-2023 (numeric)",
    variable == "DTP_goal" ~ "Binary: 1 if DTP coverage equal to or greater than goal of 95%, 0 otherwise; 1996-2021 (numeric)",
    variable == "measles_cases" ~ "number of measles cases for residents of a municipality; 2001-2023 (numeric)",
    variable == "measles_deaths" ~ "number of measles deaths for residents of a municipality; 1996-2023 (numeric)",
    variable == "mumps_deaths" ~ "number of mumps deaths for residents of a municipality; 1996-2023 (numeric)",
    variable == "whooping_deaths" ~ "number of whooping cough deaths for residents of a municipality; 1996-2023 (numeric)",
    variable == "pct_urban_1991" ~ "Percent of municipality that lives in an urban area in 1991 (numeric)",
    variable == "pct_urban_2000" ~ "Percent of municipality that lives in an urban area in 2000 (numeric)",
    variable == "pct_urban_2010" ~ "Percent of municipality that lives in an urban area in 2010 (numeric)",
    
    variable == "pct_low_inc_1991" ~ "percent of municipality with a monthly per capita household income of up to half the minimum wage in 1991 (numeric)",
    variable == "pct_low_inc_2000" ~ "percent of municipality with a monthly per capita household income of up to half the minimum wage in 2000 (numeric)",
    variable == "pct_low_inc_2010" ~ "percent of municipality with a monthly per capita household income of up to half the minimum wage in 2010 (numeric)",
    
    variable == "educ_pct_8_yrs_1991" ~ "percent of municipality that completed 8 years and more of study in 1991 (numeric)",
    variable == "educ_pct_8_yrs_2000" ~ "percent of municipality that completed 8 years and more of study in 2000 (numeric)",
    variable == "pct_complete_educ_2010" ~ "percent of municipality that completed 2nd cycle of basic education or more in 2010 (numeric)",
    
    variable == "population" ~ "popualtion of municipality; 1996-2023 (numeric)",
    variable == "CBR" ~ "Live births per 1,000 population; 1996-2021 (numeric)",
    variable == "IMR" ~ "infant mortality rate; number of deaths per 1,000 live births of children under 1 year old; 1996-2021 (numeric)",
    variable == "CDR" ~ "crude death rate; deaths per 1,000 population; 1996-2021 (numeric)",
    variable == "MHDI" ~ "Brazilian Municipal Human Development Index in 1991, 2000, and 2010. three dimensions: education, longevity, and income; 1996-2021 (numeric)",   
    variable == "GINI" ~ "Value of Gini coefficient; 1996-2021 (numeric)",
    variable == "GDP_PC" ~ "per capita Gross Domestic Product, current price (BRL 1,00); 1996-2021 (numeric)",
    variable == "sanitation" ~ "Percent of people in households with an inadequate water supply and sanitation; 1996-2021 (numeric)",
    variable == "muni_code_6" ~ "6-digit municipality code (numeric)",
    variable == "muni_name" ~ "municipality name (character)",
    variable == "state_name" ~ "state name of municipality (character)",
    variable == "state_code" ~ "state code  of municipality (numeric)",
    variable == "region" ~ "region of municipality (character)"
  ),
  years_avail = case_when(
    variable == "MMR1_coverage" ~ "1999-2023",
    variable == "MMR2_coverage" ~ "2013-2023",
    variable == "monovalent_coverage" ~ "1996-2003",
    variable %in% c("DTP_coverage", "measles_deaths", 
                    "mumps_deaths", "whooping_deaths") ~ "1996-2023",
    
    variable == "measles_cases" ~ "2001-2023",
    
    variable %in% c("pct_urban_1991", "pct_urban_2000", "pct_urban_2010",
                    "pct_low_inc_1991", "pct_low_inc_2000", "pct_low_inc_2010",
                    "educ_pct_8_yrs_1991", "educ_pct_8_yrs_2000", "pct_complete_educ_2010") ~ "census year",
    
    variable %in% c("population", "CBR", "CDR", "IMR", "MHDI", "GINI", "GDP_PC",
                    "sanitation", "DTP_goal", "MMR2_goal") ~ "1996-2021",
    
    variable %in% c("muni_code", "muni_code_6", "year", "muni_name", "state_name", "state_code", "region") ~ "1996-2023"
  ),
  source = case_when(
    variable %in% c("muni_code", "year") ~ "",
    
    variable %in% c("MMR2_coverage", "MMR1_coverage", "monovalent_coverage") ~ "DATASUS taabnet - SI-PNI",
    
    variable == "measles_cases" ~ "DATASUS tabnet - SINAN",
    
    variable %in% c("measles_deaths", "mumps_deaths", "whooping_deaths") ~ "DATASUS tabnet - SIMDO",
    
    variable %in% c("pct_urban_1991", "pct_urban_2000", "pct_urban_2010") ~ "IBGE table 202",

    variable %in% c("pct_low_inc_1991", "pct_low_inc_2000", "pct_low_inc_2010") ~ "DATASUS tabnet/IBGE",

    variable %in% c("educ_pct_8_yrs_1991", "educ_pct_8_yrs_2000", "pct_complete_educ_2010") ~ "DATASUS tabnet/IBGE",

    variable %in% c("population", "CBR", "CDR", "IMR", "MHDI", "GINI", "GDP_PC",
                  "sanitation", "DTP_goal", "MMR2_goal") ~ "",
   
    variable %in% c("muni_code", "muni_name", "state_name", "state_code", "region") ~ "IBGE"
  ),
  link = case_when(
    variable %in% c("muni_code", "year") ~ "",
    
    variable == "measles_cases" ~ "",
    
    variable %in% c("measles_deaths", "mumps_deaths", "whooping_deaths") ~ "",
    
    variable %in% c("pct_urban_1991", "pct_urban_2000", "pct_urban_2010") ~ "https://sidra.ibge.gov.br/tabela/202",
    
    variable %in% c("pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010") ~ "https://datasus.saude.gov.br/trabalho-e-renda-censos-1991-2000-e-2010",
    
    variable %in% c("educ_pct_8_yrs_1991", "educ_pct_8_yrs_2000", "pct_complete_educ_2010") ~ "https://datasus.saude.gov.br/educacao-censos-1991-2000-e-2010",
    
    variable == "population" ~ "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvs2024br.def",
    
    variable %in% c("muni_code", "muni_name", "state_name", "state_code", "region") ~ "https://www.ibge.gov.br/en/geosciences/territorial-organization/territorial-meshes/18890-municipal-mesh.html?edicao=24069"
  ),
  link2 = case_when(
    variable %in% c("MMR2_coverage", "DTP_coverage") ~ "http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/cpnibr.def"
  ),
  link3 = case_when(
    variable %in% c("MMR2_coverage", "DTP_coverage") ~ "https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA.html"
  )) %>% 
  filter(variable != "tetra_coverage")

