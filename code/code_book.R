library(tidyr)
library(dplyr)

load("~/Brazil-measles/data/clean_brazil_data.RData")
# coverage variable = monovalent coverage 1996-2000, max between monovalent and MMR1 between 2000-2003, 
# MMR1 2004-2012, MMR2 2013-2023

# coverage2 variable = monovalent coverage 1996-2000, sum of monovalent and MMR1 2000-2003, 
# MMR1 2004-2012, MMR2 2013-2023

vars <- data.frame(variable = names(df)) %>% 
  mutate(description = case_when(
    variable == "muni_code" ~ "7 digit municipality code",
    variable == "year" ~ "year",
    variable == "measles_cases" ~ "measles cases by municipality residence",
    variable == "measles_deaths" ~ "deaths by municipality residence",
    variable == "mumps_deaths" ~ "deaths by municipality residence",
    variable == "whooping_deaths" ~ "deaths by municipality residence",
    
    variable == "measles_cases_p100k" ~ "measles cases per 100,000 by municipality residence",
    variable == "measles_deaths_p100k" ~ "measles deaths per 100,000 by municipality residence",
    
    variable == "outbreak" ~ "flags outbreak periods: 2013-2015 or 2018-2021, NA othewise",
    
    variable == "monovalent_coverage" ~ "monovalent measles vaccine coverage",
    variable == "MMR1_coverage" ~ "MMR first dose coverage",
    variable == "MMR2_coverage" ~ "MMR second dose coverage",
    variable == "coverage" ~ "combination of all measles vaccine coverage, equal to the higher coverage between monovalent and MMR1 between 2000-2003",
    variable == "coverage2" ~ "additive combination of measles vaccines, only MMR2 after 2013",
    variable == "goal" ~ "Binary: 1 if overall coverage equal to or greater than goal of 95%, 0 otherwise; 1996-2023",
    
    variable == "coverage_lag2" ~ "Coverage2 variable lagged 2 years",
    variable == "coverage_lag3" ~ "Coverage2 variable lagged 3 years",
    variable == "coverage_lag4" ~ "Coverage2 variable lagged 4 years",
    variable == "coverage_lag5" ~ "Coverage2 variable lagged 5 years",
    
    variable == "UBS" ~ "Number of UBS clinics in municipality as of Januaary each year",
    variable == "nurses" ~ "Number of nurses in municipality as of Januaary each year, no duplicate individuals",
    variable == "doctors" ~ "Number of doctors in municipality as of Januaary each year, no duplicate individuals",
    
    variable == "UBS_p100k" ~ "Number of UBS clinics per 100,000 municipality residents as of Januaary each year",
    variable == "nurses_p100k" ~ "Number of nurses per 100,000 municipality residents as of Januaary each year",
    variable == "doctors_p100k" ~ "Number of doctors per 100,000 municipality residents as of Januaary each year",
    
    variable == "pct_urban_1991" ~ "Percent of municipality that lives in an urban area in 1991",
    variable == "pct_urban_2000" ~ "Percent of municipality that lives in an urban area in 2000",
    variable == "pct_urban_2010" ~ "Percent of municipality that lives in an urban area in 2010",
    
    variable == "pct_low_inc_1991" ~ "Percent of municipality with a monthly per capita household income of up to half the minimum wage in 1991",
    variable == "pct_low_inc_2000" ~ "Percent of municipality with a monthly per capita household income of up to half the minimum wage in 2000",
    variable == "pct_low_inc_2010" ~ "Percent of municipality with a monthly per capita household income of up to half the minimum wage in 2010",
    
    variable == "educ_pct_8_yrs_1991" ~ "Percent of municipality that completed 8 years and more of study in 1991",
    variable == "educ_pct_8_yrs_2000" ~ "Percent of municipality that completed 8 years and more of study in 2000",
    variable == "pct_complete_educ_2010" ~ "Percent of municipality that completed 2nd cycle of basic education or more in 2010",
    
    variable == "population" ~ "Popualtion of municipality; intercensal count for 1996, estimates otherwise",
    variable == "muni_code_6" ~ "6-digit municipality code",
    variable == "muni_name" ~ "Municipality name",
    variable == "state_name" ~ "State name of municipality",
    variable == "state_code" ~ "State code  of municipality",
    variable == "region" ~ "Region of municipality"
  ),
  years_avail = case_when(
    variable == "coverage_lag2" ~ "1998-2023",
    variable == "coverage_lag3" ~ "1999-2023",    
    variable == "coverage_lag4" ~ "2000-2023",
    
    variable %in% c("UBS", "UBS_p100k") ~ "2007-2023",
    variable %in% c("nurses", "doctors", "nurses_p100k", "doctors_p100k") ~ "2008-2023",
    
    variable == "MMR1_coverage" ~ "1999-2023",
    variable == "MMR2_coverage" ~ "2013-2023",
    variable == "monovalent_coverage" ~ "1996-2003",
    
    variable %in% c("measles_deaths", "mumps_deaths", "whooping_deaths",
                    "coverage", "coverage2", "outbreak", "population", "goal",
                    "measles_deaths_p100k", "muni_code", "muni_code_6", "year", 
                    "muni_name", "state_name", "state_code", "region") ~ "1996-2023",
    
    variable %in% c("measles_cases", "measles_cases_p100k", "coverage_lag5") ~ "2001-2023",
    
    variable %in% c("pct_urban_1991", "pct_urban_2000", "pct_urban_2010",
                    "pct_low_inc_1991", "pct_low_inc_2000", "pct_low_inc_2010",
                    "educ_pct_8_yrs_1991", "educ_pct_8_yrs_2000", "pct_complete_educ_2010") ~ "census years",
  ),
  source = case_when(
    variable == "outbreak" ~ "derived from inference",

    variable %in% c("UBS", "UBS_p100k", "nurses", "doctors", 
                    "nurses_p100k", "doctors_p100k") ~ "CNES",
    
    variable %in% c("coverage", "coverage2", "goal", "coverage_lag2",
                    "coverage_lag3", "coverage_lag4", "coverage_lag5") ~ "derived from SI-PNI",
    
    variable %in% c("muni_code_6", "year") ~ "",
    
    variable %in% c("MMR2_coverage", "MMR1_coverage", "monovalent_coverage") ~ "SI-PNI",
    
    variable == "measles_cases" ~ "SINAN",
    
    variable == "measles_cases_p100k" ~ "derived from SINAN",
    
    variable == "measles_deaths_p100k" ~ "derived from SIMDO",
    
    variable %in% c("measles_deaths", "mumps_deaths", "whooping_deaths") ~ "SIMDO",
    
    variable %in% c("pct_urban_1991", "pct_urban_2000", "pct_urban_2010") ~ "IBGE table 202",

    variable %in% c("pct_low_inc_1991", "pct_low_inc_2000", "pct_low_inc_2010") ~ "IBGE",

    variable %in% c("educ_pct_8_yrs_1991", "educ_pct_8_yrs_2000", "pct_complete_educ_2010") ~ "IBGE",

    variable %in% c("population", "muni_code", "muni_name", "state_name", "state_code", "region") ~ "IBGE"
  ),
  link = case_when(
    variable %in% c("muni_code_6", "year") ~ "",
    
    variable == "UBS" ~ "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/estabbr.def",
    
    variable == "measles_cases" ~ "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/exantbr.def",
    
    variable %in% c("measles_deaths", "mumps_deaths", "whooping_deaths") ~ "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def",
    
    variable %in% c("pct_urban_1991", "pct_urban_2000", "pct_urban_2010") ~ "https://sidra.ibge.gov.br/tabela/202",
    
    variable %in% c("pct_low_inc_1991, pct_low_inc_2000, pct_low_inc_2010") ~ "https://datasus.saude.gov.br/trabalho-e-renda-censos-1991-2000-e-2010",
    
    variable %in% c("educ_pct_8_yrs_1991", "educ_pct_8_yrs_2000", "pct_complete_educ_2010") ~ "https://datasus.saude.gov.br/educacao-censos-1991-2000-e-2010",
    
    variable == "population" ~ "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/popsvs2024br.def",
    
    variable %in% c("muni_code", "muni_name", "state_name", "state_code", "region") ~ "https://www.ibge.gov.br/en/geosciences/territorial-organization/territorial-meshes/18890-municipal-mesh.html?edicao=24069"
  ),
  link2 = case_when(
    variable == "population" ~ "https://www.ibge.gov.br/en/statistics/social/labor/18448-estimates-of-resident-population-for-municipalities-and-federation-units.html?=&t=downloads",
    variable == "measles_cases" ~ "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinanwin/cnv/exantbr.def",
    variable %in% c("MMR2_coverage", "DTP_coverage") ~ "http://tabnet.datasus.gov.br/cgi/dhdat.exe?bd_pni/cpnibr.def"
  ),
  link3 = case_when(
    variable == "population" ~ "https://sidra.ibge.gov.br/tabela/475",
    variable %in% c("MMR2_coverage", "DTP_coverage") ~ "https://infoms.saude.gov.br/extensions/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA/SEIDIGI_DEMAS_VACINACAO_CALENDARIO_NACIONAL_COBERTURA_RESIDENCIA.html"
  ))

