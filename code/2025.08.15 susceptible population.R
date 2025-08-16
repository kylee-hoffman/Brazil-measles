# Setting-up.
rm(list=ls())
setwd("/Users/simoncooper/Documents/measles")
getwd()
load("clean_brazil_data.RData")

library(tidyverse)
library(readr)
library(dplyr)
library(stargazer)
library(plm)
library(fixest)

# Calculating version of coverage variables that is top coded to 100.
df <- df %>%
  group_by(muni_code, year) %>%
  mutate(
    tc_coverage_dose2_lag1 = pmin(coverage_dose2_lag1, 100),
    tc_coverage_dose2_lag2 = pmin(coverage_dose2_lag2, 100),
    tc_coverage_dose2_lag3 = pmin(coverage_dose2_lag3, 100),
    tc_coverage_dose2_lag4 = pmin(coverage_dose2_lag4, 100),
    tc_coverage_dose2_lag5 = pmin(coverage_dose2_lag5, 100),
    tc_coverage_dose2_lag6 = pmin(coverage_dose2_lag6, 100),
    tc_coverage_dose2_lag7 = pmin(coverage_dose2_lag7, 100),
    tc_coverage_dose2_lag8 = pmin(coverage_dose2_lag8, 100),
    tc_coverage_dose2_lag9 = pmin(coverage_dose2_lag9, 100)) %>%
  ungroup()

# Version 1: simple susceptible population formula.
df <- df %>%
  mutate(susceptible_pop = (births - deaths_U6mo) + 
           pop_one*(100 - tc_coverage_dose2_lag1)/100 +
           pop_two*(100 - tc_coverage_dose2_lag2)/100 +
           pop_three*(100 - tc_coverage_dose2_lag3)/100 +
           pop_four*(100 - tc_coverage_dose2_lag4)/100 +
           pop_five*(100 - tc_coverage_dose2_lag5)/100 +
           pop_six*(100 - tc_coverage_dose2_lag6)/100 +
           pop_seven*(100 - tc_coverage_dose2_lag7)/100 +
           pop_eight*(100 - tc_coverage_dose2_lag8)/100 +
           pop_nine*(100 - tc_coverage_dose2_lag9)/100) %>%
  ungroup()

summary(df$susceptible_pop)

# Version 2: susceptible population formula starting to account for infection-acquired immunity.

# Creating an indicator variable for whether there was a measles outbreak in a given year in a given municipality.
df <- df %>%
  mutate(any_cases = as.numeric(measles_cases > 0))

# Calculating susceptible population. If there was a measles outbreak in the year a child was born, we assume they have infection-acquired immunity.
df <- df %>%
  group_by(muni_code, year) %>%
  mutate(any_cases = as.numeric(any(measles_cases > 0, na.rm = TRUE)),
    susceptible_pop2 = ifelse(any_cases == 1, 0, births - deaths_U6mo) +  
      pop_one*(100 - tc_coverage_dose2_lag1)/100 +
      pop_two*(100 - tc_coverage_dose2_lag2)/100 +
      pop_three*(100 - tc_coverage_dose2_lag3)/100 +
      pop_four*(100 - tc_coverage_dose2_lag4)/100 +
      pop_five*(100 - tc_coverage_dose2_lag5)/100 +
      pop_six*(100 - tc_coverage_dose2_lag6)/100 +
      pop_seven*(100 - tc_coverage_dose2_lag7)/100 +
      pop_eight*(100 - tc_coverage_dose2_lag8)/100 +
      pop_nine*(100 - tc_coverage_dose2_lag9)/100) %>%
  ungroup()

summary(df$susceptible_pop2)



