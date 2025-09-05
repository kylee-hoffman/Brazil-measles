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
library(zoo)

# Calculating version of coverage variables that is top coded to 100.
df <- df %>%
  group_by(muni_code, year) %>%
  mutate(
    tc_coverage_dose1_lag1 = pmin(coverage_dose1_lag1, 100),
    tc_coverage_dose1_lag2 = pmin(coverage_dose1_lag2, 100),
    tc_coverage_dose1_lag3 = pmin(coverage_dose1_lag3, 100),
    tc_coverage_dose1_lag4 = pmin(coverage_dose1_lag4, 100),
    tc_coverage_dose1_lag5 = pmin(coverage_dose1_lag5, 100),
    tc_coverage_dose1_lag6 = pmin(coverage_dose1_lag6, 100),
    tc_coverage_dose1_lag7 = pmin(coverage_dose1_lag7, 100),
    tc_coverage_dose1_lag8 = pmin(coverage_dose1_lag8, 100),
    tc_coverage_dose1_lag9 = pmin(coverage_dose1_lag9, 100)) %>%
  ungroup()

# Noticed that there was lots of missingness. Checking where it's coming from.
sapply(df[c("births", "deaths_U6mo", "pop_one", "pop_two", "pop_three", "pop_four", "pop_five", 
            "pop_six", "pop_seven", "pop_eight", "pop_nine", "tc_coverage_dose1_lag1", "tc_coverage_dose1_lag2", 
            "tc_coverage_dose1_lag3", "tc_coverage_dose1_lag4", "tc_coverage_dose1_lag5", "tc_coverage_dose1_lag6", "tc_coverage_dose1_lag7",
            "tc_coverage_dose1_lag8", "tc_coverage_dose1_lag9")], function(x) sum(is.na(x)))

# Age-structured population data is missing for some municipality-year combos. Restricting data to municipality-year combos with  age-structured population data.
vars <- c("births", "deaths_U6mo", "pop_one", "pop_two", "pop_three", "pop_four", "pop_five", 
          "pop_six", "pop_seven", "pop_eight", "pop_nine")
df <- df[complete.cases(df[vars]), ]

# Version 1: simple susceptible population formula.
df <- df %>%
  mutate(across(c(births, deaths_U6mo, pop_one:pop_nine, 
                  tc_coverage_dose1_lag1:tc_coverage_dose1_lag9), 
                ~replace_na(., 0))) %>%
  mutate(susceptible_pop = (births - deaths_U6mo) + 
           pop_one*(100 - tc_coverage_dose1_lag1)/100 +
           pop_two*(100 - tc_coverage_dose1_lag2)/100 +
           pop_three*(100 - tc_coverage_dose1_lag3)/100 +
           pop_four*(100 - tc_coverage_dose1_lag4)/100 +
           pop_five*(100 - tc_coverage_dose1_lag5)/100 +
           pop_six*(100 - tc_coverage_dose1_lag6)/100 +
           pop_seven*(100 - tc_coverage_dose1_lag7)/100 +
           pop_eight*(100 - tc_coverage_dose1_lag8)/100 +
           pop_nine*(100 - tc_coverage_dose1_lag9)/100) %>%
  ungroup()

options(scipen = 999)
summary(df$susceptible_pop)

# Version 2: first version of susceptible population formula starting to account for infection-acquired immunity.

# Creating an indicator variable for whether there was a measles outbreak in a given year in a given municipality.
df <- df %>%
  mutate(any_cases = as.numeric(measles_cases > 0))

# Calculating susceptible population. If there was a measles outbreak in the year a child was born, we assume they have infection-acquired immunity.
df <- df %>%
  group_by(muni_code, year) %>%
  mutate(across(c(births, deaths_U6mo, pop_one:pop_nine, 
                  tc_coverage_dose1_lag1:tc_coverage_dose1_lag9), 
                ~replace_na(., 0))) %>%
  mutate(any_cases = as.numeric(any(measles_cases > 0, na.rm = TRUE)),
         susceptible_pop2 = ifelse(any_cases == 1, 0, births - deaths_U6mo) +  
           pop_one*(100 - tc_coverage_dose1_lag1)/100 +
           pop_two*(100 - tc_coverage_dose1_lag2)/100 +
           pop_three*(100 - tc_coverage_dose1_lag3)/100 +
           pop_four*(100 - tc_coverage_dose1_lag4)/100 +
           pop_five*(100 - tc_coverage_dose1_lag5)/100 +
           pop_six*(100 - tc_coverage_dose1_lag6)/100 +
           pop_seven*(100 - tc_coverage_dose1_lag7)/100 +
           pop_eight*(100 - tc_coverage_dose1_lag8)/100 +
           pop_nine*(100 - tc_coverage_dose1_lag9)/100) %>%
  ungroup()

summary(df$susceptible_pop2)

# Version 3: second version of susceptible population formula starting to account for infection-acquired immunity.
# Assuming that all measles cases happen among children under 10.

df <- df %>%
  group_by(muni_code) %>%
  arrange(year) %>%
  mutate(across(c(births, deaths_U6mo, pop_one:pop_nine, tc_coverage_dose1_lag1:tc_coverage_dose1_lag9, measles_cases), 
                ~replace_na(., 0))) %>%
  mutate(cumulative_10yr_cases = lag(rollapply(measles_cases, width = 10, FUN = sum, fill = 0, align = "right", partial = TRUE), default = 0),
         susceptible_pop3 = pmax(0, (births - deaths_U6mo) + 
                                   pop_one*(100 - tc_coverage_dose1_lag1)/100 +
                                   pop_two*(100 - tc_coverage_dose1_lag2)/100 +
                                   pop_three*(100 - tc_coverage_dose1_lag3)/100 +
                                   pop_four*(100 - tc_coverage_dose1_lag4)/100 +
                                   pop_five*(100 - tc_coverage_dose1_lag5)/100 +
                                   pop_six*(100 - tc_coverage_dose1_lag6)/100 +
                                   pop_seven*(100 - tc_coverage_dose1_lag7)/100 +
                                   pop_eight*(100 - tc_coverage_dose1_lag8)/100 +
                                   pop_nine*(100 - tc_coverage_dose1_lag9)/100 -
                                   cumulative_10yr_cases)) %>%
  ungroup()

summary(df$susceptible_pop3)

# Version 4: third version of susceptible population formula starting to account for infection-acquired immunity.
# Explicitly accounting for measles cases that happened among children under 10.

df <- df %>%
  group_by(muni_code) %>%
  arrange(year) %>%
  mutate(across(c(births, deaths_U6mo, pop_one:pop_nine, tc_coverage_dose1_lag1:tc_coverage_dose1_lag9, measles_cases, measles_cases_u10), 
                ~replace_na(., 0))) %>%
  mutate(cumulative_previous_u10_cases = lag(rollapply(measles_cases_u10, width = 10, FUN = sum, fill = 0, align = "right", partial = TRUE), default = 0),
         susceptible_pop4 = pmax(0, (births - deaths_U6mo) + 
                                   pop_one*(100 - tc_coverage_dose1_lag1)/100 +
                                   pop_two*(100 - tc_coverage_dose1_lag2)/100 +
                                   pop_three*(100 - tc_coverage_dose1_lag3)/100 +
                                   pop_four*(100 - tc_coverage_dose1_lag4)/100 +
                                   pop_five*(100 - tc_coverage_dose1_lag5)/100 +
                                   pop_six*(100 - tc_coverage_dose1_lag6)/100 +
                                   pop_seven*(100 - tc_coverage_dose1_lag7)/100 +
                                   pop_eight*(100 - tc_coverage_dose1_lag8)/100 +
                                   pop_nine*(100 - tc_coverage_dose1_lag9)/100 -
                                   cumulative_previous_u10_cases)) %>%
  ungroup()

summary(df$susceptible_pop4)

# Confirming things worked as expected.
identical(df$susceptible_pop, df$susceptible_pop2)
identical(df$susceptible_pop2, df$susceptible_pop3)
identical(df$susceptible_pop3, df$susceptible_pop4)

save(df, file = "clean_brazil_data.RData")

