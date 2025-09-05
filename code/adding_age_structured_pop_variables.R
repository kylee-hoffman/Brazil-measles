
library(tidyverse)
library(dplyr)
library(data.table)

rm(list=ls())
setwd("/Users/simoncooper/Documents/measles")
getwd()

## Adding live births.

# Live births.
live_births <- read_csv2("~/Documents/measles/live_births.csv",
                         skip = 3,        
                         n_max = 5598,
                         locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "births") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         births = as.numeric(births)) %>%
  filter(!is.na(muni_code))
  
## Adding age-structured population.

# Population under one.
pop_U1 <- read_csv2("~/Documents/measles/less_than_one.csv",
                         skip = 4,        
                         n_max = 5598,
                         locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_U1") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_U1 = as.numeric(pop_U1)) %>%
  filter(!is.na(muni_code))
  

# Population aged 1-2.
pop_one <- read_csv2("~/Documents/measles/one.csv",
                    skip = 4,        
                    n_max = 5598,
                    locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_one") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_one = as.numeric(pop_one)) %>%
  filter(!is.na(muni_code))
  

# Population aged 2-3.
pop_two <- read_csv2("~/Documents/measles/two.csv",
                     skip = 4,        
                     n_max = 5598,
                     locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_two") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_two = as.numeric(pop_two)) %>%
  filter(!is.na(muni_code))
  

# Population aged 3-4.
pop_three <- read_csv2("~/Documents/measles/three.csv",
                     skip = 4,        
                     n_max = 5598,
                     locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_three") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_three = as.numeric(pop_three)) %>%
  filter(!is.na(muni_code))
  

# Population aged 4-5.
pop_four <- read_csv2("~/Documents/measles/four.csv",
                       skip = 4,        
                       n_max = 5598,
                       locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_four") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_four = as.numeric(pop_four)) %>%
  filter(!is.na(muni_code))
  

# Population aged 5-6.
pop_five <- read_csv2("~/Documents/measles/five.csv",
                      skip = 4,        
                      n_max = 5598,
                      locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_five") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_five = as.numeric(pop_five)) %>%
  filter(!is.na(muni_code))
  

# Population aged 6-7.
pop_six <- read_csv2("~/Documents/measles/six.csv",
                      skip = 4,        
                      n_max = 5598,
                      locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_six") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_six = as.numeric(pop_six)) %>%
  filter(!is.na(muni_code))
  

# Population aged 7-8.
pop_seven <- read_csv2("~/Documents/measles/seven.csv",
                     skip = 4,        
                     n_max = 5598,
                     locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_seven") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_seven = as.numeric(pop_seven)) %>%
  filter(!is.na(muni_code))
  

# Population aged 8-9.
pop_eight <- read_csv2("~/Documents/measles/eight.csv",
                       skip = 4,        
                       n_max = 5598,
                       locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_eight") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_eight = as.numeric(pop_eight)) %>%
  filter(!is.na(muni_code))
  

# Population aged 9-10.
pop_nine <- read_csv2("~/Documents/measles/nine.csv",
                       skip = 4,        
                       n_max = 5598,
                       locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_nine") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_nine = as.numeric(pop_nine)) %>%
  filter(!is.na(muni_code))
  

# Population aged 10-11.
pop_ten <- read_csv2("~/Documents/measles/ten.csv",
                      skip = 4,        
                      n_max = 5598,
                      locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "pop_ten") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         pop_ten = as.numeric(pop_ten)) %>%
  filter(!is.na(muni_code))
  
## Adding infant deaths.

# Deaths before 6 months.
deaths_U6mo <- read_csv2("~/Documents/measles/deaths_before_6months.csv",
                     skip = 4,        
                     n_max = 5598,
                     locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "deaths_U6mo") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         deaths_U6mo = as.numeric(deaths_U6mo)) %>%
  filter(!is.na(muni_code))

## Adding measles cases under age 10.

# Measles cases under age 10 from 2001 through 2006.
cases_u10_2001to2006 <- read_csv2("/Users/simoncooper/Documents/measles/cases_u10_2001to2006.csv",
                              skip = 4,        
                              n_max = 5603,
                              locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "measles_cases_u10") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         measles_cases_u10 = as.numeric(case_when(
           measles_cases_u10 == "-" ~ "0",
           TRUE ~ measles_cases_u10))) %>%
  filter(!is.na(muni_code_6)) %>%
  select(muni_code_6, year, measles_cases_u10)

# Measles cases under age 10 from 2007 through 2024.
cases_u10_2007to2024 <- read_csv2("/Users/simoncooper/Documents/measles/cases_u10_2007to2024.csv",
                                  skip = 4,        
                                  n_max = 5603,
                                  locale = locale(encoding = "Latin1")) %>%
  rename(muni_code = 1) %>%
  mutate(muni_code = as.numeric(gsub("[^0-9]", "", muni_code)),
         muni_code_6 = as.numeric(substr(muni_code, 1, 6))) %>%
  pivot_longer(cols = -c(muni_code, muni_code_6),
               names_to = "year",
               values_to = "measles_cases_u10") %>%
  mutate(year = as.numeric(gsub("[^0-9]", "", year)),
         measles_cases_u10 = as.numeric(case_when(
           measles_cases_u10 == "-" ~ "0",
           TRUE ~ measles_cases_u10))) %>%
  filter(!is.na(muni_code_6),
         year >= 2007 & year <= 2024) %>% 
  select(muni_code_6, year, measles_cases_u10)

cases_u10_combined <- bind_rows(
  cases_u10_2001to2006,
  cases_u10_2007to2024
)

# Confirming muni_code and year uniquely identify all rows in all datasets.
datasets <- list(
  live_births = live_births,
  pop_U1 = pop_U1,
  pop_one = pop_one,
  pop_two = pop_two,
  pop_three = pop_three,
  pop_four = pop_four,
  pop_five = pop_five,
  pop_six = pop_six,
  pop_seven = pop_seven,
  pop_eight = pop_eight,
  pop_nine = pop_nine,
  deaths_U6mo = deaths_U6mo,
  cases_u10_combined = cases_u10_combined
  )
sapply(datasets, function(df) c(total = nrow(df), unique = nrow(unique(df[c("muni_code_6", "year")]))))

# Combining datasets into one.
# Using this method because it's more memory efficient.
dt_list <- map(datasets, as.data.table)
df_new <- dt_list[[1]]

for(i in 2:length(dt_list)) {
  merge_data <- dt_list[[i]]
  if("muni_code" %in% names(merge_data)) {
    merge_data[, muni_code := NULL]
  }
  
  df_new <- merge(df_new, merge_data, by = c("muni_code_6", "year"), all = TRUE)
  gc()
}

df_new <- as.data.frame(df_new)

# Combining df_new with df.
load("clean_brazil_data.RData")
df <- df %>%
  left_join(df_new, by = c("muni_code_6", "year"))

summary(df$pop_one)

# Confirming muni_code still looks as it should.
identical(df$muni_code.x, df$muni_code.y)
summary(df$muni_code.x)
summary(df$muni_code.y)
df <- df %>%
  mutate(muni_code = muni_code.x) %>%
  select(-muni_code.x)

# Removing duplicate variables.
vars_to_remove <- c("pop_U1.y", "pop_one.y", "pop_two.y", "pop_three.y", "pop_four.y", "pop_five.y", "pop_six.y", "pop_seven.y", "pop_eight.y", "pop_nine.y", "muni_code.y.y", "deaths_U6mo.y")
df <- df %>%
  select(-all_of(vars_to_remove))

df <- df %>%
  rename_with(~ gsub("\\.x$", "", .x))

save(df, file = "clean_brazil_data.RData")
