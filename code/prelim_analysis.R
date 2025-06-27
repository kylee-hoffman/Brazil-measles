library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)

load("~/Brazil-measles/data/clean_brazil_data.RData")

# add rates and lagged coverage
data <- data_clean %>% 
  mutate(measles_cases_p100k = measles_cases / population * 100000,
         measles_deaths_p100k = measles_deaths / population * 100000,
         region = factor(region),
         state_name = factor(state_name),
         # let's do a summed vaccine coverage to have a single variable work for all years
         # monovalent vaccine: 1996-2003  
         # MMR: 2000-
         # MMR second dose data: 2013-
         coverage = case_when(
           year < 2000 ~ monovalent_coverage,
           year %in% 2000:2003 ~ pmax(monovalent_coverage, MMR1_coverage, na.rm = T),
           year %in% 2004:2012 ~ MMR1_coverage,
           TRUE ~ MMR2_coverage
         )) %>% 
  group_by(muni_code) %>% 
  mutate(covg_lag2 = lag(coverage, 2, order_by = year),
         covg_lag3 = lag(coverage, 3, order_by = year),
         covg_lag4 = lag(coverage, 4, order_by = year)) %>% 
  ungroup()


reg_data <- data %>% 
  dplyr::select(muni_code, year, measles_cases, covg_lag2, covg_lag3, pct_urban_2010, 
                pct_low_inc_2010, UBS, state_name, population, pct_complete_educ_2010,
                nurses, doctors) %>% 
  filter(year >= 2007 & !muni_code %in% c(1504752,
                                          4212650,
                                          4220000,
                                          4314548,
                                          5006275))

# first attempt: cases per muni population estimated by lag2 MMR2 coverage, % urban, % poverty
# no muni fixed effects yet
glm(measles_cases ~ covg_lag2 + pct_urban_2010 + pct_low_inc_2010 + doctors
    + UBS + state_name + factor(year) + offset(log(population)),
    family = poisson(link = "log"),
    data = reg_data) %>%
  summary()
# not awesome

# try neg binomial
glm.nb(measles_cases ~ 
         covg_lag2 + pct_urban_2010 + UBS + pct_complete_educ_2010 +
         doctors + state_name + offset(log(population)),
       data = reg_data) %>% 
  summary()


# mortality
glm(measles_deaths ~ measles_cases + covg_lag2 + factor(year) + state_name,
    offset = log(population),
    family = poisson(link = "log"),
    data = data) %>% 
  summary()
# also not awesome

# State-level
state_data <- data %>% 
  group_by(state_name, year) %>% 
  summarise(avg_MMR2_lag4 = mean(MMR2_covg_lag4, na.rm = T),
            cases = sum(measles_cases, na.rm = T),
            pop = sum(population, na.rm = T),
            region = unique(region))


glm(cases ~ avg_MMR2_lag4 + factor(year) + region,
    offset = log(pop),
    family = poisson(link = "log"),
    data = state_data) %>% 
  summary()

# region-level
region_data <- data %>% 
  group_by(region, year) %>% 
  summarise(avg_MMR2_lag2 = mean(MMR2_covg_lag2, na.rm = T),
            cases = sum(measles_cases, na.rm = T),
            pop = sum(population, na.rm = T))


glm(cases ~ avg_MMR2_lag2 + factor(year),
    offset = log(pop),
    family = poisson(link = "log"),
    data = region_data) %>% 
  summary()


# plotting

region_data %>% filter(year > 2014) %>% 
  ggplot(aes(color = region)) +
  geom_line(aes(x = year, y = avg_MMR2_lag2), na.rm = T)  +
  geom_line(aes(x = year, y = cases/pop*100000), na.rm = T)


state_data %>% filter(year > 2014) %>% 
  ggplot(aes(color = state_name)) +
  geom_line(aes(x = year, y = avg_MMR2_lag4), na.rm = T)  +
  geom_line(aes(x = year, y = cases/pop*100000), na.rm = T)

data %>% 
  ggplot(aes(x = pct_low_inc_2000, y = measles_cases/population)) +
  geom_point() +
  scale_y_log10()
  
data %>% 
  ggplot(aes(x = pct_low_inc_2000, y = MMR2_coverage)) +
  geom_point() +
  scale_y_log10()

region_data %>% 
  ggplot(aes(x = year, color = region)) +
  geom_line(aes(y = avg_MMR2_lag2)) +
  geom_line(aes(y = cases/pop*100000))


national_data <- data %>% 
  group_by(year) %>% 
  summarise(avg_MMR2_lag2 = mean(MMR2_covg_lag2, na.rm = T),
            avg_MMR2_covg = mean(MMR2_coverage, na.rm = T),
            cases = sum(measles_cases, na.rm = T),
            pop = sum(population, na.rm = T))


scale_factor <- max(national_data$avg_MMR2_covg, na.rm = TRUE) / 
  max(national_data$cases, na.rm = TRUE)

national_data %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_MMR2_covg), color = "purple") +
  geom_line(aes(y = cases * scale_factor), color = "red") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Measles Cases"))










