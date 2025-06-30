library(tidyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(AER)
library(glmmTMB)



load("~/Brazil-measles/data/clean_brazil_data.RData")


reg_data <- df %>% 
  filter(!muni_code %in% c(1504752,  # all established in 2013
                           4212650, 
                           4220000, 
                           4314548, 
                           5006275)) %>%
  mutate(region = factor(region),
         state_name = factor(state_name),
         year = factor(year)) %>% 
  dplyr::select(muni_code, year, measles_cases, measles_deaths, coverage_lag2, coverage_lag3, pct_urban_2010, 
                pct_low_inc_2010, pct_complete_educ_2010, UBS_p100k, nurses_p100k, doctors_p100k, 
                state_name, population, region, coverage2)



# first attempt: cases per muni population estimated by lag2 coverage, % urban, % poverty
poiss <- glm(measles_cases ~ coverage_lag2 + coverage_lag3 + pct_urban_2010 + pct_low_inc_2010 + doctors_p100k
    + UBS_p100k + nurses_p100k + year + state_name + offset(log(population)),
    family = poisson(link = "log"),
    data = reg_data) 

# check overdispersion
dispersiontest(poiss,trafo=1)
# alpha much higher than 0 - overdispersion

# negative binomial
summary_m1 <- glmmTMB(measles_cases ~ coverage_lag2 + pct_urban_2010 * pct_low_inc_2010 + doctors_p100k
        + UBS_p100k + nurses_p100k + year + region + offset(log(population)), 
        family = nbinom2, 
        data = reg_data) %>% 
  summary()

coefs <- summary_m1$coefficients$cond

# make a nice little df
irr.tab <- data.frame(IRR = round(exp(coefs[, "Estimate"]), 8),
                      lower95 = round(exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]), 5),
                      upper95 = round(exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]), 5),
                      p_value = round(coefs[, "Pr(>|z|)"], 9))





# mortality
glmmTMB(measles_deaths ~ measles_cases + coverage_lag2 + pct_urban_2010 * pct_low_inc_2010 + doctors_p100k
        + UBS_p100k + nurses_p100k + year + region + offset(log(population)), 
    family = nbinom2,
    data = reg_data) %>% 
  summary()






# state-level
state_data <- reg_data %>% 
  group_by(state_name, year) %>% 
  summarise(avg_coverage_lag2 = mean(coverage_lag2, na.rm = T),
            measles_cases = sum(measles_cases, na.rm = T),
            population = sum(population, na.rm = T),
            region = unique(region))

glmmTMB(measles_cases ~ avg_coverage_lag2 + year + region + offset(log(population)),
    family = nbinom2,
    data = state_data) %>% 
  summary()

# region-level
region_data <- df %>% 
  group_by(region, year) %>% 
  summarise(avg_coverage_lag2 = mean(coverage_lag2, na.rm = T),
            measles_cases = sum(measles_cases, na.rm = T),
            population = sum(population, na.rm = T))


glmmTMB(measles_cases ~ avg_coverage_lag2 + year + offset(log(population)),
    family = nbinom2,
    data = region_data) %>% 
  summary()


# plotting

region_data %>% 
  ggplot(aes(color = region)) +
  geom_line(aes(x = year, y = avg_coverage_lag2), na.rm = T)  +
  geom_line(aes(x = year, y = measles_cases/population*100000), na.rm = T)


state_data %>% filter(year > 2014) %>% 
  ggplot(aes(color = state_name)) +
  geom_line(aes(x = year, y = avg_coverage_lag2), na.rm = T)  +
  geom_line(aes(x = year, y = cases/pop*100000), na.rm = T)

reg_data %>% 
  ggplot(aes(x = pct_low_inc_2010, y = measles_cases/population)) +
  geom_point() +
  scale_y_log10()
  
df %>% 
  ggplot(aes(x = pct_low_inc_2010, y = coverage2)) +
  geom_point() +
  scale_y_log10()

region_data %>% 
  ggplot(aes(x = year, color = region)) +
  geom_line(aes(y = avg_MMR2_lag2)) +
  geom_line(aes(y = cases/pop*100000))


national_data <- reg_data %>% 
  group_by(year) %>% 
  summarise(avg_coverage_lag2 = mean(coverage_lag2, na.rm = T),
            avg_coverage = mean(coverage2, na.rm = T),
            cases = sum(measles_cases, na.rm = T),
            pop = sum(population, na.rm = T))


scale_factor <- max(national_data$avg_coverage, na.rm = TRUE) / 
  max(national_data$cases, na.rm = TRUE)

national_data %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = avg_coverage), color = "purple") +
  geom_line(aes(y = cases * scale_factor), color = "red") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Measles Cases"))










