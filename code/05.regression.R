library(tidyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(AER)
library(glmmTMB)
library(ggeffects)
library(performance)
library(broom.mixed)
library(purrr)

load("~/Brazil-measles/data/clean_brazil_data.RData")

# clean up data to work for analysis
reg_data <- df %>% 
  filter(!muni_code %in% c(1504752,  # all established in 2013 - data is a little funky?
                           4212650, 
                           4220000, 
                           4314548, 
                           5006275)
         & year %in% 2008:2020) %>% # complete data for 2008-2020 only
  mutate(region = factor(region),
         state_name = factor(state_name),
         year = factor(year)) #%>% 
#  dplyr::select(muni_code, year, measles_cases, measles_deaths, coverage_lag2, coverage_lag3, 
#                coverage_lag4, coverage_lag5, coverage_lag6, 
#                goal, pct_urban_2010, outbreak, pct_low_inc_2010, pct_complete_educ_2010, 
#                UBS_p100k, nurses_p100k, doctors_p100k, state_name, population, region, 
#                coverage2, sanitation, CDR, IMR, birth_rate, GDP_PC, GINI, MHDI, MHDI_E, 
#                MHDI_L, MHDI_I, birth_rate_lag2, birth_rate_lag3, birth_rate_lag4, 
#                birth_rate_lag5, birth_rate_lag6, birth_rate_lag7, birth_rate_lag8)



# trying poisson: estimating cases per muni pop with simple variables
poiss <- glm(measles_cases ~ coverage_lag5 + year + region + offset(log(population)),
             family = poisson(link = "log"),
             data = reg_data) 

# check overdispersion
dispersiontest(poiss,trafo=1)
# alpha much higher than 0 -> overdispersion -> do negative binomial
rm(poiss)



# simple negative binomial models for different coverage lags
nb_lag2 <- glmmTMB(measles_cases ~ coverage_lag2 + year + offset(log(population)), 
                      family = nbinom2, 
                      data = reg_data)
#summary(nb_lag2)

nb_lag3 <- glmmTMB(measles_cases ~ coverage_lag3 + year + offset(log(population)), 
                      family = nbinom2, 
                      data = reg_data)
#summary(nb_lag3)

nb_lag4 <- glmmTMB(measles_cases ~ coverage_lag4 + year + offset(log(population)), 
                      family = nbinom2, 
                      data = reg_data)
#summary(nb_lag4)

nb_lag5 <- glmmTMB(measles_cases ~ coverage_lag5 + year + offset(log(population)), 
                      family = nbinom2, 
                      data = reg_data)
#summary(nb_lag5)

nb_lag6 <- glmmTMB(measles_cases ~ coverage_lag6 + year + offset(log(population)), 
                      family = nbinom2, 
                      data = reg_data)
#summary(nb_lag6)


# compare coefficients for each lag
print(map_dfr(paste0("nb_lag", 2:6), function(model) {
  tidy(get(model)) %>%
    filter(term %in% paste0("coverage_lag", gsub("nb_lag", "", model)))
}))
# 2 year lagged (like coverage in 2010 for the year 2012) looks the biggest


# predict fxn for each lag
pred_fun <- function(model, lag) {
  lag_num <- gsub("lag", "", lag)
  predict_response(model,
                   terms = paste0("coverage_lag", lag_num, " [0:100]"),
                   type = "count",
                   condition = c(population = 100000, year = 2019),
                   interval = "confidence") %>%
    rename(coverage = x) %>% data.frame() %>% mutate(group = lag)
}


preds <- purrr::map2_dfr(list(nb_lag2, nb_lag3, nb_lag4, nb_lag5, nb_lag6),
                         c("lag2", "lag3", "lag4", "lag5", "lag6"),
                         ~pred_fun(.x, .y))

# plot
ggplot(preds, aes(x = coverage, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~group, ncol = 3)

# log scale
ggplot(preds, aes(x = coverage, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~group, ncol = 3) +
  scale_y_log10()

rm(nb_lag2, nb_lag3, nb_lag4, nb_lag5, nb_lag6)

# coverage lagged by 2 years (i.e coverage in 2008 for the year 2010) seems good to 
# go with based on these simple results and previous empirical work,
# but 5 also seems good to consider


# testing only including outbreak years
outbreak <- reg_data %>% 
  filter(outbreak != "0")

nb_ob_simple <- glmmTMB(measles_cases ~ coverage_lag2 + coverage_lag5 +
                       offset(log(population)), 
                     family = nbinom2, 
                     data = outbreak)
summary(nb_ob_simple)
# doesn;t seem to change much

# add fixed effects for year and region
nb_fe <- glmmTMB(measles_cases ~ coverage_lag2 + coverage_lag5 +
                year + region + offset(log(population)), 
                      family = nbinom2, 
                      data = reg_data)
summary(nb_fe)

# some performance checks - R2
null_mod <- glmmTMB(measles_cases ~ 1 + offset(log(population)), 
                      family = nbinom2, data = reg_data)

1 - (as.numeric(logLik(nb_fe)) / as.numeric(logLik(null_mod)))



# add some SES coefficients
nb <- glmmTMB(measles_cases ~ coverage_lag2 + coverage_lag5 + log(GDP_PC) + 
                pct_urban_2010 + MHDI + year + region + offset(log(population)), 
                 family = nbinom2, 
                 data = reg_data)
summary(nb)

1 - (as.numeric(logLik(nb)) / as.numeric(logLik(null_mod)))


predict_response(nb,
                 terms = c("coverage_lag5 [0:100]", "MHDI"), 
                 type = "count", 
                 condition = c(population = 100000, year = 2019), # ref as year w/ most cases
                 interval = "confidence") %>%
  rename(coverage_lag2 = x, MHDI = group) %>% 
  ggplot(aes(x = coverage_lag2, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = MHDI), alpha = 0.2) +
  geom_line(aes(color = MHDI)) +
  scale_y_log10()


# add some more coefficients
nb_full <- glmmTMB(measles_cases ~ coverage_lag2 + coverage_lag5 + log(GDP_PC) + 
                pct_urban_2010 + MHDI + UBS_p100k + doctors_p100k + sanitation +
                year + region + offset(log(population)), 
              family = nbinom2, 
              data = reg_data)
summary(nb_full)

1 - (as.numeric(logLik(nb_full)) / as.numeric(logLik(null_mod)))



# moving on to predicting coverage

lm <- lm(coverage2 ~ measles_cases + IMR + MHDI + log(GDP_PC) + pct_urban_2010 + 
            outbreak + region + UBS_p100k + CDR + sanitation + GINI + pct_complete_educ_2010,
              data = reg_data)

summary(lm)


pred_df <- reg_data[, c("coverage2", "measles_cases", "IMR", "MHDI", "GDP_PC", 
                                "pct_urban_2010", "outbreak", "region", "UBS_p100k", 
                                "CDR", "sanitation", "GINI", "pct_complete_educ_2010")] %>% 
  na.omit() %>% 
  mutate(fit = predict(lm, se.fit = TRUE)$fit,
         se = predict(lm, se.fit = TRUE)$se.fit,
         lower.ci = fit - 1.96 * se,
         upper.ci = fit + 1.96 * se)


ggplot(pred_df, aes(x = coverage2, y = fit, color = log(GDP_PC)), alpha = 0.25) +
  geom_pointrange(aes(ymin = lower.ci, ymax = upper.ci)) +
  geom_point()

# not entirely sure what to do with this





