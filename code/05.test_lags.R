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
         year = factor(year))


# trying poisson: estimating cases per muni pop with simple variables
#poiss <- glm(measles_cases ~ coverage_lag5 + year + region + offset(log(population)),
#             family = poisson(link = "log"),
#             data = reg_data) 

# check overdispersion
#check_overdispersion(poiss)
# do negative binomial
#rm(poiss)


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

rm(nb_lag2, nb_lag3, nb_lag4, nb_lag5, nb_lag6, pred_fun)


# coverage lagged by 2 years (i.e coverage in 2008 for the year 2010) and 5 years look strongest
# see other vaccines:
mmr1_lag2 <- glmmTMB(measles_cases ~ MMR1_coverage_lag2 + year + offset(log(population)), 
                     family = nbinom2, 
                     data = reg_data)

mmr1_lag5 <- glmmTMB(measles_cases ~ MMR1_coverage_lag5 + year + offset(log(population)), 
                     family = nbinom2, 
                     data = reg_data)

mmr2_lag2 <- glmmTMB(measles_cases ~ MMR2_coverage_lag2 + year + offset(log(population)), 
                     family = nbinom2, 
                     data = reg_data)

mmr2_lag5 <- glmmTMB(measles_cases ~ MMR2_coverage_lag5 + year + offset(log(population)), 
                     family = nbinom2, 
                     data = reg_data)


mmr1_lag2_pred <- predict_response(mmr1_lag2,
                                   terms = "MMR1_coverage_lag2 [0:100]",
                                   type = "count",
                                   condition = c(population = 100000, year = 2019),
                                   interval = "confidence") %>%
  data.frame() %>% 
  rename(coverage = x) %>%
  mutate(group = "MMR1 lag 2")

mmr1_lag5_pred <- predict_response(mmr1_lag5,
                                   terms = "MMR1_coverage_lag5 [0:100]",
                                   type = "count",
                                   condition = c(population = 100000, year = 2019),
                                   interval = "confidence") %>%
  data.frame() %>% 
  rename(coverage = x) %>%
  mutate(group = "MMR1 lag 5")

mmr2_lag2_pred <- predict_response(mmr2_lag2,
                                   terms = "MMR2_coverage_lag2 [0:100]",
                                   type = "count",
                                   condition = c(population = 100000, year = 2019),
                                   interval = "confidence") %>%
  data.frame() %>% 
  rename(coverage = x) %>%
  mutate(group = "MMR2 lag 2")

mmr2_lag5_pred <- predict_response(mmr2_lag5,
                                   terms = "MMR2_coverage_lag5 [0:100]",
                                   type = "count",
                                   condition = c(population = 100000, year = 2019),
                                   interval = "confidence") %>%
  data.frame() %>% 
  rename(coverage = x) %>%
  mutate(group = "MMR2 lag 5")


preds <- bind_rows(mmr1_lag2_pred,
                   mmr1_lag5_pred,
                   mmr2_lag2_pred,
                   mmr2_lag5_pred)


ggplot(preds, aes(x = coverage, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~group, ncol = 2) +
  labs(x = "coverage",
       y = "predicted measles cases")


rm(mmr1_lag2_pred, mmr1_lag5_pred, mmr2_lag2_pred, mmr2_lag5_pred,
   mmr1_lag2, mmr1_lag5, mmr2_lag2, mmr2_lag5, preds, lag_models)


# let's just go with MMR2 lag2 for now
