library(tidyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(AER)
library(glmmTMB)
library(ggeffects)

load("~/Brazil-measles/data/clean_brazil_data.RData")


reg_data <- df %>% 
  filter(!muni_code %in% c(1504752,  # all established in 2013
                           4212650, 
                           4220000, 
                           4314548, 
                           5006275)
         & year >= 2008) %>%
  mutate(region = factor(region),
         state_name = factor(state_name),
         year = factor(year)) %>% 
  dplyr::select(muni_code, year, measles_cases, measles_deaths, coverage_lag2, coverage_lag3, 
                coverage_lag4, coverage_lag5, goal, pct_urban_2010, outbreak,
                pct_low_inc_2010, pct_complete_educ_2010, UBS_p100k, nurses_p100k, doctors_p100k, 
                state_name, population, region, coverage2)



# first attempt: cases per muni population estimated by lag2 coverage, % urban, % poverty
poiss <- glm(measles_cases ~ MMR1_lag4 + MMR1_lag5 + pct_urban_2010 * pct_low_inc_2010 + doctors_p100k
             + UBS_p100k + nurses_p100k + year + region + offset(log(population)),
    family = poisson(link = "log"),
    data = outbreak) 

# check overdispersion
dispersiontest(poiss,trafo=1)
# alpha much higher than 0 - overdispersion


outbreak <- df %>% 
  mutate(region = factor(region),
         state_name = factor(state_name),
         year = factor(year),
         goal = factor(goal)) %>% 
  group_by(muni_code) %>% 
  mutate(goal_lag2 = lag(goal, 2, order_by = year),
         goal_lag3 = lag(goal, 3, order_by = year),
         goal_lag4 = lag(goal, 4, order_by = year),
         goal_lag5 = lag(goal, 5, order_by = year),
         MMR1_lag2 = lag(MMR1_coverage, 2, order_by = year),
         MMR1_lag3 = lag(MMR1_coverage, 3, order_by = year),
         MMR1_lag4 = lag(MMR1_coverage, 4, order_by = year),
         MMR1_lag5 = lag(MMR1_coverage, 5, order_by = year)) %>%
  ungroup() %>% 
  filter(!is.na(outbreak)
         & coverage_lag2 <= 200
         & coverage_lag3 <= 200
         & coverage_lag4 <= 200
         & coverage_lag5 <= 200) %>%
  dplyr::select(muni_code, year, measles_cases, measles_deaths, pct_urban_2010, goal_lag2, goal_lag3, goal_lag4, goal_lag5,
                pct_low_inc_2010, pct_complete_educ_2010, UBS_p100k, nurses_p100k, doctors_p100k, 
                state_name, population, region, MMR1_lag2, MMR1_lag3, MMR1_lag4, MMR1_lag5,
                coverage_lag2, coverage_lag3, coverage_lag4, coverage_lag5)


nb <- glmmTMB(measles_cases ~ coverage_lag4 + MMR1_lag5 + pct_urban_2010 * pct_low_inc_2010 + doctors_p100k
        + UBS_p100k + nurses_p100k + year + region + offset(log(population)), 
        family = nbinom2, 
        data = outbreak)


nb <- glmmTMB(measles_cases ~ coverage_lag2 + coverage_lag3 + coverage_lag4 + 
                offset(log(population)), 
              family = nbinom2, 
              data = outbreak)


summary(nb)

marginal_effect <- predict_response(
  nb,
  terms = c("coverage_lag2", "coverage_lag3", "coverage_lag4"),
  ci_level = 0.95,
  type = "count",
  condition = c(population = 100000),
  interval = "confidence") #%>% 
  #rename(coverage_lag2 = x)

ggplot(marginal_effect, aes(x, predicted)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width=0.5), 
                  alpha = 0.9, size = 0.6) +
  scale_y_log10() 



  scale_color_manual(name = "Imprinted strain",
                     values = c(pH1N1 = "#ff6698",
                                H1N1_alpha = "#b70000", 
                                H1N1_beta = "#ff8900", 
                                H1N1_gamma = "#ECC905", 
                                H2N2 = "#95C90F", 
                                H3N2 = "#3683C3",
                                ambiguous = "#989898"),
                     labels = c(ambiguous = "Ambiguous",
                                pH1N1 = bquote(pH1N1["\u03b1"]  ),
                                H1N1_alpha = bquote(H1N1["\u03b1"]),
                                H1N1_beta = bquote(H1N1["\u03b2"]),
                                H1N1_gamma = bquote(H1N1["\u03b3"]))) +
  theme_bw() +
  labs(x = "Circulating strain", y = "Predicted influenza mortality rate") +
  theme(legend.justification = c(0, 1), legend.position = c(.01, 0.98),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.25),
        legend.title = element_text(size = 14),
        text = element_text(size = 16, family = "Helvetica"),
        plot.margin=unit(c(0.75, 0.25, 0.25, 0.25), "lines"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# negative binomial
summary_m1 <- glmmTMB(measles_cases ~ coverage_lag4 + pct_urban_2010 * pct_low_inc_2010 + doctors_p100k
        + UBS_p100k + nurses_p100k + year + region + outbreak + offset(log(population)), 
        family = nbinom2, 
        data = outbreak) %>% 
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










