library(tidyverse)
library(tsibble)
library(ggeffects)
library(feasts)
library(ggplot2)
library(ggpubr)
library(ggtime)
library(MASS)
library(glmmTMB)
library(broom.mixed)
library(pscl)

source("~/brazil_measles/code/utils.R")

load("~/Brazil-measles/data/analysis_data.RData")

################################################################################
##
##
## finding best lagged coverage
##
##
################################################################################

cor.test(df$mcv_d1_cov_tc, df$mv_cases_total)      # -0.0072
cor.test(df$mcv_d1_cov_tc_lag1, df$mv_cases_total) # -0.0058
cor.test(df$mcv_d1_cov_tc_lag2, df$mv_cases_total) # -0.0072
cor.test(df$mcv_d1_cov_tc_lag3, df$mv_cases_total) # -0.0058
cor.test(df$mcv_d1_cov_tc_lag4, df$mv_cases_total) # -0.0015
cor.test(df$mcv_d1_cov_tc_lag5, df$mv_cases_total) # -0.0007

summary(m0 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

summary(m1 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag1 + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

summary(m2 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag2 + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

summary(m3 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag3 + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

summary(m4 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag4 + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

summary(m5 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag5 + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

# compare coefficients for each lag
summ0 <- summary(m0)
summ1 <- summary(m1)
summ2 <- summary(m2)
summ3 <- summary(m3)
summ4 <- summary(m4)
summ5 <- summary(m5)

summ0$coefficients$cond["mcv_d1_cov_tc", ]
summ1$coefficients$cond["mcv_d1_cov_tc_lag1", ]
summ2$coefficients$cond["mcv_d1_cov_tc_lag2", ]
summ3$coefficients$cond["mcv_d1_cov_tc_lag3", ]
summ4$coefficients$cond["mcv_d1_cov_tc_lag4", ]
summ5$coefficients$cond["mcv_d1_cov_tc_lag5", ]

AIC(m0, m1, m2, m3, m4, m5) %>% arrange(AIC)

lag0_pred <- predict_response(m0, 
                              terms = list(mcv_d1_cov_tc = seq(0, 100, by = 10)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag1_pred <- predict_response(m1, 
                              terms = list(mcv_d1_cov_tc_lag1 = seq(0, 100, by = 10)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag2_pred <- predict_response(m2, 
                              terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 10)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag3_pred <- predict_response(m3, 
                              terms = list(mcv_d1_cov_tc_lag3 = seq(0, 100, by = 10)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag4_pred <- predict_response(m4, 
                              terms = list(mcv_d1_cov_tc_lag4 = seq(0, 100, by = 10)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag5_pred <- predict_response(m5, 
                              terms = list(mcv_d1_cov_tc_lag5 = seq(0, 100, by = 10)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 
# Adjusted for:
# * region = Nordeste
# *   year =     2006

pR2(m0)["McFadden"]
pR2(m1)["McFadden"]
pR2(m2)["McFadden"]
pR2(m3)["McFadden"]
pR2(m4)["McFadden"]
pR2(m5)["McFadden"]

theme <- theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks.x = element_line(size = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 4, r = 3, b = 4, l = 3, unit = "mm"),
        legend.background = element_blank())


lag0_p <- lag0_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ccdbf0", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage", y = "Measles cases per 1,000") +
  coord_cartesian(ylim = c(0.0001, 0.00375), xlim = c(3.8, 96.2)) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.003, 0.004)) +
  theme + theme(axis.text.y = element_text(size = 12, color = "black"),
                axis.title.y = element_text(size = 12, color = "black"),
                axis.ticks.y = element_line(size = 0.3, color = "black")) +
  annotate("text", x = 97, y = 0.0035, family = "Myriad Pro", size = 3.6, hjust = 1,
           label = paste("Pseudo R²:", format(round(pR2(m0)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m0) - AIC(m2), 1)))



lag1_p <- lag1_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ccdbf0", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 1 year prior", y = NULL) +
  coord_cartesian(ylim = c(0.0001, 0.00375), xlim = c(3.8, 96.2)) +
  theme  +
  annotate("text", x = 97, y = 0.0035, family = "Myriad Pro", size = 3.6, hjust = 1,
           label = paste("Pseudo R²:", format(round(pR2(m1)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m1) - AIC(m2), 1)))

lag2_p <- lag2_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ccdbf0", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 2 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.0001, 0.00375), xlim = c(3.8, 96.2)) +
  theme +
  annotate("text", x = 97, y = 0.0035, family = "Myriad Pro", size = 3.6, hjust = 1,
           label = paste("Pseudo R²:", format(round(pR2(m2)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC: 0.0"))

  

lag3_p <- lag3_pred %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ccdbf0", alpha = 0.7) +
    geom_line(linewidth = 0.3) +
    labs(x = "MCV1 coverage 3 years prior", y = "Measles cases per 1,000") +
   coord_cartesian(ylim = c(0.0001, 0.00375), xlim = c(3.8, 96.2)) +
   scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.003)) +
   theme + theme(axis.text.y = element_text(size = 12, color = "black"),
                 axis.title.y = element_text(size = 12, color = "black"),
                 axis.ticks.y = element_line(size = 0.3, color = "black")) +
  annotate("text", x = 97, y = 0.0035, family = "Myriad Pro", size = 3.6, hjust = 1,
           label = paste("Pseudo R²:", format(round(pR2(m3)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m3) - AIC(m2), 1)))


lag4_p <- lag4_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ccdbf0", alpha = 0.8) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 4 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.0001, 0.00375), xlim = c(3.8, 96.2)) +
  theme +
  annotate("text", x = 97, y = 0.0035, family = "Myriad Pro", size = 3.6, hjust = 1,
           label = paste("Pseudo R²:", format(round(pR2(m4)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m4) - AIC(m2), 1)))

lag5_p <- lag5_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ccdbf0", alpha = 0.8) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 5 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.0001, 0.00375), xlim = c(3.8, 96.2)) +
  theme +
  annotate("text", x = 97, y = 0.0035, family = "Myriad Pro", size = 3.6, hjust = 1,
           label = paste("Pseudo R²:", format(round(pR2(m5)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m5) - AIC(m2), 1)))


fig <- ggarrange(lag0_p, lag1_p, lag2_p, lag3_p, lag4_p, lag5_p, 
                 nrow = 2, ncol = 3,
          widths = c(1.27, 1, 1, 
                     1.27, 1, 1))

ggsave(filename = "~/Brazil-measles/figures/cases_v_lagged_coverage.png", 
       plot = fig, height = 6, width = 7.25, units = "in", bg='white')


rm(list=ls())

################################################################################
##
##
## models estimating measles cases
##
##
################################################################################

load("~/Brazil-measles/data/analysis_data.RData")

summary(m <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + literacy_rate + poverty_rate + 
                        pct_urban + clinics_pc + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))


tab <- tidy(m) %>%
  mutate(sig = cut(p.value, 
             breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
             labels = c("***", "**", "*", ".", "")),
         estimate = paste0(round(estimate, 3), sig),
         SE = paste0("(", round(std.error, 3), ")")) %>% 
  dplyr::select(term, estimate, SE)

write.table(tab, file = "", quote = F, row.names = F, sep = "\t")

# quantile(df$mcv_d1_cov, probs = c(0.1, 0.9), na.rm=T)
# 75.000 153.811 
mcv_pred <- predict_response(m2, 
                             terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 5)), 
                             margin = "mean_mode", 
                             ci_level = 0.95,
                             type = "count", 
                             condition = c(pop_total = 1000)) %>% data.frame()

# quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 3377.45 37903.07 
gdp_pred <- predict_response(m2, 
                             terms = list(gdp_pc = seq(3377.45, 37903.07, length.out = 10)), 
                             margin = "mean_mode", 
                             ci_level = 0.95,
                             type = "count", 
                             condition = c(pop_total = 1000)) %>% data.frame()

# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.0927 0.5520
clinic_pred <- predict_response(m2, 
                                terms = list(clinics_pc = seq(0.0927, 0.5520, length.out = 20)), 
                                margin = "mean_mode", 
                                ci_level = 0.95,
                                type = "count", 
                                condition = c(pop_total = 1000)) %>% data.frame() 

# quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
# 9.1760 18.6304
cbr_pred <- predict_response(m2, 
                             terms = list(cbr = seq(9.176, 18.630, length.out = 20)), 
                             margin = "mean_mode", 
                             ci_level = 0.95,
                             type = "count", 
                             condition = c(pop_total = 1000)) %>% data.frame() 

# quantile(df$pop_den, probs = c(0.1, 0.9), na.rm=T)
# 4.3776 130.6577
pop_den_pred <- predict_response(m2, 
                                 terms = list(pop_den = seq(4.378, 130.658, length.out = 20)), 
                                 margin = "mean_mode", 
                                 ci_level = 0.95,
                                 type = "count", 
                                 condition = c(pop_total = 1000)) %>% data.frame() 



theme <- theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks.x = element_line(size = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 1, r = 3, b = 1, l = 1, unit = "mm"),
        legend.background = element_blank())

mcv_p <- mcv_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#faeaac", alpha = 0.7) +
  geom_line(linewidth = 0.5) +
  labs(x = "MCV1 coverage 2 years prior", y = NULL) + 
  coord_cartesian(ylim = c(0.0003, 0.0034), xlim = c(4, 96)) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.003)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

gdp_p <- gdp_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#e6d7f7", alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  labs(x = "GDP per capita", y = NULL) + 
  coord_cartesian(ylim = c(0.0003, 0.0034), xlim = c(4800, 36500)) +
  theme

cbr_p <- cbr_pred %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#f5baa6", alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  labs(x = "Crude birth rate", y = NULL) + 
  coord_cartesian(ylim = c(0.0003, 0.0034), xlim = c(9.55, 18.25)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

clinic_p <- clinic_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a3d6ca", alpha = 0.4) +
  geom_line(linewidth = 0.5) +
  labs(x = "UBS clinics per capita", y = NULL) + 
  coord_cartesian(ylim = c(0.0003, 0.0034), xlim = c(0.11, 0.535)) +
  theme


fig <- ggarrange(mcv_p, gdp_p, NULL, NULL, cbr_p, clinic_p, ncol = 2, nrow = 3, heights = c(1, 0.1, 1)) + 
  theme(plot.margin=margin(t = 1, r = 1, b = 1, l = 8, unit = "mm")) +
  annotate("text", x = -0.02, y = 0.5, angle = 90,
           label = "Adjusted measles incidence per 1,000", 
           family = "Myriad Pro")

ggsave(filename = "~/brazil_measles/figures/cases_preds.png", 
       plot = fig, height = 4, width = 7.25, units = "in", bg='white')