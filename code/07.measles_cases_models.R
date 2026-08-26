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
library(clubSandwich)
library(parameters)
library(performance)

source("~/Brazil-measles/code/utils.R")

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

m0 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc + region + year + offset(log(pop_total)),
              family = nbinom2, data = df)

m1 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag1 + region + year + offset(log(pop_total)),
              family = nbinom2, data = df)

m2 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag2 + region + year + offset(log(pop_total)),
              family = nbinom2, data = df)

m3 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag3 + region + year + offset(log(pop_total)),
              family = nbinom2, data = df)

m4 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag4 + region + year + offset(log(pop_total)),
              family = nbinom2, data = df)

m5 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag5 + region + year + offset(log(pop_total)),
              family = nbinom2, data = df)


model_parameters(m0, robust = TRUE, vcov_type = "HC2", exponentiate = T)
model_parameters(m1, robust = TRUE, vcov_type = "HC2", exponentiate = T)
model_parameters(m2, robust = TRUE, vcov_type = "HC2", exponentiate = T)
model_parameters(m3, robust = TRUE, vcov_type = "HC2", exponentiate = T)
model_parameters(m4, robust = TRUE, vcov_type = "HC2", exponentiate = T)
model_parameters(m5, robust = TRUE, vcov_type = "HC2", exponentiate = T)


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

m <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + literacy_rate + 
                poverty_rate + pct_urban + clinics_pc + cbr + region + year + 
                offset(log(pop_total)) | 1 + offset(log(pop_total)),
              data = df, dist = "negbin")

tab <- model_parameters(m, robust = TRUE, vcov_type = "HC2", exponentiate = T) %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")"),
         SE = round(SE, 3)) %>% 
  dplyr::select(Term = Parameter, IRR, CI, SE)
  
write.table(tab, file = "", quote = F, row.names = F, sep = "\t & ")

# maybe try efron
pR2(m)["McFadden"]

  
################################################################################
##
## prediction plots (not robust?)
##
################################################################################
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
        plot.margin=margin(t = 3, r = 3, b = 3, l = 0, unit = "mm"),
        legend.background = element_blank())

mcv_p <- predict_response(m, 
                          terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 5)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "count", 
                         # vcov = "HC",
                          condition = c(pop_total = 1000)) %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a7c5e8", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 2 years prior", y = NULL) + 
  coord_cartesian(ylim = c(0.00006, 0.002), xlim = c(4, 96)) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.003)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))


#quantile(df$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
# 17.13 78.78 
pov_p <- predict_response(m, 
                          terms = list(poverty_rate = seq(17.13, 78.78, length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed",
                          condition = c(pop_total = 1000)) %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate", y = NULL) + 
  coord_cartesian(ylim = c(0.00006, 0.002), xlim = c(19.6, 76.3)) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme


#quantile(df$literacy_rate, probs = c(0.1, 0.9), na.rm=T)
# 70.67 95.30 
lit_p <- predict_response(m, 
                          terms = list(literacy_rate = seq(70.67, 95.30, 
                                                           length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          condition = c(pop_total = 1000)) %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Literacy rate", y = NULL) + 
  coord_cartesian(ylim = c(0.00006, 0.002), xlim = c(71.7, 94.3)) +
  scale_x_continuous(breaks = c(75, 85, 95)) + theme

# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.09167864 0.54545004 
clinic_p <- predict_response(m, 
                                terms = list(clinics_pc = seq(0.09167864, 0.54545004, length.out = 20)), 
                                margin = "mean_mode", 
                                ci_level = 0.95,
                                type = "count", 
                                condition = c(pop_total = 1000)) %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#ACC196", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "UBS clinics per capita", y = NULL) + 
  coord_cartesian(ylim = c(0.00006, 0.002), xlim = c(0.11, 0.527)) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.003)) +
  theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))


# quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 33.14 92.87
urban_p <- predict_response(m, 
                            terms = list(pct_urban = seq(33.14, 92.87, length.out = 20)), 
                            margin = "mean_mode", 
                            ci_level = 0.95,
                            type = "count", 
                            condition = c(pop_total = 1000)) %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#E5C1BD", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "% urban", y = NULL) + 
  coord_cartesian(ylim = c(0.00006, 0.002), xlim = c(35.5, 90.5)) +
  theme

# quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
#  9.170367 17.785892 
cbr_p <- predict_response(m, 
                          terms = list(cbr = seq(9.170367, 17.785892, length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "count", 
                          condition = c(pop_total = 1000)) %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#89bbc7", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(0.00006, 0.002), xlim = c(9.54, 17.43)) +
  theme


fig <- ggarrange(mcv_p, pov_p, lit_p, clinic_p, urban_p, cbr_p, nrow = 2, ncol = 3,
                 widths = c(1, 0.8, 0.8)) + 
  theme(plot.margin=margin(t = 1, r = 1, b = 1, l = 8, unit = "mm")) +
  annotate("text", x = -0.02, y = 0.5, angle = 90,
           label = "Adjusted measles incidence per 1,000", 
           family = "Myriad Pro")

ggsave(filename = "~/Brazil-measles/figures/cases_preds.png", 
       plot = fig, height = 4, width = 7.25, units = "in", bg='white')
