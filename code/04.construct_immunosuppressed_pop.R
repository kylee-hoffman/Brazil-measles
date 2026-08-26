library(tidyverse)
library(ggeffects)
library(fixest)
library(ggplot2)
library(ggpubr)
library(gtools)
library(msm)
library(rsq)
library(MASS)
library(splines)

load("~/Brazil-measles/data/analysis_data.RData")

################################################################################
##
##
## finding best lagged measles incidence
##
##
################################################################################

cor.test(df$mv_incid_total, df$nm_mx)      # -0.0007988
cor.test(df$mv_incid_total_lag1, df$nm_mx) # -0.0033647
cor.test(df$mv_incid_total_lag2, df$nm_mx) #  0.0008388
cor.test(df$mv_incid_total_lag3, df$nm_mx) #  0.0127636
cor.test(df$mv_incid_total_lag4, df$nm_mx) #  0.0113815
cor.test(df$mv_incid_total_lag5, df$nm_mx) # -0.0008554

m0 <- glm.nb(nm_deaths ~ mv_incid_total + region + year + offset(log(pop_total)),
             data = df)

m1 <- glm.nb(nm_deaths ~ mv_incid_total_lag1 + region + year + offset(log(pop_total)),
             data = df)

m2 <- glm.nb(nm_deaths ~ mv_incid_total_lag2 + region + year + offset(log(pop_total)),
             data = df)

m3 <- glm.nb(nm_deaths ~ mv_incid_total_lag3 + region + year + offset(log(pop_total)),
             data = df)

m4 <- glm.nb(nm_deaths ~ mv_incid_total_lag4 + region + year + offset(log(pop_total)),
             data = df)

m5 <- glm.nb(nm_deaths ~ mv_incid_total_lag5 + region + year + offset(log(pop_total)),
             data = df)


model_parameters(m0, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald")[2, ]
model_parameters(m1, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald")[2, ]
model_parameters(m2, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald")[2, ]
model_parameters(m3, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald")[2, ]
model_parameters(m4, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald")[2, ]
model_parameters(m5, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald")[2, ]


AIC(m0, m1, m2, m3, m4, m5) %>% arrange(AIC)


lag0_pred <- predict_response(m0, 
                              terms = list(mv_incid_total = seq(0, 10, by = 1)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag1_pred <- predict_response(m1, 
                              terms = list(mv_incid_total_lag1 = seq(0, 10, by = 1)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag2_pred <- predict_response(m2, 
                              terms = list(mv_incid_total_lag2 = seq(0, 10, by = 1)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag3_pred <- predict_response(m3, 
                              terms = list(mv_incid_total_lag3 = seq(0, 10, by = 1)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag4_pred <- predict_response(m4, 
                              terms = list(mv_incid_total_lag4 = seq(0, 10, by = 1)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count", 
                              condition = c(pop_total = 1000)) %>% data.frame() 

lag5_pred <- predict_response(m5, 
                              terms = list(mv_incid_total_lag5 = seq(0, 10, by = 1)), 
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
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#87406f", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases per 1,000", y = "Non-measles ID deaths per 1,000") +
  coord_cartesian(ylim = c(0.15, 0.82), xlim = c(0.4, 9.6)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  theme + theme(axis.text.y = element_text(size = 12, color = "black"),
                axis.title.y = element_text(size = 12, color = "black"),
                axis.ticks.y = element_line(size = 0.3, color = "black")) +
  annotate("text", x = 0.6, y = 0.75, family = "Myriad Pro", size = 3.6, hjust = 0,
           label = paste("Pseudo R²:", format(round(pR2(m0)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m0) - AIC(m3), 1)))


lag1_p <- lag1_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#87406f", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 1 year prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 0.82), xlim = c(0.4, 9.6)) +
  theme +
  annotate("text", x = 0.6, y = 0.75, family = "Myriad Pro", size = 3.6, hjust = 0,
           label = paste("Pseudo R²:", format(round(pR2(m1)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m1) - AIC(m3), 1)))

lag2_p <- lag2_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#87406f", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 2 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 0.82), xlim = c(0.4, 9.6)) +
  theme +
  annotate("text", x = 0.6, y = 0.75, family = "Myriad Pro", size = 3.6, hjust = 0,
           label = paste("Pseudo R²:", format(round(pR2(m2)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m2) - AIC(m3), 1)))



lag3_p <- lag3_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#87406f", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 3 years prior", y = "Non-measles ID deaths per 1,000") +
  coord_cartesian(ylim = c(0.15, 0.82), xlim = c(0.4, 9.6)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  theme + theme(axis.text.y = element_text(size = 12, color = "black"),
                axis.title.y = element_text(size = 12, color = "black"),
                axis.ticks.y = element_line(size = 0.3, color = "black")) +
  annotate("text", x = 0.6, y = 0.75, family = "Myriad Pro", size = 3.6, hjust = 0,
           label = paste("Pseudo R²:", format(round(pR2(m3)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC: 0.0"))


lag4_p <- lag4_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#87406f", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 4 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 0.82), xlim = c(0.4, 9.6)) +
  theme +
  annotate("text", x = 0.6, y = 0.75, family = "Myriad Pro", size = 3.6, hjust = 0,
           label = paste("Pseudo R²:", format(round(pR2(m4)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m4) - AIC(m3), 1)))

lag5_p <- lag5_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#87406f", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 5 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 0.82), xlim = c(0.4, 9.6)) +
  theme +
  annotate("text", x = 0.6, y = 0.75, family = "Myriad Pro", size = 3.6, hjust = 0,
           label = paste("Pseudo R²:", format(round(pR2(m5)["McFadden"], 3), nsmall = 3), 
                         "\nΔAIC:", round(AIC(m5) - AIC(m3), 1)))


fig <- ggarrange(lag0_p, lag1_p, lag2_p, lag3_p, lag4_p, lag5_p, 
                 nrow = 2, ncol = 3,
                 widths = c(1.27, 1, 1, 
                            1.27, 1, 1))

ggsave(filename = "~/Brazil-measles/figures/nmid_v_lagged_cases.png", 
       plot = fig, height = 6, width = 7.25, units = "in", bg='white')


rm(list=ls())


################################################################################
## cured measles cases -> amnesia prevalence (1, 2, and 3-year durations)
##
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def
##
################################################################################
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/analysis_data.RData")


cases_cured <- read_cases("~/Brazil-measles/data/measles_cases/cured/total_01-06_sinanwin_cnv.csv", 
                          5, "total") %>% 
  merge(read_cases("~/Brazil-measles/data/measles_cases/cured/total_07-25_sinannet_cnv.csv", 
                   5, c("x2001", "x2006", "em_branco_ign", "total")),
        by = "municipio_de_residencia") %>% 
  mutate(x2004 = 0) %>% # no cases in 2004 so no original column
  pivot_longer(cols = -"municipio_de_residencia", values_to = "mv_cases_cured") %>% 
  mutate(year = as.numeric(substring(name, 2)),
         muni_code_6 = as.numeric(str_split_fixed(municipio_de_residencia, ' ', 2)[, 1])) %>% 
  dplyr::select(muni_code_6, year, mv_cases_cured)


amnesia <- cases_cured %>% 
  group_by(muni_code_6) %>% 
  mutate(amnesia_d1_ct = mv_cases_cured + dplyr::lag(mv_cases_cured, 1, order_by = year),
         amnesia_d2_ct = amnesia_d1_ct + dplyr::lag(mv_cases_cured, 2, order_by = year),
         amnesia_d3_ct = amnesia_d2_ct + dplyr::lag(mv_cases_cured, 3, order_by = year)) %>% 
  ungroup() %>% 
  merge(df %>% dplyr::select(muni_code_6, year, pop_total), by = c("muni_code_6", "year"), all.y = T) %>% 
  mutate(amnesia_prev_d1 = amnesia_d1_ct / pop_total * 1000,
         amnesia_prev_d2 = amnesia_d2_ct / pop_total * 1000,
         amnesia_prev_d3 = amnesia_d3_ct / pop_total * 1000) %>%
  dplyr::select(muni_code_6, year, amnesia_prev_d1, amnesia_prev_d2, amnesia_prev_d3)

df <- df %>% 
  merge(amnesia, by = c("muni_code_6", "year")) %>% 
  dplyr::select(region, state, muni_code_6, year, nm_deaths, nm_mx, 
                mv_cases_total, mv_incid_total, 
                #mv_incid_total_lag1, mv_incid_total_lag2, 
                #mv_incid_total_lag3, mv_incid_total_lag4, mv_incid_total_lag5,
                amnesia_prev_d1, amnesia_prev_d2, amnesia_prev_d3, susceptible_pop_u10,
                mcv_d1_cov, mcv_d1_cov_tc, mcv_d1_cov_tc_lag1, mcv_d1_cov_tc_lag2, mcv_d1_cov_tc_lag3, 
                mcv_d1_cov_tc_lag4, mcv_d1_cov_tc_lag5, cbr, cdr, gdp_pc, 
                clinics_pc, pop_den, prop_1to9, pop_total, pct_urban, poverty_rate, literacy_rate)


save(df, file = '~/Brazil-measles/data/analysis_data.RData')