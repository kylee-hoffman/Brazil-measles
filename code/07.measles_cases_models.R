library(tidyverse)
library(tsibble)
library(ggeffects)
#library(feasts)
library(ggplot2)
library(ggpubr)
#library(ggtime)
library(MASS)
library(glmmTMB)
library(broom.mixed)
library(pscl)
library(clubSandwich)
library(parameters)
library(performance)
library(legendry)
library(ggnewscale)
library(scales)

source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/analysis_data.RData")

################################################################################
##
##
## finding best lagged coverage
##
##
################################################################################

lag0_r <- format(round(cor(df$mcv_d1_cov_tc, df$mv_incid_total, method = "pearson"), 3), nsmall = 3)
lag1_r <- format(round(cor(df$mcv_d1_cov_tc_lag1, df$mv_incid_total, method = "pearson"), 3), nsmall = 3)
lag2_r <- format(round(cor(df$mcv_d1_cov_tc_lag2, df$mv_incid_total, method = "pearson"), 3), nsmall = 3)
lag3_r <- format(round(cor(df$mcv_d1_cov_tc_lag3, df$mv_incid_total, method = "pearson"), 3), nsmall = 3)
lag4_r <- format(round(cor(df$mcv_d1_cov_tc_lag4, df$mv_incid_total, method = "pearson"), 3), nsmall = 3)
lag5_r <- format(round(cor(df$mcv_d1_cov_tc_lag5, df$mv_incid_total, method = "pearson"), 3), nsmall = 3)

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

p0 <- ggplot(df %>% filter(mv_incid_total > 0), 
       aes(x = mcv_d1_cov_tc, y = mv_incid_total)) +
  geom_point(shape = 21, color = "#0a4466", stroke = 0.3, fill = "#accfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "MCV1 coverage", y = "Measles cases per 1,000") +
  scale_y_log10() +
  coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
  annotate("text", x = 5, y = 14, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag0_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p1 <- ggplot(df %>% filter(mv_incid_total > 0), 
             aes(x = mcv_d1_cov_tc_lag1, y = mv_incid_total)) +
  geom_point(shape = 21, color = "#0a4466", stroke = 0.3, fill = "#accfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "MCV1 coverage 1 year prior", y = NULL) +
  scale_y_log10() +
  coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
  annotate("text", x = 5, y = 14, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag1_r)) + theme

p2 <- ggplot(df %>% filter(mv_incid_total > 0), 
             aes(x = mcv_d1_cov_tc_lag2, y = mv_incid_total)) +
  geom_point(shape = 21, color = "#0a4466", stroke = 0.3, fill = "#accfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "MCV1 coverage 2 years prior", y = NULL) +
  scale_y_log10() +
  coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
  annotate("text", x = 5, y = 14, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag2_r)) + theme

p3 <- ggplot(df %>% filter(mv_incid_total > 0), 
             aes(x = mcv_d1_cov_tc_lag3, y = mv_incid_total)) +
  geom_point(shape = 21, color = "#0a4466", stroke = 0.3, fill = "#accfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "MCV1 coverage 3 years prior", y = "Measles cases per 1,000") +
  scale_y_log10() +
  coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
  annotate("text", x = 5, y = 14, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag3_r)) + theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p4 <- ggplot(df %>% filter(mv_incid_total > 0), 
             aes(x = mcv_d1_cov_tc_lag4, y = mv_incid_total)) +
  geom_point(shape = 21, color = "#0a4466", stroke = 0.3, fill = "#accfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "MCV1 coverage 4 years prior", y = NULL) +
  scale_y_log10() +
  coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
  annotate("text", x = 5, y = 14, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag4_r)) + theme

p5 <- ggplot(df %>% filter(mv_incid_total > 0), 
             aes(x = mcv_d1_cov_tc_lag5, y = mv_incid_total)) +
  geom_point(shape = 21, color = "#0a4466", stroke = 0.3, fill = "#accfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "MCV1 coverage 5 years prior", y = NULL) +
  scale_y_log10() +
  coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
  annotate("text", x = 5, y = 14, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag5_r)) + theme

fig <- ggarrange(p0, p1, p2, p3, p4, p5,
                 nrow = 2, ncol = 3,
                 widths = c(1.32, 1, 1))

ggsave(filename = "~/Brazil-measles/figures/cases_v_lagged_coverage.png", 
       plot = fig, height = 6, width = 7.25, units = "in", bg='white')

rm(list = ls())

################################################################################
##
##
## models estimating measles cases
##
##
################################################################################

load("~/Brazil-measles/data/analysis_data.RData")

# preliminary tests
# m_zi <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc +
#                    poverty_rate + pct_urban + cbr + region + year + 
#                  offset(log(pop_total)) | 1,
#                data = df, dist = "negbin")
# 
# m_zi_yr <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc +
#                         poverty_rate + pct_urban + cbr + region + year + 
#                         offset(log(pop_total)) | year,
#                       data = df, dist = "negbin")
 
m_zi_reg_yr <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + 
                          clinics_pc + poverty_rate + pct_urban + cbr + 
                          region + year + offset(log(pop_total)) | region + year,
                        data = df, dist = "negbin")
 
# AIC(m_zi, m_zi_yr, m_zi_reg_yr)
#             df    AIC
# m_zi        30 16188.26
# m_zi_yr     47 15166.99
# m_zi_reg_yr 51 14737.74
# 
# # default calculates R2 based on residual variance divided by total variance
# r2_zeroinflated(m_zi, method = "default") # 0.926
# r2_zeroinflated(m_zi_yr, method = "default") # 0.983
# r2_zeroinflated(m_zi_reg_yr, method = "default") # 0.996

m <- m_zi_reg_yr

rm(m_zi, m_zi_yr, m_zi_reg_yr)

################################################################################
##
## prediction plots (CIs not robust)
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
        axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 0.5, r = 2, b = 1.5, l = 0, unit = "mm"),
        legend.background = element_blank())

mcv_p <- predict_response(m,
                          terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 5)),
                          margin = "mean_mode",
                          ci_level = 0.95,
                          type = "count",
                          condition = c(pop_total = 1000)) %>%
  data.frame() %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a7c5e8", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 2 years prior", y = "Predicted measles cases per 1,000") +
  coord_cartesian(ylim = c(0.0005, 2.6), xlim = c(4, 96)) +
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))
  
#quantile(df$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
# 17.13 78.78 
pov_p <- predict_response(m, 
                          terms = list(poverty_rate = seq(17.13, 78.78, 
                                                          length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "count",
                          condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 2.6), xlim = c(19.5, 76.5)) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme

#quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 33.14 92.87
urban_p <- predict_response(m, 
                            terms = list(pct_urban = seq(33.14, 92.87, 
                                                         length.out = 20)), 
                            margin = "mean_mode", 
                            ci_level = 0.95,
                            type = "count",
                            condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% Urban", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 2.6), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) + theme


# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.09167864 0.54545004 
clinic_p <- predict_response(m, 
                              terms = list(clinics_pc = seq(0.09167864, 0.54545004,
                                                            length.out = 20)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "count",
                              condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#ACC196", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "UBS clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim =  c(0.0005, 2.6), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme


# quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 4688.17 41242.88 
gdp_p <- predict_response(m, 
                          terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                    length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "count",
                          condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#114B5F", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "GDP PC (1,000 BRL)", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 2.6), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme


# quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
# 9.170367 17.785892
cbr_p <- predict_response(m, 
                          terms = list(cbr = seq(9.170367, 17.785892, length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "count",
                          condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate per 1,000", y = "Predicted measles cases per 1,000") + 
  coord_cartesian(ylim = c(0.0005, 2.6), xlim = c(9.54, 17.42)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + 
  scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) + theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))


fig <- ggarrange(mcv_p, clinic_p, gdp_p,
          NULL, NULL, NULL, 
          cbr_p, urban_p, pov_p, 
          nrow = 3, ncol = 3, 
          widths = c(1.15, 1, 1), heights = c(1, 0.05, 1)) + 
  theme(plot.margin=margin(t = 0, r = -1, b = -0.5, l = 1, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/cases_preds.png", 
       plot = fig, height = 6, width = 8, units = "in", bg='white')


################################################################################
##
##
## restricting to years 2018-2023
##
##
################################################################################
df_2018 <- df %>% filter(as.numeric(as.character(year)) >= 2018)
nrow(df[df$mv_cases_total == 0, ]) / nrow(df) # 0.9867
nrow(df_2018[df_2018$mv_cases_total == 0, ]) / nrow(df_2018) # 0.9660


m_2018 <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc +
                poverty_rate + pct_urban + cbr + region + year + 
                offset(log(pop_total)) | region + year,
              data = df_2018, dist = "negbin")

################################################################################
##
## prediction plots (CIs not robust)
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
        axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 0.5, r = 3, b = 1.5, l = 0, unit = "mm"),
        legend.background = element_blank())

mcv_p_18 <- predict_response(m_2018,
                          terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 5)),
                          margin = "mean_mode",
                          ci_level = 0.95,
                          type = "count",
                          condition = c(pop_total = 1000)) %>%
  data.frame() %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a7c5e8", alpha = 0.7) +
  geom_line(linewidth = 0.3) +
  labs(x = "MCV1 coverage 2 years prior", y = "Predicted measles cases per 1,000") +
  coord_cartesian(ylim = c(0.0005, 0.0195), xlim = c(4, 96)) +
  scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.015, 0.02)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

# quantile(df_2018$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
# 14.97 68.11
pov_p_18 <- predict_response(m_2018, 
                             terms = list(poverty_rate = seq(14.97, 68.11, length.out = 10)), 
                             margin = "mean_mode", 
                             ci_level = 0.95,
                             type = "count",
                             condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 0.0195), xlim = c(17.15, 65.9)) + theme

# quantile(df_2018$cbr, probs = c(0.1, 0.9), na.rm=T)
# 8.840183 16.418256
cbr_p_18 <- predict_response(m_2018, 
                             terms = list(cbr = seq(8.840183, 16.418256, length.out = 10)), 
                             margin = "mean_mode", 
                             ci_level = 0.95,
                             type = "count",
                             condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate", y = "Predicted measles cases per 1,000") + 
  coord_cartesian(ylim = c(0.0005, 0.0195), xlim = c(9.16, 16.1)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))


# quantile(df_2018$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.1253918 0.5809225 
clinic_p_18 <- predict_response(m_2018, 
                 terms = list(clinics_pc = seq(0.1253918, 0.5809225, length.out = 10)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count",
                 condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#ACC196", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 0.0195), xlim = c(0.145, 0.561)) + theme


# quantile(df_2018$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 9198.624 58517.626 
gdp_p_18 <- predict_response(m_2018, 
                                terms = list(gdp_pc = seq(9198.624, 58517.626, length.out = 10)), 
                                margin = "mean_mode", 
                                ci_level = 0.95,
                                type = "count",
                                condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#114B5F", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000, 50000),
                     labels = c(10, 20, 30, 40, 50)) +
  labs(x = "GDP per capita (1,000 BRL)", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 0.0195), xlim = c(11300, 56400)) + theme

# quantile(df_2018$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 38.1 94.1 
urban_p_18 <- predict_response(m_2018, 
                                terms = list(pct_urban = seq(38.1, 94.1, length.out = 10)), 
                                margin = "mean_mode", 
                                ci_level = 0.95,
                                type = "count",
                                condition = c(pop_total = 1000)) %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% Urban", y = NULL) + 
  coord_cartesian(ylim = c(0.0005, 0.0195), xlim = c(40.5, 91.7)) + theme


fig <- ggarrange(mcv_p_18, clinic_p_18, gdp_p_18, 
          NULL, NULL, NULL,
          cbr_p_18, urban_p_18, pov_p_18, 
          nrow = 3, ncol = 3, widths = c(1.415, 1, 1), heights = c(1, 0.05, 1)) + 
  theme(plot.margin=margin(t = 0, r = -1, b = -0.5, l = 1, unit = "mm"))



ggsave(filename = "~/Brazil-measles/figures/cases_preds_2018-2023.png", 
       plot = fig, height = 6, width = 8, units = "in", bg='white')



################################################################################
##
## table
##
################################################################################
tab <- model_parameters(m, robust = TRUE, vcov_type = "HC2", exponentiate = T) %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")"),
         SE = round(SE, 3)) %>% 
  dplyr::select(Term = Parameter, IRR.1 = IRR, CI.1 = CI)


tab_2018 <- model_parameters(m_2018, robust = TRUE, vcov_type = "HC2", exponentiate = T) %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")"),
         SE = round(SE, 3)) %>% 
  dplyr::select(Term = Parameter, IRR.2 = IRR, CI.2 = CI)

tab_cases <- merge(tab, tab_2018, by = "Term", all = T) %>% 
  mutate(across(everything(), ~replace_na(.x, "")),
         CI.2 = paste(CI.2, "\\\\"))

write.table(tab_cases, file = "", quote = F, row.names = F, sep = "\t & ")

r2_zeroinflated(m, method = "default")
r2_zeroinflated(m_2018, method = "default")




################################################################################
##
##
## swapping in literacy for poverty
##
##
################################################################################
df_2018 <- df %>% filter(as.numeric(as.character(year)) >= 2018)

m_lit <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc +
                    literacy_rate + pct_urban + cbr + region + year + 
                    offset(log(pop_total)) | region + year,
                  data = df, dist = "negbin")

m_lit_2018 <- zeroinfl(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc +
                    literacy_rate + pct_urban + cbr + region + year + 
                    offset(log(pop_total)) | region + year,
                  data = df_2018, dist = "negbin")


tab_lit <- model_parameters(m_lit, robust = TRUE, vcov_type = "HC2", exponentiate = T) %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(format(round(Coefficient, 2), nsmall = 2), sig),
         CI = paste0("(", format(round(CI_low, 2), nsmall = 2), ", ", format(round(CI_high, 2), nsmall = 2), ")")) %>% 
  dplyr::select(Term = Parameter,  IRR, CI)


tab_lit_2018 <- model_parameters(m_lit_2018, robust = TRUE, vcov_type = "HC2", exponentiate = T) %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(format(round(Coefficient, 2), nsmall = 2), sig),
         CI = paste0("(", format(round(CI_low, 2), nsmall = 2), ", ", format(round(CI_high, 2), nsmall = 2), ")")) %>% 
  dplyr::select(Term = Parameter,  IRR2 = IRR, CI2 = CI)


tab <- merge(tab_lit, tab_lit_2018, by = "Term", all = T) %>% 
  mutate(across(everything(), ~replace_na(.x, "")),
         CI2 = paste(CI2, "\\\\"))


write.table(tab, file = "", row.names = F, quote = F, sep = " & \t")

r2_zeroinflated(m_lit, method = "default")
r2_zeroinflated(m_lit_2018, method = "default")

# ############################################################
# ##
# ## prediction plots (not working)
# ##
# ############################################################
# 
# # quantile(df$literacy_rate, probs = c(0.1, 0.9))
# # 70.67 95.30 
# predict_response(m_lit_region_int, 
#                         terms = list(literacy_rate = seq(70.67, 95.30, length.out = 10),
#                                      region = c("Norte",  "Nordeste", "Sudeste", "Sul", "Centro-oeste")), 
#                         margin = "mean_mode", 
#                         ci_level = 0.95,
#                         type = "count",
#                  condition = c(pop_total = 1000)) %>% 
#   as.data.frame() #%>% 
#   mutate(group = dplyr::recode(group, Norte = "North", Sul = "South", `Centro-oeste` = "Central-West",
#                                Nordeste = "Northeast", Sudeste = "Southeast")) %>% 
#   ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.3) +
#   geom_line() + labs(x = "Literacy rate", y = "Measles cases per 1,000", fill = "Region", color = "Region") +
#  # coord_cartesian(xlim = c(71.7, 94.3)) +
# #  scale_y_continuous(breaks = c(94, 96, 98, 100, 102),
# #                     labels = c("94%", "96%", "98%", "100%", "102%")) +
#   theme_bw(base_family = "Myriad Pro", base_size = 12) +
#   theme(palette.color.discrete = scales::pal_brewer(palette = "Dark2"),
#         text = element_text(color = "black"),
#         axis.text = element_text(size = 12, color = "black"),
#         axis.title.y = element_text(margin = margin(r = 8)),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position = c(0.85, 0.86),
#         legend.background = element_blank(),
#         panel.border = element_rect(color = "black", linewidth = 0.3),
#         axis.ticks = element_line(linewidth = 0.3, color = "black"),
#         plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"))
# 
# predict_response(m_lit_yr_int, 
#                         terms = list(literacy_rate = seq(70.67, 95.30, length.out = 10),
#                                      year = seq(2006, 2023)), 
#                         margin = "mean_mode", 
#                         ci_level = 0.95,
#                         type = "count",
#                  condition = c(pop_total = 1000)) #%>% data.frame() %>% 
#   ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
#   geom_line() + labs(x = "Literacy rate", y = "MCV dose 1 coverage", fill = "year", color = "year") +
#   scale_y_continuous(breaks = c(80, 85, 90, 95, 100),
#                      labels = c("80%", "85%", "90%", "95%", "100% ")) +
#   coord_cartesian(xlim = c(71.7, 94.3)) +
#   theme_bw(base_family = "Myriad Pro", base_size = 12) +
#   theme(palette.colour.discrete = scales::pal_viridis(),
#         text = element_text(color = "black"),
#         axis.text = element_text(size = 12, color = "black"),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_rect(color = "black", linewidth = 0.3),
#         axis.ticks = element_line(size = 0.3, color = "black"),
#         plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"),
#         legend.background = element_blank())
# # interesting
# 
# fig <- ggarrange(a_p, NULL, b_p, nrow = 1, widths = c(0.825, 0.08, 1))
# 
# ggsave(filename = "~/Brazil-measles/figures/cases_preds_lit_interaction.png", 
#        plot = fig, height = 6, width = 10, units = "in", bg='white')


