library(tidyverse)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(msm)
library(jtools)
library(performance)
library(estimatr)
library(modelsummary)
library(car)

source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/analysis_data.RData")

################################################################################
##
## models estimating vaccination
##
################################################################################

# just region-year FEs
summary(m1 <- lm(mcv_d1_cov_tc ~ region + year, data = df))
# interaction
summary(m2 <- lm(mcv_d1_cov_tc ~ region * year, data = df))

AIC(m1, m2)

predict_response(m2, 
                 terms = list(year = seq(2006, 2023),
                              region = c("Norte",  "Nordeste", "Sudeste", "Sul", "Centro-oeste")), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
  geom_line() + labs(x = "year", y = "mcv coverage", fill = "region", color = "region")


# % of residents living in urban dwellings vs. pop density
# summary(a <- lm(mcv_d1_cov_tc ~ pct_urban + region + year, data = df))
# summary(b <- lm(mcv_d1_cov_tc ~ pop_den + region + year, data = df))
# AIC(a, b)
# rm(a, b)
# pop density is better, but i will use % urban for now


# add healthcare access and SES data

# summary(lm(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + poverty_rate + 
#              pct_urban + clinics_pc + cbr + region + year, data = df))
# # clinics may be colinear?
# summary(lm(cbr ~ clinics_pc + region + year, data = df))
# # more clinics = lower CBR by quite a lot
# 
# summary(a <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + poverty_rate + 
#                   pct_urban + clinics_pc + region + year, data = df))
# 
# summary(b <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + poverty_rate + 
#                   pct_urban + cbr + region + year, data = df))
# 
# summary(c <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + poverty_rate + 
#                   pct_urban + clinics_pc + cbr + region + year, data = df))
# 
# AIC(a, b, c)
# rm(a, b, c)
# cbr is better, but keeping clinics for now

lm(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + poverty_rate + 
          pct_urban + clinics_pc + cbr + region + year, data = df)
vif(m)

tidy(m <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + 
                             pct_urban + clinics_pc + cbr + region + year, 
                           data = df))

tidy(m_robust <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + 
                             pct_urban + clinics_pc + cbr + region + year, 
                           data = df, se_type = "HC2"))

coefs <- tidy(m_robust) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 3), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 3), sig),
         SE = as.character(round(std.error, 3))) %>% 
  dplyr::select(term, Estimate, CI, SE)

r2(m_robust)
# Observations: 99126
# R2: 0.114
# adj. R2: 0.114

write.table(coefs, file = "", row.names = F, quote = F, sep = " & \t")

############################################################################
##
## prediction plots 
##
############################################################################

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

#quantile(df$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
# 17.13 78.78 
pov_p <- predict_response(m_robust, 
                          terms = list(poverty_rate = seq(17.13, 78.78, 
                                                          length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate", y = "Predicted MCV1 coverage") + 
  coord_cartesian(ylim = c(96.7, 100), xlim = c(19.7, 76.2)) +
  scale_y_continuous(breaks = c(97, 98, 99, 100),
                     labels = c("97%", "98%", "99%", "100% ")) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))

#quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 33.14 92.87
urban_p <- predict_response(m_robust, 
                            terms = list(pct_urban = seq(33.14, 92.87, 
                                                         length.out = 20)), 
                            margin = "mean_mode", 
                            ci_level = 0.95,
                            type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% urban", y = NULL) + 
  coord_cartesian(ylim = c(96.7, 100), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) + theme


# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.09167864 0.54545004 
clinic_p <-  predict_response(m_robust, 
                              terms = list(clinics_pc = seq(0.09167864, 0.54545004,
                                                            length.out = 20)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#ACC196", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "UBS clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim =  c(96.7, 100), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme


# quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 4688.17 41242.88 
gdp_p <- predict_response(m_robust, 
                          terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                    length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#114B5F", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "GDP PC (1,000 BRL)", y = NULL) + 
  coord_cartesian(ylim = c(96.7, 100), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme


# quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
# 9.170367 17.785892
cbr_p <- predict_response(m_robust, 
                          terms = list(cbr = seq(9.170367, 17.785892, length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate", y = NULL) + 
  coord_cartesian(ylim = c(96.7, 100), xlim = c(9.54, 17.42)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme


fig <- ggarrange(pov_p, cbr_p, urban_p, clinic_p, gdp_p, 
          nrow = 1, widths = c(1.415, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0, r = -1, b = -0.5, l = 1, unit = "mm"))



ggsave(filename = "~/Brazil-measles/figures/vax_preds.png", 
       plot = fig, height = 2.3, width = 8, units = "in", bg='white')


###############################################################################
##
## restricting to years 2018-2023
##
###############################################################################

load("~/Brazil-measles/data/analysis_data.RData")

df_2018 <- df %>% filter(as.numeric(as.character(year)) >= 2018)

tidy(m_robust_2018 <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + 
                             pct_urban + clinics_pc + cbr + region + year, 
                           data = df_2018, se_type = "HC2"))

coefs_2018 <- tidy(m_robust_2018) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 3), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 3), sig),
         SE = as.character(round(std.error, 3))) %>% 
  dplyr::select(term, Estimate.2 = Estimate, CI.2 = CI)

vax_coefs <- merge(coefs, coefs_2018, by = "term", all = T) %>% 
  mutate(across(everything(), ~replace_na(.x, "")),
         CI.2 = paste(CI.2, "\\\\")) %>% 
  dplyr::select(-SE)

r2(m_robust)
r2(m_robust_2018)

write.table(vax_coefs, file = "", row.names = F, quote = F, sep = " & \t")

############################################################################
##
## prediction plots 
##
############################################################################

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

# quantile(df_2018$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
# 14.97 68.11
pov_p <- predict_response(m_robust_2018, 
                          terms = list(poverty_rate = seq(14.97, 68.11, 
                                                          length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate", y = "Predicted MCV1 coverage") + 
  coord_cartesian(ylim = c(89.5, 99.7), xlim = c(17.1, 65.9)) +
  scale_y_continuous(breaks = c(90, 92, 94, 96, 98, 100),
                     labels = c("90%", "92%", "94%", "96%", "98%", "100% ")) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))

# quantile(df_2018$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 38.1 94.1 
urban_p <- predict_response(m_robust_2018, 
                            terms = list(pct_urban = seq(38.1, 94.1, 
                                                         length.out = 20)), 
                            margin = "mean_mode", 
                            ci_level = 0.95,
                            type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% urban", y = NULL) + 
  coord_cartesian(ylim = c(89.5, 99.7), xlim = c(40.4, 91.8)) +
  scale_x_continuous(breaks = c(40, 60, 80)) + theme


# quantile(df_2018$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.1253918 0.5809225 
clinic_p <- predict_response(m_robust_2018, 
                              terms = list(clinics_pc = seq(0.1253918, 0.5809225,
                                                            length.out = 20)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#ACC196", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "UBS clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim =  c(89.5, 99.7), xlim = c(0.145, 0.562)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme


# quantile(df_2018$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 9198.624 58517.626 
gdp_p <- predict_response(m_robust_2018, 
                          terms = list(gdp_pc = seq(9198.624, 58517.626, 
                                                    length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#114B5F", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "GDP PC (1,000 BRL)", y = NULL) + 
  coord_cartesian(ylim = c(89.5, 99.7), xlim = c(11250, 56500)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000, 50000, 60000),
                     labels = c("10", "20", "30", "40", "50", "60")) + theme


# quantile(df_2018$cbr, probs = c(0.1, 0.9), na.rm=T)
# 8.840183 16.418256
cbr_p <- predict_response(m_robust_2018, 
                          terms = list(cbr = seq(8.840183, 16.418256, length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate", y = NULL) + 
  coord_cartesian(ylim = c(89.5, 99.7), xlim = c(9.15, 16.11)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme


fig <- ggarrange(pov_p, cbr_p, urban_p, clinic_p, gdp_p, 
                 nrow = 1, widths = c(1.415, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0.25, r = -1.5, b = -0.5, l = 0.5, unit = "mm"))



ggsave(filename = "~/Brazil-measles/figures/vax_preds_2018-2023.png", 
       plot = fig, height = 2.3, width = 8, units = "in", bg='white')



#############################################################################
##
## trying literacy rates
##
#############################################################################

tidy(m_robust_lit <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + 
                                 pct_urban + clinics_pc + cbr + region + year, 
                               data = df, se_type = "HC2"))

coefs_lit <- tidy(m_robust_lit) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.1 = Estimate, CI.1 = CI)
# literacy rate is unexpected

a <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + pct_urban + clinics_pc + cbr + 
                 literacy_rate * region + year, 
               data = df, se_type = "HC2")

b <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + 
                 pct_urban + clinics_pc + cbr + literacy_rate * year + region,
               data = df, se_type = "HC2")


coefs_a <- tidy(a) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.2 = Estimate, CI.2 = CI)

coefs_b <- tidy(b) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.3 = Estimate, CI.3 = CI)



coefs <- merge(coefs_lit, coefs_a, by = "term", all = T) %>%
  merge(coefs_b, by = "term", all = T) %>% 
  mutate(CI.3 = paste(CI.3, "\\\\"))

write.table(coefs, file = "", row.names = F, quote = F, sep = " & \t")
r2(m_robust_lit)
r2(a)
r2(b)

a_p <- predict_response(a, 
                        terms = list(literacy_rate = seq(70.67, 95.30, length.out = 10),
                                     region = c("Norte",  "Nordeste", "Sudeste", "Sul", "Centro-oeste")), 
                        margin = "mean_mode", 
                        ci_level = 0.95,
                        type = "fixed") %>% 
  as.data.frame() %>% 
  mutate(group = dplyr::recode(group, Norte = "North", Sul = "South", `Centro-oeste` = "Central-West",
                               Nordeste = "Northeast", Sudeste = "Southeast")) %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.3) +
  geom_line() + labs(x = "Literacy rate", y = "MCV dose 1 coverage", fill = "Region", color = "Region") +
  coord_cartesian(xlim = c(71.7, 94.3)) +
  scale_y_continuous(breaks = c(94, 96, 98, 100, 102),
                     labels = c("94%", "96%", "98%", "100%", "102%")) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(palette.color.discrete = scales::pal_brewer(palette = "Dark2"),
        text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.85, 0.86),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"))

b_p <- predict_response(b, 
                        terms = list(literacy_rate = seq(70.67, 95.30, length.out = 10),
                                     year = seq(2006, 2023)), 
                        margin = "mean_mode", 
                        ci_level = 0.95,
                        type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
  geom_line() + labs(x = "Literacy rate", y = "MCV dose 1 coverage", fill = "year", color = "year") +
  scale_y_continuous(breaks = c(80, 85, 90, 95, 100),
                     labels = c("80%", "85%", "90%", "95%", "100% ")) +
  coord_cartesian(xlim = c(71.7, 94.3)) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(palette.colour.discrete = scales::pal_viridis(),
        text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"),
        legend.background = element_blank())
# interesting

fig <- ggarrange(a_p, NULL, b_p, nrow = 1, widths = c(0.825, 0.08, 1))

ggsave(filename = "~/Brazil-measles/figures/lit_interaction.png", 
       plot = fig, height = 6, width = 10, units = "in", bg='white')





############################################################
## literacy for 2018-2023 only
############################################################
tidy(m_robust_lit_2018 <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + 
                                 pct_urban + clinics_pc + cbr + region + year, 
                               data = df_2018, se_type = "HC2"))

a_2018 <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + pct_urban + clinics_pc + cbr + 
                 literacy_rate * region + year, 
               data = df_2018, se_type = "HC2")

b_2018 <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + 
                 pct_urban + clinics_pc + cbr + literacy_rate * year + region,
               data = df_2018, se_type = "HC2")


coefs_lit_2018 <- tidy(m_robust_lit_2018) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.1 = Estimate, CI.1 = CI)

coefs_a_2018 <- tidy(a_2018) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.2 = Estimate, CI.2 = CI)

coefs_b_2018 <- tidy(b_2018) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.3 = Estimate, CI.3 = CI)



coefs_2018 <- merge(coefs_lit_2018, coefs_a_2018, by = "term", all = T) %>%
  merge(coefs_b_2018, by = "term", all = T) %>% 
  mutate(across(everything(), ~replace_na(.x, "")),
         CI.3 = paste(CI.3, "\\\\"))

write.table(coefs_2018, file = "", row.names = F, quote = F, sep = " & \t")

r2(m_robust_lit_2018)
r2(a_2018)
r2(b_2018)

#quantile(df_2018$literacy_rate, probs = c(0.1, 0.9))
#75.00 96.17
a_2018_p <- predict_response(a_2018, 
                        terms = list(literacy_rate = seq(75.00, 96.17, length.out = 10),
                                     region = c("Norte",  "Nordeste", "Sudeste", "Sul", "Centro-oeste")), 
                        margin = "mean_mode", 
                        ci_level = 0.95,
                        type = "fixed") %>% 
  as.data.frame() %>% 
  mutate(group = dplyr::recode(group, Norte = "North", Sul = "South", `Centro-oeste` = "Central-West",
                               Nordeste = "Northeast", Sudeste = "Southeast")) %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.3) +
  geom_line() + labs(x = "Literacy rate", y = "MCV dose 1 coverage", fill = "Region", color = "Region") +
  coord_cartesian(xlim = c(75.9, 95.3)) +
  scale_y_continuous(breaks = c(85, 90, 95, 100),
                     labels = c("85%", "90%", "95%", "100%")) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(palette.color.discrete = scales::pal_brewer(palette = "Dark2"),
        text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title.y = element_text(margin = margin(r = 8)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.84, 0.82),
        legend.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"))

b_2018_p <- predict_response(b_2018, 
                        terms = list(literacy_rate = seq(75.00, 96.17, length.out = 10),
                                     year = seq(2018, 2023)), 
                        margin = "mean_mode", 
                        ci_level = 0.95,
                        type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
  geom_line() + labs(x = "Literacy rate", y = "MCV dose 1 coverage", fill = "year", color = "year") +
  scale_y_continuous(breaks = c(80, 85, 90, 95, 100),
                     labels = c("80%", "85%", "90%", "95%", "100% ")) +
  coord_cartesian(xlim = c(75.9, 95.3)) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(palette.colour.discrete = scales::pal_viridis(),
        text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title.y = element_text(margin = margin(0,5,0,0, "mm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"),
        legend.background = element_blank())

fig <- ggarrange(a_2018_p, NULL, b_2018_p, nrow = 1, widths = c(0.825, 0.08, 1))

ggsave(filename = "~/Brazil-measles/figures/lit_interaction_2018-2023.png", 
       plot = fig, height = 6, width = 10, units = "in", bg='white')


# all 4
fig <- ggarrange(a_p + theme(legend.position = c(0.84, 0.82)), NULL, b_p,
                 NULL, NULL, NULL,
                 a_2018_p, NULL, b_2018_p, 
                 nrow = 3, ncol = 3, 
                 widths = c(0.825, 0.08, 1), heights = c(1, 0.07, 1)) +
  theme(plot.margin=margin(t = 7, r = -2, b = 0.2, l = 0.5, unit = "mm")) +
  annotate("text", x = 0.5, y = 1.015, size = 5, label = "Full models (2006-2023)", family = "Myriad Pro") +
  annotate("text", x = 0.5, y = 0.5, size = 5, label = "Reduced models (2018-2023)", family = "Myriad Pro")

ggsave(filename = "~/Brazil-measles/figures/lit_interaction_2x2.png", 
       plot = fig, height = 10, width = 9, units = "in", bg='white')




# interacting literacy and urbanicity
c <- lm_robust(mcv_d1_cov_tc ~ log(gdp_pc) + clinics_pc + cbr + 
                 literacy_rate * pct_urban + year + region,
               data = df, se_type = "HC2")

coefs_c <- tidy(c) %>%
  mutate(sig = cut(p.value, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(conf.low, 2), ", ", round(conf.high, 3), ")"),
         Estimate = paste0(round(estimate, 2), sig),
         SE = as.character(round(std.error, 2))) %>% 
  dplyr::select(term, Estimate.4 = Estimate, CI.4 = CI)

predict_response(c, 
                 terms = list(literacy_rate = seq(70.67, 95.30, length.out = 10),
                              pct_urban = c(33.14, 92.87)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
  geom_line() + labs(x = "Literacy rate", y = "MCV dose 1 coverage", fill = "%Urban", color = "%Urban") +
  scale_y_continuous(breaks = c(80, 85, 90, 95, 100),
                     labels = c("80%", "85%", "90%", "95%", "100% ")) +
  coord_cartesian(xlim = c(71.7, 94.3)) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(palette.colour.discrete = scales::pal_viridis(),
        text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 0.5, r = 1, b = 1, l = 0.5, unit = "mm"),
        legend.background = element_blank())

