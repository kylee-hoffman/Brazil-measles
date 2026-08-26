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

################################################################################
##
##
## models estimating NMID mortality
##
##
################################################################################

source("~/brazil_measles/code/utils.R")
load("~/Brazil-measles/data/analysis_data.RData")

# model 1: controls for CDR and age structure with 2-way fixed effects
summary(m1_d1 <- glm.nb(nm_deaths ~ amnesia_prev_d1 + cdr + cbr + region + year + offset(log(pop_total)),
                          data = df))

summary(m1_d2 <- glm.nb(nm_deaths ~ amnesia_prev_d2 + cdr + cbr + region + year + offset(log(pop_total)),
                          data = df))

summary(m1_d3 <- glm.nb(nm_deaths ~ amnesia_prev_d3 + cdr + cbr + region + year + offset(log(pop_total)),
                          data = df))


tab_d1 <- model_parameters(m1_d1, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")) %>% 
  dplyr::select(Term = Parameter, IRR_d1 = IRR, CI_d1 = CI)

tab_d2 <- model_parameters(m1_d2, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")) %>% 
  dplyr::select(Term = Parameter, IRR_d2 = IRR, CI_d2 = CI)

tab_d3 <- model_parameters(m1_d3, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")) %>% 
  dplyr::select(Term = Parameter, IRR_d3 = IRR, CI_d3 = CI)

tab <- merge(tab_d1, tab_d2, by = "Term", all = T) %>% 
  merge(tab_d3, by = "Term", all = T) %>% 
  mutate(CI_d3 = paste(CI_d3, "\\\\"))

write.table(tab, file = "", quote = F, row.names = F, sep = "\t & ")

AIC(m1_d1, m1_d2, m1_d3)

AIC(m1_d1) - AIC(m1_d3)
AIC(m1_d2) - AIC(m1_d3)





##############################################################
##
## prediction plots with robust CIs
##
##############################################################

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
        plot.margin=margin(t = 1, r = 3, b = 1, l = 1, unit = "mm"),
        legend.position = c(0.3, 0.8),
        legend.background = element_blank())

p1 <- predict_response(m1_d1, 
                       terms = list(amnesia_prev_d1 = seq(0, 15, by = 1)), 
                       margin = "mean_mode", 
                       ci_level = 0.95,
                       type = "count", 
                       condition = c(pop_total = 1000),
                       vcov = "HC2") %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  ggplot(aes(x = amnesia_prev, y = predicted)) +
  geom_hline(yintercept = mean(df$nm_mx), linetype = "dashed", color = "#cc3d3d") + # 0.4706952
  geom_vline(xintercept = max(df$amnesia_prev_d1), linetype = "dashed", color = "#e38c3b") + # 9.30741
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, linewidth = 0, fill = "#b2a5c4") +
  geom_line(linewidth = 0.3) +
  coord_cartesian(ylim = c(0.4, 1.57), xlim = c(0.6, 14.4)) +
  labs(x = "Immune amnesia prevalence per 1,000\n(1-year duration)",
       y = "Adjusted NMID deaths per 1,000") + theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))

p2 <- predict_response(m1_d2, 
                 terms = list(amnesia_prev_d2 = seq(0, 15, by = 1)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count", 
                 condition = c(pop_total = 1000),
                 vcov = "HC2") %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  ggplot(aes(x = amnesia_prev, y = predicted)) +
  geom_hline(yintercept = mean(df$nm_mx), linetype = "dashed", color = "#cc3d3d") + # 0.4706952
  geom_vline(xintercept = max(df$amnesia_prev_d2), linetype = "dashed", color = "#e38c3b") + # 9.30741
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, linewidth = 0, fill = "#b2a5c4") +
  geom_line(linewidth = 0.3) +
  coord_cartesian(ylim = c(0.4, 1.57), xlim = c(0.6, 14.4)) +
  labs(x = "Immune amnesia prevalence per 1,000\n(2-year duration)",
       y = NULL) + theme

p3 <- predict_response(m1_d3, 
                 terms = list(amnesia_prev_d3 = seq(0, 15, by = 1)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count", 
                 condition = c(pop_total = 1000),
                 vcov = "HC2") %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  ggplot(aes(x = amnesia_prev, y = predicted)) +
  geom_hline(yintercept = mean(df$nm_mx), linetype = "dashed", color = "#cc3d3d") + # 0.4706952
  geom_vline(xintercept = max(df$amnesia_prev_d3), linetype = "dashed", color = "#e38c3b") + # 9.30741
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, linewidth = 0, fill = "#b2a5c4") +
  geom_line(linewidth = 0.3) +
  coord_cartesian(ylim = c(0.4, 1.57), xlim = c(0.6, 14.4)) +
  labs(x = "Immune amnesia prevalence per 1,000\n(3-year duration)",
       y = NULL) + theme

fig <- ggarrange(p1, p2, p3, nrow = 1, widths = c(1.15, 1, 1))


ggsave(filename = "~/Brazil-measles/figures/amnesia_preds.png", 
       plot = fig, height = 3, width = 9, units = "in", bg='white')


predict_response(m1_d3, 
                 terms = list(amnesia_prev_d3 = c(10, 15)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count", 
                 condition = c(pop_total = 1000),
                 vcov = "HC2")
# amnesia_prev_d3 | Predicted |   95% CI
# ----------------------------------------
# 10              |   0.71    | 0.54, 0.93
# 15              |   0.98    | 0.65, 1.48
# 
# Adjusted for:
#   *    cdr =     6.46
# *    cbr =    13.33
# * region = Nordeste
# *   year =     2006


rm(list = ls())


load("~/Brazil-measles/data/analysis_data.RData")

# model 2: interaction + controls for CDR and age structure + 2-way fixed effects
# summary(m2_d1 <- glm.nb(nm_deaths ~ amnesia_prev_d1 * log(gdp_pc) + cdr + cbr + 
#                           region + year + offset(log(pop_total)), data = df))
# 
# summary(m2_d1_v2 <- glm.nb(nm_deaths ~ amnesia_prev_d1 * poverty_rate + cdr + cbr + 
#                              region + year + offset(log(pop_total)), data = df))
# 
# AIC(m2_d1, m2_d1_v2)
summary(m2_d1 <- glm.nb(nm_deaths ~ amnesia_prev_d1 * poverty_rate + cdr + cbr + 
                          region + year + offset(log(pop_total)), data = df))

summary(m2_d2 <- glm.nb(nm_deaths ~ amnesia_prev_d2 * poverty_rate + cdr + cbr + 
                          region + year + offset(log(pop_total)), data = df))

summary(m2_d3 <- glm.nb(nm_deaths ~ amnesia_prev_d3 * poverty_rate + cdr + cbr + 
                          region + year + offset(log(pop_total)), data = df))

AIC(m2_d1, m2_d2, m2_d3)


tab_d1 <- model_parameters(m2_d1, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")) %>% 
  dplyr::select(Term = Parameter, IRR_d1 = IRR, CI_d1 = CI)

tab_d2 <- model_parameters(m2_d2, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")) %>% 
  dplyr::select(Term = Parameter, IRR_d2 = IRR, CI_d2 = CI)

tab_d3 <- model_parameters(m2_d3, robust = TRUE, vcov_type = "HC2", exponentiate = T, ci_method="wald") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                   labels = c("***", "**", "*", ".", "")),
         IRR = paste0(round(Coefficient, 3), sig),
         CI = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")) %>% 
  dplyr::select(Term = Parameter, IRR_d3 = IRR, CI_d3 = CI)

tab <- merge(tab_d1, tab_d2, by = "Term", all = T) %>% 
  merge(tab_d3, by = "Term", all = T) %>% 
  mutate(CI_d3 = paste(CI_d3, "\\\\"))

write.table(tab, file = "", quote = F, row.names = F, sep = "\t & ")

AIC(m2_d1) - AIC(m2_d3)
AIC(m2_d2) - AIC(m2_d3)


##################################################
##
## prediction plots
##
##################################################

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
        plot.margin=margin(t = 1, r = 3, b = 1, l = 1, unit = "mm"),
        legend.position = c(0.3, 0.8),
        legend.background = element_blank())


# quantile(df$poverty_rate, probs = c(0.1, 0.5, 0.9))
#  10%   50%   90% 
# 17.13 48.53 78.78 
fig <- predict_response(m2_d3, 
                         terms = list(amnesia_prev_d3 = seq(0, 15, by = 1),
                                      poverty_rate = c(17.13, 48.53, 78.78)), 
                         margin = "mean_mode", 
                         ci_level = 0.95,
                         type = "count", 
                         condition = c(pop_total = 1000)) %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  mutate(SES_percentile = case_when(group == 17.13 ~ "10th",
                                    group == 48.53 ~ "50th",
                                    group == 78.78 ~ "90th")) %>%
  ggplot(aes(x = amnesia_prev, y = predicted, color = SES_percentile, fill = SES_percentile)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(name = "Socioeconomic percentile", 
                     values = c(`10th` = "#D1603D",
                                `50th` = "#84A9C0",
                                `90th` = "#4B3F72"),
                     labels = c(`10th` = "10th    (Poverty rate: 17.2%)", 
                                `50th` = "50th    (Poverty rate: 48.5%)",
                                `90th` = "90th    (Poverty rate: 78.8%)")) +
  scale_fill_manual(name = "Socioeconomic percentile", 
                    values = scales::alpha(c(`10th` = "#D1603D",
                                             `50th` = "#84A9C0",
                                             `90th` = "#4B3F72"), 0.7),
                    labels = c(`10th` = "10th    (Poverty rate: 17.2%)", 
                               `50th` = "50th    (Poverty rate: 48.5%)",
                               `90th` = "90th    (Poverty rate: 78.8%)")) +
  labs(x = "Immune amnesia prevalence per 1,000",
       y = "Adjusted NMID deaths per 1,000") + 
  coord_cartesian(ylim = c(0.2, 10.8), xlim = c(0.6, 14.4)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"),
        legend.position = c(0.25, 0.75)) +
  annotate("rect", xmin = 0.35, xmax = 7.1, ymin = 6, ymax = 10.8, fill = "#f9f9f9", color = "black",
         linewidth=0.14)

ggsave(filename = "~/Brazil-measles/figures/amnesia_interaction_preds_d3.png", 
       plot = fig, height = 3, width = 5, units = "in", bg='white')


predict_response(m2_d3, 
                 terms = list(amnesia_prev_d3 = c(10, 15),
                              poverty_rate = c(17.13, 48.53, 78.78)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count", 
                 condition = c(pop_total = 1000))
# poverty_rate: 17.13
# amnesia_prev_d3 | Predicted 
# ---------------------------------------
# 10              | 0.11 [0.07,  0.19]
# 15              | 0.06 [0.03,  0.12]
# 
# poverty_rate: 48.53
# amnesia_prev_d3 | Predicted |      95% CI
# -----------------------------------------
# 10              | 0.56 [0.44,  0.70]
# 15              | 0.65 [0.46,  0.92]
# 
# poverty_rate: 78.78
# amnesia_prev_d3 | Predicted |      95% CI
# -----------------------------------------
# 10              | 2.59 [1.84,  3.65]
# 15              | 6.84 [4.08, 11.45]



p_d1 <- predict_response(m2_d1, 
                 terms = list(amnesia_prev_d1 = seq(0, 15, by = 1),
                              poverty_rate = c(17.13, 48.53, 78.78)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count", 
                 condition = c(pop_total = 1000)) %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  mutate(SES_percentile = case_when(group == 17.13 ~ "10th",
                                    group == 48.53 ~ "50th",
                                    group == 78.78 ~ "90th")) %>%
  ggplot(aes(x = amnesia_prev, y = predicted, color = SES_percentile, fill = SES_percentile)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(name = "Socioeconomic percentile", 
                     values = c(`10th` = "#D1603D",
                                `50th` = "#84A9C0",
                                `90th` = "#4B3F72"),
                     labels = c(`10th` = "10th    (Poverty rate: 17.2%)", 
                                `50th` = "50th    (Poverty rate: 48.5%)",
                                `90th` = "90th    (Poverty rate: 78.8%)")) +
  scale_fill_manual(name = "Socioeconomic percentile", 
                    values = scales::alpha(c(`10th` = "#D1603D",
                                             `50th` = "#84A9C0",
                                             `90th` = "#4B3F72"), 0.7),
                    labels = c(`10th` = "10th    (Poverty rate: 17.2%)", 
                               `50th` = "50th    (Poverty rate: 48.5%)",
                               `90th` = "90th    (Poverty rate: 78.8%)")) +
  labs(x = "Immune amnesia prevalence per 1,000\n(1-year duration)",
       y = "Adjusted NMID deaths per 1,000") + 
  coord_cartesian(ylim = c(0.2, 10.8), xlim = c(0.6, 14.4)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"),
        legend.position = c(0.4, 0.82))


p_d2 <- predict_response(m2_d2, 
                 terms = list(amnesia_prev_d2 = seq(0, 15, by = 1),
                              poverty_rate = c(17.13, 48.53, 78.78)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "count", 
                 condition = c(pop_total = 1000)) %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  mutate(SES_percentile = case_when(group == 17.13 ~ "10th",
                                    group == 48.53 ~ "50th",
                                    group == 78.78 ~ "90th")) %>%
  ggplot(aes(x = amnesia_prev, y = predicted, color = SES_percentile, fill = SES_percentile)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(name = "Socioeconomic percentile", 
                     values = c(`10th` = "#D1603D",
                                `50th` = "#84A9C0",
                                `90th` = "#4B3F72"),
                     labels = c(`10th` = "10th    (Poverty rate: 17.2%)", 
                                `50th` = "50th    (Poverty rate: 48.5%)",
                                `90th` = "90th    (Poverty rate: 78.8%)")) +
  scale_fill_manual(name = "Socioeconomic percentile", 
                    values = scales::alpha(c(`10th` = "#D1603D",
                                             `50th` = "#84A9C0",
                                             `90th` = "#4B3F72"), 0.7),
                    labels = c(`10th` = "10th    (Poverty rate: 17.2%)", 
                               `50th` = "50th    (Poverty rate: 48.5%)",
                               `90th` = "90th    (Poverty rate: 78.8%)")) +
  labs(x = "Immune amnesia prevalence per 1,000\n(2-year duration)",
       y = "Adjusted NMID deaths per 1,000") + 
  coord_cartesian(ylim = c(0.2, 10.8), xlim = c(0.6, 14.4)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"),
        legend.position = "none")



fig <- ggarrange(p_d1, p_d2, nrow = 1)

ggsave(filename = "~/Brazil-measles/figures/amnesia_interaction_preds_d1_d2.png", 
       plot = fig, height = 4, width = 8, units = "in", bg='white')
