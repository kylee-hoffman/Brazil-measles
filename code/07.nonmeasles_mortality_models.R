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







summary(m2_d2 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d2 * log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

summary(m2_d3 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d3 * log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

# table of results
rr_m2_d1 <- rr_tab(m2_d1) %>% filter(!str_detect(Term, "Intercept|year"))
rr_m2_d2 <- rr_tab(m2_d2) %>% filter(!str_detect(Term, "Intercept|year"))
rr_m2_d3 <- rr_tab(m2_d3) %>% filter(!str_detect(Term, "Intercept|year"))

nobs(m2_d1)
nobs(m2_d2)
nobs(m2_d3)

rsq(m2_d1, adj = T)
rsq(m2_d2, adj = T)
rsq(m2_d3, adj = T)

write.table(rr_m2_d1, file = "", row.names = F, quote = F, sep = "\t")
write.table(rr_m2_d2, file = "", row.names = F, quote = F, sep = "\t")
write.table(rr_m2_d3, file = "", row.names = F, quote = F, sep = "\t")


rr_m3_d1 <- rr_tab(m3_d1) %>% filter(!str_detect(Term, "Intercept|year"))
rr_m3_d2 <- rr_tab(m3_d2) %>% filter(!str_detect(Term, "Intercept|year"))
rr_m3_d3 <- rr_tab(m3_d3) %>% filter(!str_detect(Term, "Intercept|year"))

nobs(m3_d1)
nobs(m3_d2)
nobs(m3_d3)

rsq(m3_d1, adj = T)
rsq(m3_d2, adj = T)
rsq(m3_d3, adj = T)

write.table(rr_m3_d1, file = "", row.names = F, quote = F, sep = "\t")
write.table(rr_m3_d2, file = "", row.names = F, quote = F, sep = "\t")
write.table(rr_m3_d3, file = "", row.names = F, quote = F, sep = "\t")


################################################################################
##
## prediction plots
##
################################################################################
mean(df$nm_mx_total)
max(df$amnesia_prev_total_d1)

p1 <- predict_response(m3_d1, 
                 terms = list(amnesia_prev_total_d1 = seq(0, 15, by = 1)), 
                        margin = "mean_mode", 
                        ci_level = 0.95,
                        type = "count", 
                        condition = c(pop_total = 1000)) %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x) %>%
  ggplot(aes(x = amnesia_prev, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#c3f3fa", alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0.440298, linetype = "dashed", color = "#eb5e34") +
  labs(x = "Immune amnesia prevalence per 1,000",
       y = "Adjusted NMID deaths per 1,000") + 
  coord_cartesian(ylim = c(0.4, 1.05), xlim = c(0.6, 14.4)) +
 # annotate("rect", xmin = 0.3, xmax = 8.6, ymin = 48.5, ymax = 76.5, fill = "#f9f9f9", color = "black",
  #         linewidth=0.14) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 1, r = 3, b = 1, l = 1, unit = "mm"),
        legend.position = c(0.3, 0.8),
        legend.background = element_blank())



ggsave(filename = "~/brazil_measles/figures/ses_interaction.png", 
       plot = fig, height = 3.5, width = 4, units = "in", bg='white')




pred <- predict_response(m3_d1, 
                         terms = list(amnesia_prev_total_d1 = seq(0, 15, by = 1),
                                      gdp_pc = c(3377.45, 11074.94, 37903.08)), 
                         margin = "mean_mode", 
                         ci_level = 0.95,
                         type = "count", 
                         condition = c(pop_total = 1000))

mean(df$amnesia_prev_total_d1)
mean(df$nm_mx_total)

# quantile(df$gdp_pc, probs = seq(0, 1, by = 0.1), na.rm = T)
#  0%        10%      20%       30%       40%      50%       60%       70%       80%       90%       100%
# -1459.83   3377.45  5170.32   6954.52   8808.16  11074.94  14107.70  18387.22  25036.57  37903.08  920828.36
p2 <- predict_response(m3_d1, 
                        terms = list(amnesia_prev_total_d1 = seq(0, 15, by = 1),
                                     gdp_pc = c(3377.45, 11074.94, 37903.08)), 
                        margin = "mean_mode", 
                        ci_level = 0.95,
                        type = "count", 
                        condition = c(pop_total = 1000)) %>% 
  data.frame() %>% dplyr::rename(amnesia_prev = x, gdp_pc = group) %>%
  mutate(gdp_percentile = case_when(gdp_pc == 3377.45 ~ "10th",
                                    gdp_pc == 11074.94 ~ "50th",
                                    gdp_pc == 37903.08 ~ "90th")) %>%
  ggplot(aes(x = amnesia_prev, y = predicted, color = gdp_percentile, fill = gdp_percentile)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(name = "Socioeconomic percentile", 
                     values = c(`10th` = "#D1603D",
                                `50th` = "#84A9C0",
                                `90th` = "#4B3F72")) +
  scale_fill_manual(name = "Socioeconomic percentile", 
                    values = scales::alpha(c(`10th` = "#D1603D",
                                             `50th` = "#84A9C0",
                                             `90th` = "#4B3F72"), 0.7)) +
  labs(x = "Immune amnesia prevalence per 1,000",
       y = "Adjusted NMID deaths per 1,000") + 
  coord_cartesian(ylim = c(1, 75), xlim = c(0.6, 14.4)) +
  annotate("rect", xmin = 0.25, xmax = 9.6, ymin = 44, ymax = 76.5, fill = "#f9f9f9", color = "black",
           linewidth=0.14) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 1, r = 2, b = 1, l = 1, unit = "mm"),
        legend.position = c(0.36, 0.77),
        legend.background = element_blank())

ggsave(filename = "~/brazil_measles/figures/ses_interaction.png", 
       plot = p2, height = 3.5, width = 4, units = "in", bg='white')


fig <- suppressWarnings(ggarrange(p1, NULL, p2, ncol = 3, widths = c(1, 0.06, 1))) +
  annotate("text", x = 0.02, y = 0.975, label = "A", family = "Myriad Pro Bold") +
  annotate("text", x = 0.54, y = 0.975, label = "B", family = "Myriad Pro Bold")



ggsave(filename = "~/brazil_measles/figures/preds.png", 
       plot = fig, height = 3, width = 7.25, units = "in", bg='white')


