library(tidyverse)
library(tsibble)
library(ggeffects)
library(feasts)
library(ggplot2)
library(ggpubr)
library(ggtime)
library(MASS)
library(broom.mixed)

source("~/brazil_measles/code/utils.R")
load("~/brazil_measles/generated_data/muni-year_panel_02-24.RData")


################################################################################
##
##
## models estimating measles cases
##
##
################################################################################

df <- df %>% mutate(mcv_d1_cov_tc = pmin(df$mcv_d1_cov, 100),
                    mcv_d1_cov_tc_lag2 = pmin(df$mcv_d1_cov_lag2, 100)) # top coded to 100


df_16_24 <- df %>% filter(as.numeric(as.character(year)) >= 2018)


summary(m1 <- glm.nb(mv_cases_total ~ mcv_d1_cov_tc_lag2 + region + year + offset(log(pop_total)),
                     data = df_16_24))


summary(m2 <- glmmTMB(mv_cases_total ~ mcv_d1_cov_tc_lag2 + log(gdp_pc) + clinics_pc + cbr + pop_den + region + year + offset(log(pop_total)),
                      family = nbinom2, data = df))

tab <- tidy(m2) %>%
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