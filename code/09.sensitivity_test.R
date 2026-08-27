library(tidyverse)
library(ggplot2)
library(janitor)
library(tsibble)
library(ggeffects)
library(feasts)

source("~/Brazil-measles/code/utils.R")

load('~/Brazil-measles/data/analysis_data.RData')


# Deaths by Residence by Year of Death according to Region,
# ICD-10 Category: C25 Malignant neoplasm of the pancreas,
# Period: 2000-2024
panc <- read_datasus("~/Brazil-measles/data/mortality/pancreatic_cancer_sim_cnv.csv",
             line_skip = 4, drop_cols = "total", values_to = "pancreatic_deaths") 


# Deaths by Residence by Year of Death according to Region.
# ICD-10 Group: 
# Malignant neoplasms of the lip, 
# oral cavity and pharynx, 
# Malignant neoplasms of the digestive organs,
# Malignant neoplasms of the respiratory tract and intrathoracic organs,
# Malignant neoplasms of bones and articular cartilage,
# Melanoma and other malignant neoplasms of the skin, 
# Malignant neoplasms of mesothelial tissue and soft tissues, 
# Malignant neoplasms of the breast, 
# Malignant neoplasms of the female genital organs, 
# Malignant neoplasms of the male genital organs, 
# Malignant neoplasms of the urinary tract,
# Malignant neoplasms of the eyes, brain and other parts of the central nervous system, 
# Malignant neoplasms of the thyroid and other endocrine glands, 
# Malignant neoplasms of the maldefined, 
# secondary and nonspecific sites,
# Malignant neoplasms of lymphatic,
# hematopoietic and related tissues, 
# Malignant neoplasms of multiple independent sites (primary).
# Period: 2000-2025
cancer <- read_datasus("~/Brazil-measles/data/mortality/all_cancer_sim_cnv.csv",
                       line_skip = 5, drop_cols = "total", values_to = "cancer_deaths") 


dat <- df %>% merge(panc, by = c("muni_code_6", "year")) %>% 
  merge(cancer, by = c("muni_code_6", "year")) %>% 
  mutate(pancreatic_mx = pancreatic_deaths / pop_total * 1000,
         cancer_mx = cancer_deaths / pop_total * 1000)


cor.test(dat$mv_incid_total, dat$cancer_mx)      # -0.01037754 
cor.test(dat$mv_incid_total_lag1, dat$cancer_mx) # -0.01061673
cor.test(dat$mv_incid_total_lag2, dat$cancer_mx) # -0.008021512
cor.test(dat$mv_incid_total_lag3, dat$cancer_mx) # -0.00378682
cor.test(dat$mv_incid_total_lag4, dat$cancer_mx) #  0.002069004
cor.test(dat$mv_incid_total_lag5, dat$cancer_mx) # -0.007480575


cor.test(dat$mv_incid_total, dat$pancreatic_mx)      # -0.002775584
cor.test(dat$mv_incid_total_lag1, dat$pancreatic_mx) # -0.004472342
cor.test(dat$mv_incid_total_lag2, dat$pancreatic_mx) # -0.000403031  
cor.test(dat$mv_incid_total_lag3, dat$pancreatic_mx) #  0.001001683
cor.test(dat$mv_incid_total_lag4, dat$pancreatic_mx) #  0.002054092
cor.test(dat$mv_incid_total_lag5, dat$pancreatic_mx) # -0.002564923


m0 <- glm.nb(cancer_deaths ~ mv_incid_total + region + year + offset(log(pop_total)),
             data = dat)

m1 <- glm.nb(cancer_deaths ~ mv_incid_total_lag1 + region + year + offset(log(pop_total)),
             data = dat)

m2 <- glm.nb(cancer_deaths ~ mv_incid_total_lag2 + region + year + offset(log(pop_total)),
             data = dat)

m3 <- glm.nb(cancer_deaths ~ mv_incid_total_lag3 + region + year + offset(log(pop_total)),
             data = dat)

m4 <- glm.nb(cancer_deaths ~ mv_incid_total_lag4 + region + year + offset(log(pop_total)),
             data = dat)

m5 <- glm.nb(cancer_deaths ~ mv_incid_total_lag5 + region + year + offset(log(pop_total)),
             data = dat)


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
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="gray60", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases per 1,000", y = "Cancer deaths per 1,000") +
  coord_cartesian(ylim = c(0.15, 1.2), xlim = c(0.4, 9.6)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  theme + theme(axis.text.y = element_text(size = 12, color = "black"),
                axis.title.y = element_text(size = 12, color = "black"),
                axis.ticks.y = element_line(size = 0.3, color = "black"))


lag1_p <- lag1_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="gray60", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 1 year prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 1.2), xlim = c(0.4, 9.6)) +
  theme

lag2_p <- lag2_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="gray60", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 2 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 1.2), xlim = c(0.4, 9.6)) +
  theme


lag3_p <- lag3_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="gray60", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 3 years prior", y = "Cancer deaths per 1,000") +
  coord_cartesian(ylim = c(0.15, 1.2), xlim = c(0.4, 9.6)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  theme + theme(axis.text.y = element_text(size = 12, color = "black"),
                axis.title.y = element_text(size = 12, color = "black"),
                axis.ticks.y = element_line(size = 0.3, color = "black"))


lag4_p <- lag4_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="gray60", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 4 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 1.2), xlim = c(0.4, 9.6)) +
  theme

lag5_p <- lag5_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="gray60", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Measles cases 5 years prior", y = NULL) +
  coord_cartesian(ylim = c(0.15, 1.2), xlim = c(0.4, 9.6)) +
  theme


fig <- ggarrange(lag0_p, lag1_p, lag2_p, lag3_p, lag4_p, lag5_p, 
                 nrow = 2, ncol = 3,
                 widths = c(1.27, 1, 1, 
                            1.27, 1, 1))


ggsave(filename = "~/Brazil-measles/figures/cancer_v_lagged_cases.png", 
       plot = fig, height = 6, width = 7.25, units = "in", bg='white')



