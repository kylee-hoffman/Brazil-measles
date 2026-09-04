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
library(janitor)
library(feasts)


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

lag0_r <- format(round(cor(df$mv_incid_total, df$nm_mx, method = "pearson"), 3), nsmall = 3)
lag1_r <- format(round(cor(df$mv_incid_total_lag1, df$nm_mx, method = "pearson"), 3), nsmall = 3)
lag2_r <- format(round(cor(df$mv_incid_total_lag2, df$nm_mx, method = "pearson"), 3), nsmall = 3)
lag3_r <- format(round(cor(df$mv_incid_total_lag3, df$nm_mx, method = "pearson"), 3), nsmall = 3)
lag4_r <- format(round(cor(df$mv_incid_total_lag4, df$nm_mx, method = "pearson"), 3), nsmall = 3)
lag5_r <- format(round(cor(df$mv_incid_total_lag5, df$nm_mx, method = "pearson"), 3), nsmall = 3)

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
        plot.margin=margin(t = 0, r = 4, b = 0, l = 0, unit = "mm"),
        legend.background = element_blank())

p0 <- ggplot(df %>% filter(mv_incid_total > 0 & nm_mx > 0), aes(x = mv_incid_total, y = nm_mx)) +
  geom_point(shape = 21, color = "#87406f", stroke = 0.3, fill = "#f5bfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000", y = "Non-measles ID deaths per 1,000") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10(breaks = c(0.03, 0.1, 0.3, 1, 3)) +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag0_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))


p1 <- ggplot(df %>% filter(mv_incid_total_lag1 > 0 & nm_mx > 0), 
       aes(x = mv_incid_total_lag1, y = nm_mx)) +
  geom_point(shape = 21, color = "#87406f", stroke = 0.3, fill = "#f5bfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 1 year prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag1_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p2 <- ggplot(df %>% filter(mv_incid_total_lag2 > 0 & nm_mx > 0), 
             aes(x = mv_incid_total_lag2, y = nm_mx)) +
  geom_point(shape = 21, color = "#87406f", stroke = 0.3, fill = "#f5bfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 2 years prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag2_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p3 <- ggplot(df %>% filter(mv_incid_total_lag3 > 0 & nm_mx > 0), 
             aes(x = mv_incid_total_lag3, y = nm_mx)) +
  geom_point(shape = 21, color = "#87406f", stroke = 0.3, fill = "#f5bfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 3 years prior", y = "Non-measles ID deaths per 1,000") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag3_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p4 <- ggplot(df %>% filter(mv_incid_total_lag4 > 0 & nm_mx > 0), 
             aes(x = mv_incid_total_lag4, y = nm_mx)) +
  geom_point(shape = 21, color = "#87406f", stroke = 0.3, fill = "#f5bfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 4 years prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag4_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p5 <- ggplot(df %>% filter(mv_incid_total_lag5 > 0 & nm_mx > 0), 
             aes(x = mv_incid_total_lag5, y = nm_mx)) +
  geom_point(shape = 21, color = "#87406f", stroke = 0.3, fill = "#f5bfe3", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 5 years prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag5_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

fig <- ggarrange(p0, p1, p2, 
                 NULL, NULL, NULL, 
                 p3, p4, p5,
                 nrow = 3, ncol = 3,
                 widths = c(1.1, 1, 1),
                 heights = c(1, 0.07, 1)) +
  theme(plot.margin = margin (1, 1, 1, 1, "mm"))

ggsave(filename = "~/Brazil-measles/figures/nmid_v_lagged_cases.png", 
       plot = fig, height = 6, width = 9, units = "in", bg='white')

rm(list = ls())

################################################################################
##
##
## sensitivity test - cancer instead of nmid deaths
##
##
################################################################################

load('~/Brazil-measles/data/analysis_data.RData')

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


dat <- df %>% merge(cancer, by = c("muni_code_6", "year")) %>% 
  mutate(cancer_mx = cancer_deaths / pop_total * 1000)


cor.test(dat$mv_incid_total, dat$cancer_mx)      # -0.01037754 
cor.test(dat$mv_incid_total_lag1, dat$cancer_mx) # -0.01061673
cor.test(dat$mv_incid_total_lag2, dat$cancer_mx) # -0.008021512
cor.test(dat$mv_incid_total_lag3, dat$cancer_mx) # -0.00378682
cor.test(dat$mv_incid_total_lag4, dat$cancer_mx) #  0.002069004
cor.test(dat$mv_incid_total_lag5, dat$cancer_mx) # -0.007480575

lag0_r <- format(round(cor(dat$mv_incid_total, dat$cancer_mx, method = "pearson"), 3), nsmall = 3)
lag1_r <- format(round(cor(dat$mv_incid_total_lag1, dat$cancer_mx, method = "pearson"), 3), nsmall = 3)
lag2_r <- format(round(cor(dat$mv_incid_total_lag2, dat$cancer_mx, method = "pearson"), 3), nsmall = 3)
lag3_r <- format(round(cor(dat$mv_incid_total_lag3, dat$cancer_mx, method = "pearson"), 3), nsmall = 3)
lag4_r <- format(round(cor(dat$mv_incid_total_lag4, dat$cancer_mx, method = "pearson"), 3), nsmall = 3)
lag5_r <- format(round(cor(dat$mv_incid_total_lag5, dat$cancer_mx, method = "pearson"), 3), nsmall = 3)

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
        plot.margin=margin(t = 0, r = 4, b = 0, l = 0, unit = "mm"),
        legend.background = element_blank())

p0 <- ggplot(dat %>% filter(mv_incid_total > 0 & cancer_mx > 0), 
             aes(x = mv_incid_total, y = cancer_mx)) +
  geom_point(shape = 21, color = "gray30", stroke = 0.3, fill = "gray90", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000", y = "Cancer deaths per 1,000") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10(breaks = c(0.03, 0.1, 0.3, 1, 3)) +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag0_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))


p1 <- ggplot(dat %>% filter(mv_incid_total_lag1 > 0 & cancer_mx > 0), 
             aes(x = mv_incid_total_lag1, y = cancer_mx)) +
  geom_point(shape = 21, color = "gray30", stroke = 0.3, fill = "gray90", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 1 year prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag1_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p2 <- ggplot(dat %>% filter(mv_incid_total_lag2 > 0 & cancer_mx > 0), 
             aes(x = mv_incid_total_lag2, y = cancer_mx)) +
  geom_point(shape = 21, color = "gray30", stroke = 0.3, fill = "gray90", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 2 years prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag2_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p3 <- ggplot(dat %>% filter(mv_incid_total_lag3 > 0 & cancer_mx > 0), 
             aes(x = mv_incid_total_lag3, y = cancer_mx)) +
  geom_point(shape = 21, color = "gray30", stroke = 0.3, fill = "gray90", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 3 years prior", y = "Cancer deaths per 1,000") +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag3_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p4 <- ggplot(dat %>% filter(mv_incid_total_lag4 > 0 & cancer_mx > 0), 
             aes(x = mv_incid_total_lag4, y = cancer_mx)) +
  geom_point(shape = 21, color = "gray30", stroke = 0.3, fill = "gray90", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 4 years prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag4_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

p5 <- ggplot(dat %>% filter(mv_incid_total_lag5 > 0 & cancer_mx > 0), 
             aes(x = mv_incid_total_lag5, y = cancer_mx)) +
  geom_point(shape = 21, color = "gray30", stroke = 0.3, fill = "gray90", alpha = 0.5, size = 1) + 
  geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
  labs(x = "Measles cases per 1,000 5 years prior", y = NULL) +
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
  scale_y_log10() +
  coord_cartesian(xlim = c(0.00014, 6), ylim = c(0.036, 4)) +
  annotate("text", x = 0.00015, y = 3.3, family = "Myriad Pro", size = 3.7, hjust = 0,
           label = paste("Pearson's r:", lag5_r)) + theme + 
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))

fig <- ggarrange(p0, p1, p2, 
                 NULL, NULL, NULL, 
                 p3, p4, p5,
                 nrow = 3, ncol = 3,
                 widths = c(1.1, 1, 1),
                 heights = c(1, 0.07, 1)) +
  theme(plot.margin = margin (1, 1, 1, 1, "mm"))

ggsave(filename = "~/Brazil-measles/figures/cancer_v_lagged_cases.png", 
       plot = fig, height = 6, width = 9, units = "in", bg='white')

rm(list = ls())


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
