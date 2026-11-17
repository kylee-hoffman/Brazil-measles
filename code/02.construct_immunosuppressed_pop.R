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



################################################################################
## cured measles cases -> amnesia prevalence (1, 2, and 3-year durations)
##
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinannet/cnv/exantbr.def
## http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinanwin/cnv/exantbr.def
##
################################################################################
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/02.analysis_data.RData")


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
  dplyr::select(region, state, muni_code_6, year, nm_deaths_total, nm_mx_total,
                nm_deaths_1to9, nm_mx_1to9, nm_deaths_1to14, nm_mx_1to14, 
                nm_deaths_1to19, nm_mx_1to19, nm_deaths_1to29, nm_mx_1to29,
                mv_cases_total, mv_incid_total, 
                amnesia_prev_d1, amnesia_prev_d2, amnesia_prev_d3,
                mcv_d1_cov, mcv_d1_cov_tc, mcv_d1_cov_tc_lag1, mcv_d1_cov_tc_lag2, mcv_d1_cov_tc_lag3, 
                mcv_d1_cov_tc_lag4, mcv_d1_cov_tc_lag5, cbr, cdr, imr, gdp_pc, travel,
                clinics_pc, pop_den, pop_total, pop_1to9, pop_1to14, pop_1to19, pop_1to29,
                pct_urban, poverty_rate, gini)

df$year <- droplevels(df$year)

save(df, file = '~/Brazil-measles/data/generated_data/04.analysis_data_final.RData')



# histogram of amnesia prevalence
p <- df %>% filter(amnesia_prev_d3 != 0) %>% 
  ggplot(aes(x = amnesia_prev_d3)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_y_log10(breaks = c(1, 10, 100, 1000), 
                labels = c("1", "10", "100", "1,000")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  coord_cartesian(xlim = c(0, 9), ylim = c(1.45, 2700)) +
  theme_bw(base_family = "Myriad Pro", base_size = 15) +
  labs(x = "Immune amnesia prevalence per 1,000\n(3-year duration)", y = "Frequency count (log scale)") +
  theme(text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        plot.margin=margin(t = 1, r = 1, b = 1, l = 1, unit = "mm"))

ggsave(filename = "~/Brazil-measles/figures/amnesia_prevalence_frequencies.png", 
       plot = p, height = 4, width = 7, units = "in", bg='white', dpi = 600)
