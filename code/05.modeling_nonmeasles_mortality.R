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


load("~/brazil_measles/generated_data/muni-year_panel_02-24.RData")


# overall correlation of mean mortality and incidence, NOT within-municipality
fig1a <- df %>% 
  group_by(year) %>% 
  reframe(year = as.numeric(unique(year)), 
          nm_mx_total = mean(nm_mx_total, na.rm = T), 
          mv_incid_total = mean(mv_incid_total, na.rm = T)) %>% 
  as_tsibble(index = year) %>% 
  CCF(y = nm_mx_total, x = mv_incid_total, type = "correlation", lag.max = 5) %>% 
  mutate(lag = as.numeric(str_sub(lag, end = -2))) %>% 
  ggplot(aes(x = lag, y = ccf)) +
  geom_hline(aes(yintercept = 0.409), linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(fill = as.factor(lag)), size = 2.5, shape = 21, stroke = 0.4) +
  scale_fill_manual(values = c("0" = "gray60", "3" = "gray60", "4" = "gray60", "5" = "gray60", 
                               "1" = "#ab023d", "2" = "#ab023d")) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 0.5)) +
  labs(y = "Correlation", x = "Lag of measles incidence (cured cases)") +
  theme_bw(base_size=18, base_family = "Myriad Pro") + 
  theme(plot.title = element_text(size = 9),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 18, color = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none")



plot_df <- df %>% 
  group_by(year) %>% 
  reframe(year = unique(year),
          mean_incid = mean(mv_incid_total),
          mean_nm_mx = mean(nm_mx_total),
          sd_nm_mx = sd(nm_mx_total), 
          n = n()) %>% 
  mutate(incid_lag1 = dplyr::lag(mean_incid, 1, order_by = year),
         incid_lag2 = dplyr::lag(mean_incid, 2, order_by = year),
         incid_lag3 = dplyr::lag(mean_incid, 3, order_by = year)) %>% 
  ungroup() %>% 
  mutate(upper_nm_mx = mean_nm_mx + sd_nm_mx/sqrt(n), 
         lower_nm_mx = mean_nm_mx - sd_nm_mx/sqrt(n))


theme1 <- theme_bw(base_family = "Myriad Pro", base_size = 11) +
  theme(text = element_text(size = 11, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks.x = element_line(size = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 0, r = 2, b = 0, l = 0, unit = "mm"))


p1 <- plot_df %>% 
  filter(as.numeric(year) >= 2005) %>% 
  ggplot(aes(x=mean_incid, y=mean_nm_mx)) +
  geom_smooth(method = "lm", fill = "#94D4D2", color = "black", linewidth = 0.2, alpha = 0.3) +
  geom_point(size = 1, shape = 21, alpha = 0.6, color = "gray20", fill = "gray50", stroke = 0.2) + 
  coord_cartesian(xlim = c(0.0005, 0.016), ylim = c(0.324, 0.81)) +
  scale_x_continuous(breaks = c(0, 0.005, 0.01, 0.015), labels = c("0.0", "0.005", "0.01", "0.015")) +
  labs(x= NULL, y = "Mean NMID deaths per 1,000") +
  theme_bw(base_family = "Myriad Pro", base_size = 11) +
  theme(text = element_text(size = 11, color = "black", family = "Myriad Pro"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 0, r = 2, b = 0, l = 0, unit = "mm")) +
  annotate("text", x = 0.0115, y = 0.34, label="No lag", size = 4)

p2 <- plot_df %>% 
  filter(as.numeric(year) >= 2005) %>% 
  ggplot(aes(x=incid_lag1, y=mean_nm_mx)) +
  geom_smooth(method = "lm", fill = "#94D4D2", color = "black", linewidth = 0.2, alpha = 0.3) +
  geom_point(size = 1, shape = 21, alpha = 0.6, color = "gray20", fill = "gray50", stroke = 0.2) + 
  coord_cartesian(xlim = c(0.0005, 0.016), ylim = c(0.324, 0.81)) +
  scale_x_continuous(breaks = c(0, 0.005, 0.01, 0.015), labels = c("0.0", "0.005", "0.01", "0.015")) +
  labs(x= NULL, y = NULL) +
  annotate("text", x = 0.0125, y = 0.34, label="Lag 1", size = 4) +
  theme1

p3 <- plot_df %>% 
  filter(as.numeric(year) >= 2005) %>% 
  ggplot(aes(x=incid_lag2, y=mean_nm_mx)) +
  geom_smooth(method = "lm", fill = "#94D4D2", color = "black", linewidth = 0.2, alpha = 0.3) +
  geom_point(size = 1, shape = 21, alpha = 0.6, color = "gray20", fill = "gray50", stroke = 0.2) + 
  coord_cartesian(xlim = c(0.0005, 0.016), ylim = c(0.324, 0.81)) +
  scale_x_continuous(breaks = c(0, 0.005, 0.01, 0.015), labels = c("0.0", "0.005", "0.01", "0.015")) +
  labs(x= NULL, y = NULL) +
  annotate("text", x = 0.0125, y = 0.34, label="Lag 2", size = 4) +
  theme1

p4 <- plot_df %>% 
  filter(as.numeric(year) >= 2005) %>% 
  ggplot(aes(x=incid_lag3, y=mean_nm_mx)) +
  geom_smooth(method = "lm", fill = "#94D4D2", color = "black", linewidth = 0.2, alpha = 0.3) +
  geom_point(size = 1, shape = 21, alpha = 0.6, color = "gray20", fill = "gray50", stroke = 0.2) + 
  coord_cartesian(xlim = c(0.0005, 0.016), ylim = c(0.324, 0.81)) +
  scale_x_continuous(breaks = c(0, 0.005, 0.01, 0.015), labels = c("0.0", "0.005", "0.01", "0.015")) +
  labs(x= NULL, y = NULL) +
  annotate("text", x = 0.0125, y = 0.34, label="Lag 3", size = 4) +
  theme1



fig <- suppressWarnings(ggarrange(p1, p2, p3, p4, ncol = 4, widths = c(1, 0.8, 0.8, 0.8))) +
  theme(plot.margin = unit(c(0.035, 0.015, 0.2, 0.025), "in")) +
  annotate("text", x = 0.5, y = -0.05, label="Mean measles incidence per 1,000 (cured cases)", family = "Myriad Pro",
           size = 4.3)

ggsave(filename = "~/brazil_measles/figures/scatters_lowqual.png", plot = fig, height = 2.2, width = 7.25, bg='#ffffff')


summary(lm(mean_nm_mx ~ mean_incid, plot_df))$r.squared
summary(lm(mean_nm_mx ~ incid_lag1, plot_df))$r.squared
summary(lm(mean_nm_mx ~ incid_lag2, plot_df))$r.squared
summary(lm(mean_nm_mx ~ incid_lag3, plot_df))$r.squared



rm(list = ls())

################################################################################
##
##
## models estimating NMID mortality
##
##
################################################################################

source("~/brazil_measles/code/utils.R")
load("~/brazil_measles/generated_data/muni-year_panel_02-24.RData")

# model 1: no controls, but includes 2-way fixed effects
summary(m1_d1 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d1 + region + year + offset(log(pop_total)), 
                        data = df))

summary(m1_d2 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d2 + region + year + offset(log(pop_total)),
                          data = df))

summary(m1_d3 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d3 + region + year + offset(log(pop_total)),
                          data = df))


# model 2: controls for gdp_pc, CDR, and age structure with 2-way fixed effects
summary(m2_d1 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d1 + log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

summary(m2_d2 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d2 + log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

summary(m2_d3 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d3 + log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))


# model 3: interaction + controls for CDR and age structure + 2-way fixed effects
summary(m3_d1 <- glm.nb(nm_deaths ~ amnesia_prev_d1 * log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

summary(m3_d2 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d2 * log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

summary(m3_d3 <- glm.nb(nm_deaths_total ~ amnesia_prev_total_d3 * log(gdp_pc) + cdr + prop_1to9 + region + year + offset(log(pop_total)),
                          data = df))

# table of results
rr_m1_d1 <- rr_tab(m1_d1) %>% filter(!str_detect(Term, "Intercept|year"))
rr_m1_d2 <- rr_tab(m1_d2) %>% filter(!str_detect(Term, "Intercept|year"))
rr_m1_d3 <- rr_tab(m1_d3) %>% filter(!str_detect(Term, "Intercept|year"))

write.table(rr_m1_d1, file = "", row.names = F, quote = F, sep = "\t")
write.table(rr_m1_d2, file = "", row.names = F, quote = F, sep = "\t")
write.table(rr_m1_d3, file = "", row.names = F, quote = F, sep = "\t")

nobs(m1_d1)
nobs(m1_d2)
nobs(m1_d3)

rsq(m1_d1, adj = T)
rsq(m1_d2, adj = T)
rsq(m1_d3, adj = T)


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


