library(tidyverse)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(msm)
library(jtools)

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

summary(m <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + literacy_rate + poverty_rate + 
                   pct_urban + clinics_pc + region + year, data = df))


# coefficient table
se <- summ(m, robust = "HC1", digits = 3)$coeftable %>% 
  as.data.frame() %>% rownames_to_column() %>% select(rowname, `S.E.`)

coefs <- summ(m, robust = "HC1", confint = T, digits = 3)$coeftable %>% 
  as.data.frame() %>% rownames_to_column() %>% 
  merge(se, by = "rowname") %>% 
  mutate(sig = cut(p, 
                   breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                   labels = c("***", "**", "*", ""), 
                   right = FALSE),
         CI = paste0("(", round(`2.5%`, 3), ", ", round(`97.5%`, 3), ")"),
         Estimate = paste0(round(`Est.`, 3), sig),
         SE = as.character(round(`S.E.`, 3))) %>% 
  select(Term = rowname, Estimate, CI, SE)
# Observations: 99126
# R² = 0.113
# Adj. R² = 0.112 
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
        axis.ticks.x = element_line(size = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 0.5, r = 1.1, b = 1.5, l = 1.1, unit = "mm"),
        legend.background = element_blank())


#quantile(df$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
# 17.13 78.78 
pov_p <- predict_response(m, 
                          terms = list(poverty_rate = seq(17.13, 78.78, 
                                                          length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#e6d7f7", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate", y = NULL) + 
  coord_cartesian(ylim = c(94, 100), xlim = c(19.7, 76.2)) +
  scale_y_continuous(breaks = c(94, 96, 98, 100),
                     labels = c("94%", "96%", "98%", "100% ")) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks.y = element_line(size = 0.3, color = "black"))


#quantile(df$literacy_rate, probs = c(0.1, 0.9), na.rm=T)
# 70.67 95.30 
lit_p <- predict_response(m, 
                 terms = list(literacy_rate = seq(70.67, 95.30, 
                                                  length.out = 10)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#abffc8", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Literacy rate", y = NULL) + 
  coord_cartesian(ylim = c(94, 100), xlim = c(71.7, 94.27)) +
  scale_x_continuous(breaks = c(75, 85, 95)) + theme

#quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 33.14 92.87
urban_p <- predict_response(m, 
                            terms = list(pct_urban = seq(33.14, 92.87, 
                                                         length.out = 20)), 
                            margin = "mean_mode", 
                            ci_level = 0.95,
                            type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% urban", y = NULL) + 
  coord_cartesian(ylim = c(94, 100), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) + theme


# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.09167864 0.54545004 
clinic_p <-  predict_response(m, 
                              terms = list(clinics_pc = seq(0.09167864, 0.54545004,
                                                            length.out = 20)), 
                              margin = "mean_mode", 
                              ci_level = 0.95,
                              type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#a3d6ca", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "UBS clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim =  c(94, 100), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme


# quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 4688.17 41242.88 
gdp_p <- predict_response(m, 
                          terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                    length.out = 10)), 
                          margin = "mean_mode", 
                          ci_level = 0.95,
                          type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#faacae", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "GDP PC (1,000 BRL)", y = NULL) + 
  coord_cartesian(ylim = c(94, 100), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme


fig <- ggarrange(pov_p, lit_p, urban_p, clinic_p, gdp_p, 
          nrow = 1, widths = c(1.34, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0, r = 0, b = 0, l = 4, unit = "mm")) +
  annotate("text", x = -0.01, y = 0.6, size = 4.5, angle = 90,
           label = "MCV dose 1 coverage", 
           family = "Myriad Pro")



ggsave(filename = "~/Brazil-measles/figures/vax_preds.png", 
       plot = fig, height = 2, width = 8, units = "in", bg='white')




# literacy rate is unexpected
summary(a <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + 
                  pct_urban + clinics_pc + literacy_rate * region + year,
                data = df))

summary(b <- lm(mcv_d1_cov_tc ~ log(gdp_pc) + poverty_rate + 
                  pct_urban + clinics_pc + literacy_rate * year + region,
                data = df))

a_p <- predict_response(a, 
                 terms = list(literacy_rate = seq(68.72, 95.1, length.out = 10),
                              region = c("Norte",  "Nordeste", "Sudeste", "Sul", "Centro-oeste")), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "fixed") %>% 
  as.data.frame() %>% 
  mutate(group = recode(group, Norte = "North", Sul = "South", `Centro-oeste` = "Central-West",
                        Nordeste = "Northeast", Sudeste = "Southeast")) %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.3) +
  geom_line() + labs(x = "literacy rate", y = "MCV dose 1 coverage", fill = "Region", color = "Region") +
  coord_cartesian(xlim = c(69.8, 94)) +
  scale_y_continuous(breaks = c(80, 85, 90, 95, 100, 105),
                     labels = c("80%", "85%", "90%", "95%", "100%", "105%")) +
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
                 terms = list(literacy_rate = seq(68.72, 95.1, length.out = 10),
                              year = seq(2006, 2023)), 
                 margin = "mean_mode", 
                 ci_level = 0.95,
                 type = "fixed") %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted, group = group, color = group, fill = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
  geom_line() + labs(x = "literacy rate", y = "MCV dose 1 coverage", fill = "year", color = "year") +
  scale_y_continuous(breaks = c(80, 85, 90, 95, 100),
                     labels = c("80%", "85%", "90%", "95%", "100% ")) +
  coord_cartesian(xlim = c(69.8, 94)) +
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

