library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(MASS)
library(ggeffects)
library(parameters)
library(performance)
library(legendry)
library(ggnewscale)
library(scales)
library(stringr)

source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_Data/04.analysis_data_final.RData")

load("~/Brazil-measles/data/generated_Data/models/cases/mod2a.RData")
load("~/Brazil-measles/data/generated_Data/models/cases/mod2a_vcov.RData")

load("~/Brazil-measles/data/generated_Data/models/cases/mod2b.RData")
load("~/Brazil-measles/data/generated_Data/models/cases/mod2b_vcov.RData")

load("~/Brazil-measles/data/generated_Data/models/cases/mod2c.RData")
load("~/Brazil-measles/data/generated_Data/models/cases/mod2c_vcov.RData")

load("~/Brazil-measles/data/generated_Data/models/cases/mod2d.RData")
load("~/Brazil-measles/data/generated_Data/models/cases/mod2d_vcov.RData")

################################################################################
##
## predicted probability plots for logit models
##
################################################################################

#plot(allEffects(mod2a, vcov. = mod2a.vcov))
#plot(allEffects(mod2b, vcov. = mod2b.vcov))

pred2a <- pred_prob_plot_fun.state_fe(mod2a, mod2a.vcov, 
                                      ylim = c(0.00095, 0.00985), 
                                      ybreaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
  annotate("text", x = -0.015, y = 0.57, angle = 90, label = "Predicted probability of ≥ 1 measles case",
           size = 12 / .pt, family = "Myriad Pro")

pred2b <- pred_prob_plot_fun.state_fe(mod2b, mod2b.vcov, 
                                      ylim = c(0.003, 0.035), 
                                      #ybreaks = c(0.01, 0.02, 0.03)) +
                                      ybreaks = c(0.005, 0.015, 0.025, 0.035)) +
                                      #ybreaks = c(0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.035)) +
  annotate("text", x = -0.015, y = 0.57, angle = 90, label = "Predicted probability of ≥ 1 measles case",
           size = 12 / .pt, family = "Myriad Pro")


fig <- ggarrange(pred2a, NULL, pred2b, ncol = 1, heights = c(1, 0.02, 1)) + 
  theme(plot.margin=margin(t = 4, r = 0, b = -4, l = -1, unit = "mm")) +
  annotate("text", x = 0.085, y = 1.0122, label = "A   (2006-2018)", family = "Myriad Pro Bold", size = 12 / .pt) +
  annotate("text", x = 0.085, y = 0.505, label = "B   (2018-2023)", family = "Myriad Pro Bold", size = 12 / .pt)



ggsave(filename = "~/Brazil-measles/figures/mod2ab_predicted_probs.png", 
       plot = fig, height = 7, width = 7.25, units = "in", bg='white', dpi = 800)









pred_prob_plot_fun.2c <- function(model, vcov, ylim, ybreaks) {
  
  theme <- theme_bw(base_family = "Myriad Pro", base_size = 12) +
    theme(text = element_text(color = "black"),
          axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 12, margin = margin(1, 0, 1, 0, "mm")),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.3),
          plot.background = element_blank(),
          axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
          axis.ticks.y = element_blank(),
          plot.margin=margin(t = 0.5, r = 1, b = 1.5, l = 1, unit = "mm"),
          legend.background = element_blank())
  
  mcv_p <- predict_response(model,
                            terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 10)),
                            margin = "marginalmeans",
                            ci_level = 0.95,
                            type = "fixed", 
                            vcov = vcov,
                            rg.limit = 50000) %>%
    data.frame() %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a7c5e8", alpha = 0.7) +
    geom_line(linewidth = 0.3) +
    scale_x_continuous(breaks = c(0, 25, 50, 75)) +
    scale_y_continuous(labels = scales::percent, breaks = ybreaks) +
    coord_cartesian(ylim = ylim, xlim = c(4.4, 95.6)) +
    labs(x = "MCV1 coverage\n2 years prior", y = NULL) + 
    theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 1, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  #quantile(df_muni_fe$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
  # 17.13 78.78 
  pov_p <- predict_response(model, 
                            terms = list(poverty_rate = seq(17.13, 78.78, 
                                                            length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    coord_cartesian(ylim = ylim, xlim = c(19.8, 76.1)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    labs(x = "Poverty rate\n", y = NULL) + theme
  
  #quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
  # 33.14 92.87
  urban_p <- predict_response(model, 
                              terms = list(pct_urban = seq(33.14, 92.87, 
                                                           length.out = 10)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              type = "fixed",
                              vcov = vcov,
                              rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "% Urban\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(35.6, 90.4)) +
    scale_y_continuous(labels = scales::percent, breaks = ybreaks) +
    scale_x_continuous(breaks = c(40, 60, 80)) + theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 1, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  
  #quantile(df$clinics_pc_scaled, probs = c(0.1, 0.9), na.rm=T)
  # -1.145079  1.159421 
  clinic_p <- predict_response(model, 
                               terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                    length.out = 10)), 
                               margin = "marginalmeans", 
                               ci_level = 0.95,
                               type = "fixed",
                               vcov = vcov,
                               rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x * sd(df$clinics_pc) + mean(df$clinics_pc), # rescale
               y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#edbd91", alpha = 0.4) +
    geom_line(linewidth = 0.3) +
    labs(x = "Clinics per 1,000\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(0.11, 0.527)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    scale_x_continuous(breaks = c(0.15, 0.3, 0.45)) + theme
  
  
  # quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
  # 4688.17 41242.88 
  gdp_p <- predict_response(model, 
                            terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                      length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#114B5F", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "GDP PC (1,000 BRL)\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(6200, 39700)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                       labels = c("10", "20", "30", "40")) + theme
  
  
  #quantile(df$gini, probs = c(0.1, 0.9), na.rm=T)
  #42.67 60.17
  gini_p <- predict_response(model,
                             terms = list(gini = seq(42.67, 60.17,
                                                     length.out = 10)),
                             margin = "marginalmeans",
                             ci_level = 0.95,
                             type = "fixed",
                             vcov = vcov,
                             rg.limit = 50000) %>% data.frame() %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                linewidth = 0, fill ="#ACC196", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Gini index\n", y = NULL) +
    coord_cartesian(ylim = ylim, xlim = c(43.43, 59.41)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme


  #quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
  # 9.170367 17.785892
  cbr_p <- predict_response(model, 
                            terms = list(cbr = seq(9.170367, 17.785892, 
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8783D1", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Crude birth rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(9.6, 17.4)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  
  # quantile(df$imr, probs = c(0.1, 0.9), na.rm=T)
  # 0.00000 28.90173 
  imr_p <- predict_response(model, 
                            terms = list(imr = seq(0.00, 28.90173,
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#b05a45", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Infant mortality rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(1.24, 27.64)) +
    scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  # quantile(df$cdr, probs = c(0.1, 0.9), na.rm=T)
  # 3.949247 9.060420 
  cdr_p <- predict_response(model, 
                            terms = list(cdr = seq(3.949247, 9.060420,
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8ebfad", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Crude death rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(4.16, 8.85)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  return(ggarrange(mcv_p, clinic_p, pov_p, gini_p, imr_p,
                   urban_p, cbr_p, gdp_p, cdr_p,
                   nrow = 2, ncol = 5,
                   widths = c(1.25, 1, 1, 1, 1)) + theme(plot.margin = margin(2, 1, -6, 0, unit = "mm")))
}

fig.2c <- pred_prob_plot_fun.2c(mod2c, mod2c.vcov, 
                                     ylim = c(0.018, 0.67), 
                                     ybreaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) + 
  theme(plot.margin = margin(0, 0, 0, 5, unit = "mm"))




pred_prob_plot_fun.2d <- function(model, vcov, ylim, ybreaks) {
  
  theme <- theme_bw(base_family = "Myriad Pro", base_size = 12) +
    theme(text = element_text(color = "black"),
          axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 12, margin = margin(1, 0, 1, 0, "mm")),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.3),
          plot.background = element_blank(),
          axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
          axis.ticks.y = element_blank(),
          plot.margin=margin(t = 0.5, r = 1, b = 1.5, l = 1, unit = "mm"),
          legend.background = element_blank())
  
  mcv_p <- predict_response(model,
                            terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 10)),
                            margin = "marginalmeans",
                            ci_level = 0.95,
                            type = "fixed", 
                            vcov = vcov,
                            rg.limit = 50000) %>%
    data.frame() %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a7c5e8", alpha = 0.7) +
    geom_line(linewidth = 0.3) +
    scale_x_continuous(breaks = c(0, 25, 50, 75)) +
    scale_y_continuous(labels = scales::percent, breaks = ybreaks) +
    coord_cartesian(ylim = ylim, xlim = c(4.4, 95.6)) +
    labs(x = "MCV1 coverage\n2 years prior", y = NULL) + 
    theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 1, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  #quantile(df_muni_fe$poverty_rate, probs = c(0.1, 0.9), na.rm=T)
  # 17.13 78.78 
  pov_p <- predict_response(model, 
                            terms = list(poverty_rate = seq(17.13, 78.78, 
                                                            length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    coord_cartesian(ylim = ylim, xlim = c(19.8, 76.1)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    labs(x = "Poverty rate\n", y = NULL) + theme
  
  #quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
  # 33.14 92.87
  urban_p <- predict_response(model, 
                              terms = list(pct_urban = seq(33.14, 92.87, 
                                                           length.out = 10)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              type = "fixed",
                              vcov = vcov,
                              rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "% Urban\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(35.6, 90.4)) +
    scale_y_continuous(labels = scales::percent, breaks = ybreaks) +
    scale_x_continuous(breaks = c(40, 60, 80)) + theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 1, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  # quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
  # 4688.17 41242.88 
  gdp_p <- predict_response(model, 
                            terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                      length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#114B5F", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "GDP PC (1,000 BRL)\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(6200, 39700)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                       labels = c("10", "20", "30", "40")) + theme
  
  clinic_p <- predict_response(model, 
                               terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                    length.out = 10)), 
                               margin = "marginalmeans", 
                               ci_level = 0.95,
                               type = "fixed",
                               vcov = vcov,
                               rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x * sd(df$clinics_pc) + mean(df$clinics_pc),
               y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#edbd91", alpha = 0.4) +
    geom_line(linewidth = 0.3) +
    labs(x = "Clinics per 1,000\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(0.11, 0.527)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    scale_x_continuous(breaks = c(0.15, 0.3, 0.45)) + theme
  
  
  #quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
  # 9.170367 17.785892
  cbr_p <- predict_response(model, 
                            terms = list(cbr = seq(9.170367, 17.785892, 
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8783D1", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Crude birth rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(9.6, 17.4)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  # quantile(df$imr, probs = c(0.1, 0.9), na.rm=T)
  # 0.00000 28.90173 
  imr_p <- predict_response(model, 
                            terms = list(imr = seq(0.00, 28.90173,
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#b05a45", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Infant mortality rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(1.24, 27.64)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  # quantile(df$cdr, probs = c(0.1, 0.9), na.rm=T)
  # 3.949247 9.060420 
  cdr_p <- predict_response(model, 
                            terms = list(cdr = seq(3.949247, 9.060420,
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8ebfad", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Crude death rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(4.16, 8.85)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  

  return(ggarrange(mcv_p, clinic_p, pov_p, imr_p, 
                   urban_p, cbr_p, gdp_p, cdr_p, 
                   nrow = 2, ncol = 4,
                   widths = c(1.28, 1, 1, 1)) + theme(plot.margin = margin(2, 1, -6, 0, unit = "mm")))
}




fig.2d <- pred_prob_plot_fun.2d(mod2d, mod2d.vcov, 
                                     ylim = c(0.015, 0.99), 
                                     ybreaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme(plot.margin = margin(0, 10, 0, 10, unit = "mm"))


fig <- ggarrange(fig.2c, NULL, fig.2d, ncol = 1, heights = c(1, 0.08, 1), widths = c(1, 1, 0.7)) +
  annotate("text", x = 0.04, y = 0.28, label = "Predicted probability of ≥ 1 measles case", 
           family = "Myriad Pro", size = 12 / .pt, angle = 90) +
  annotate("text", x = 0.01, y = 0.8, label = "Predicted probability of ≥ 1 measles case", 
           family = "Myriad Pro", size = 12 / .pt, angle = 90) +
  annotate("text", x = 0.515, y = 1.013, label = "2006–2023", family = "Myriad Pro Bold", size = 12 / .pt) +
  annotate("text", x = 0.52, y = 0.495, label = "2018–2023", family = "Myriad Pro Bold", size = 12 / .pt) +
  theme(plot.margin = margin(5, 1, -6, 0, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/mod2c_2d_predicted_probs.png", 
       plot = fig, height = 7, width = 7.25, units = "in", bg='white', dpi = 800)


