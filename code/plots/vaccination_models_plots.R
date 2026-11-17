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
library(lmtest)
library(scales)
library(sandwich)

source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_Data/04.analysis_data_final.RData")

load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1a.RData")
load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1a_vcov.RData")

load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1b.RData")
load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1b_vcov.RData")

load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1c.RData")
load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1c_vcov.RData")

load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1d.RData")
load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1d_vcov.RData")


################################################################################
##
## coef plot
##
################################################################################
plot_data_tab <- function(model, vcov, color, years) {
  as.data.frame(coeftest(model, vcov = vcov)[,]) %>% 
    rownames_to_column(var = "term") %>% 
    merge(as.data.frame(coefci(model, vcov = vcov)) %>% 
            rownames_to_column(var = "term"),
          by = "term") %>% 
    mutate(color = color, years = years)
}

coef_plot_df <- 
  bind_rows(plot_data_tab(mod.1a, mod.1a_vcov, color = "state", years = "2006-2023"), 
            plot_data_tab(mod.1b, mod.1b_vcov, color = "state", years = "2018-2023"),
            plot_data_tab(mod.1c, mod.1c_vcov, color = "muni", years = "2006-2023"),
            plot_data_tab(mod.1d, mod.1d_vcov, color = "muni", years = "2018-2023")) %>% 
  filter(!str_detect(term, "Intercept|muni|state|year")) %>% 
  mutate(color = factor(color, levels = c("state", "muni")),
         years = factor(years, levels = c("2006-2023", "2018-2023"))) %>% 
  dplyr::select(term, est = Estimate, lower = "2.5 %", upper = "97.5 %",
                color, years)


coef_p <- coef_plot_df %>% 
  ggplot(aes(x = factor(term, levels = c("poverty_rate", "gini", "cbr", "pct_urban", 
                                         "log(gdp_pc)", "clinics_pc_scaled")),
             y = est, color = color, shape = years)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2) +
  geom_point(size = 1.5, position = position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = color),
                position = position_dodge(width=0.4), width = 0.2, linewidth = 0.3) +
  scale_y_continuous(transform = pseudo_log_trans(sigma = 0.06, base = 10),
                     breaks = c(-4, -2, -1, -0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5, 1, 2, 4)) +
  scale_x_discrete(labels = c(cbr = "Crude birth rate", clinics_pc_scaled = "Clinics per 1,000", 
                              `log(gdp_pc)` = "GDP PC", gini = "Gini index",
                              pct_urban = "% Urban", poverty_rate = "Poverty rate")) +
  scale_color_manual(name = "Model level",
                     values = c(muni = "#793b87",
                                state = "#62b9de"),
                     labels = c(muni = "Within-municipality",
                                state = "Between municipalities")) +
  scale_shape_manual(name = "Time period",
                     values = c(`2006-2023` = 16,
                                `2018-2023` = 15)) +
  labs(y = "Estimated effect on MCV1 coverage", x = NULL) +
  coord_cartesian(ylim = c(-2.75, 3.3)) +
  annotate("rect", xmin = 0.48, xmax = 3.75, ymin = 0.6, ymax = 4.2, fill = "#F7F7F7", color = "black",
           linewidth=0.15) +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(legend.box = "horizontal",
        text = element_text(color = "black", size = 12),
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        plot.margin = margin(t = 0.3, r = 0.3, b = 0.1, l = 0.4, unit = "mm"),
        legend.title = element_text(size = 12, 
                                    margin = margin(t = 1, r = 0, b = 1, l = 0, unit = "mm")),
        legend.text = element_text(size = 11),
        legend.spacing.y = unit(-3, "mm"),
        legend.position = c(0.275, 0.87), # for plot with preds
        legend.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 1, 0, 0, "mm")))



################################################################################
##
## marginal effect plots for 1a and 1b
##
################################################################################

# preds_1a <- vax_preds_plot_fun(mod.1a, 
#                                mod.1a_vcov,
#                                ylim = c(89.5, 93.2),
#                                ybreaks = c(89, 90, 91, 92, 93),
#                                ylabs = c("89%", "90%", "91%", "92%", "93%"))
# 
# preds_1b <- vax_preds_plot_fun(mod.1b, 
#                                mod.1b_vcov,
#                                ylim = c(83, 89.5),
#                                ybreaks = c(83, 85, 87, 89),
#                                ylabs = c("83%", "85%", "87%", "89%"))
# 
# 
# fig <- ggarrange(coef_p, NULL, preds_1a, NULL, preds_1b, ncol = 1, 
#                  heights = c(1, 0.08, 0.4, 0.05, 0.4)) + 
#   theme(plot.margin=margin(t = 0, r = 0, b = -2, l = 0, unit = "mm")) +
#   annotate("rect", xmin = 0.1008, xmax = 0.978, ymin = 0.435, ymax = 0.465, 
#            color = "black", fill = "gray90", linewidth = 0.2) +
#   annotate("rect", xmin = 0.1008, xmax = 0.978, ymin = 0.203, ymax = 0.233, 
#            color = "black", fill = "gray90", linewidth = 0.2) +
#   annotate("text", x = 0.54, y = 0.45, label = "2006–2023", family = "Myriad Pro", size = 12 / .pt) +
#   annotate("text", x = 0.54, y = 0.219, label = "2018–2023", family = "Myriad Pro", size = 12 / .pt) +
#   annotate("text", x = 0.015, y = 0.99, label = "A", family = "Myriad Pro Bold", size = 12 / .pt) +
#   annotate("text", x = 0.015, y = 0.464, label = "B", family = "Myriad Pro Bold", size = 12 / .pt) +
#   annotate("text", x = 0.015, y = 0.23, label = "C", family = "Myriad Pro Bold", size = 12 / .pt)
# 
# 
# ggsave(filename = "~/Brazil-measles/figures/mod1_coefs_mod1ab_preds.png", 
#        plot = fig, height = 6.5, width = 7.25, units = "in", bg='white', dpi = 800)

pov_preds <- bind_rows(predict_response(mod.1a, 
                                        terms = list(poverty_rate = seq(17.13, 78.78, 
                                                                        length.out = 10)), 
                                        margin = "marginalmeans", 
                                        ci_level = 0.95,
                                        vcov = mod.1a_vcov,
                                        type = "fixed") %>% data.frame() %>% mutate(model = "1a"),
                       predict_response(mod.1b, 
                                        terms = list(poverty_rate = seq(17.13, 78.78, 
                                                                        length.out = 10)), 
                                        margin = "marginalmeans", 
                                        ci_level = 0.95,
                                        vcov = mod.1b_vcov,
                                        type = "fixed") %>% data.frame() %>% mutate(model = "1b"))


urban_preds <- bind_rows(predict_response(mod.1a, 
                                          terms = list(pct_urban = seq(33.14, 92.87,
                                                                       length.out = 10)), 
                                          margin = "marginalmeans", 
                                          ci_level = 0.95,
                                          vcov = mod.1a_vcov,
                                          type = "fixed") %>% data.frame() %>% mutate(model = "1a"),
                         predict_response(mod.1b, 
                                          terms = list(pct_urban = seq(33.14, 92.87, 
                                                                       length.out = 10)), 
                                          margin = "marginalmeans", 
                                          ci_level = 0.95,
                                          vcov = mod.1b_vcov,
                                          type = "fixed") %>% data.frame() %>% mutate(model = "1b"))


clinic_preds <- bind_rows(predict_response(mod.1a, 
                                           terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                                length.out = 10)), 
                                           margin = "marginalmeans", 
                                           ci_level = 0.95,
                                           vcov = mod.1a_vcov,
                                           type = "fixed") %>% data.frame() %>% mutate(model = "1a"),
                          predict_response(mod.1b, 
                                           terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                                length.out = 10)), 
                                           margin = "marginalmeans", 
                                           ci_level = 0.95,
                                           vcov = mod.1b_vcov,
                                           type = "fixed") %>% data.frame() %>% mutate(model = "1b"))


gdp_preds <- bind_rows(predict_response(mod.1a, 
                                        terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                                  length.out = 10)), 
                                        margin = "marginalmeans", 
                                        ci_level = 0.95,
                                        vcov = mod.1a_vcov,
                                        type = "fixed") %>% data.frame() %>% mutate(model = "1a"),
                       predict_response(mod.1b, 
                                        terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                                  length.out = 10)), 
                                        margin = "marginalmeans", 
                                        ci_level = 0.95,
                                        vcov = mod.1b_vcov,
                                        type = "fixed") %>% data.frame() %>% mutate(model = "1b"))


cbr_preds <- bind_rows(predict_response(mod.1a, 
                                        terms = list(cbr = seq(9.170367, 17.785892, 
                                                               length.out = 10)), 
                                        margin = "marginalmeans", 
                                        ci_level = 0.95,
                                        vcov = mod.1a_vcov,
                                        type = "fixed") %>% data.frame() %>% mutate(model = "1a"),
                       predict_response(mod.1b, 
                                        terms = list(cbr = seq(9.170367, 17.785892,
                                                               length.out = 10)), 
                                        margin = "marginalmeans", 
                                        ci_level = 0.95,
                                        vcov = mod.1b_vcov,
                                        type = "fixed") %>% data.frame() %>% mutate(model = "1b"))


gini_preds <- bind_rows(predict_response(mod.1a, 
                                         terms = list(gini = seq(42.67, 60.17,
                                                                 length.out = 10)), 
                                         margin = "marginalmeans", 
                                         ci_level = 0.95,
                                         vcov = mod.1a_vcov,
                                         type = "fixed") %>% data.frame() %>% mutate(model = "1a"),
                        predict_response(mod.1b, 
                                         terms = list(gini = seq(42.67, 60.17,
                                                                 length.out = 10)), 
                                         margin = "marginalmeans", 
                                         ci_level = 0.95,
                                         vcov = mod.1b_vcov,
                                         type = "fixed") %>% data.frame() %>% mutate(model = "1b"))




# plots of predicted vaccination levels
theme <- theme_bw(base_family = "Myriad Pro", base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 11, margin = margin(1, 0, 0, 0, "mm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        plot.background = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin=margin(t = 0.5, r = 3, b = 1.5, l = 0, unit = "mm"))


pov_p <- pov_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  scale_fill_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  labs(x = "Poverty rate", y = "Predicted MCV1 coverage") + 
  coord_cartesian(ylim = c(83.7, 93.2), xlim = c(19.7, 76.2)) +
  scale_y_continuous(breaks = c(84, 86, 88, 90, 92, 94),
                     labels = c("84%", "86%", "88%", "90%", "92%", "94%")) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme +
  theme(axis.text.y = element_text(size = 11, color = "black", 
                                   margin = margin(1, 1, 1, 2, unit = "mm")),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))


urban_p <- urban_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  scale_fill_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  labs(x = "%Urban", y = NULL) + 
  coord_cartesian(ylim = c(83.7, 93.2), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) +  theme


clinic_p <- clinic_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  scale_fill_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  labs(x = "Clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(83.7, 93.2), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme



gdp_p <- gdp_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  scale_fill_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  labs(x = "GDP per capita", y = NULL) + 
  coord_cartesian(ylim = c(83.7, 93.2), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme



cbr_p <- cbr_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  scale_fill_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  labs(x = "Births per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(83.7, 93.2), xlim = c(9.54, 17.42)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme



gini_p <- gini_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  scale_fill_manual(values = c(`1a` = "#8a425f", `1b` = "#42498a")) +
  labs(x = "Gini index", y = NULL) + 
  coord_cartesian(ylim = c(83.7, 93.2), xlim = c(43.35, 59.48)) +
  scale_x_continuous(breaks = c(45, 50, 55, 60),
                     labels = c("0.45", "0.5", "0.55", "0.6")) + theme


legend <- get_legend(gini_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(name = "Time period", 
                     values = c(`1a` = "#8a425f", `1b` = "#42498a"),
                     labels = c(`1a` = "2006-2023", `1b` = "2018-2023")) +
  scale_fill_manual(name = "Time period", 
                    values = c(`1a` = "#8a425f", `1b` = "#42498a"),
                    labels = c(`1a` = "2006-2023", `1b` = "2018-2023")) + 
  theme() +
  theme(legend.text = element_text(size = 12, family = "Myriad Pro",
                                   margin = margin(0, 0, 0, 1, unit = "mm")),
        legend.background = element_blank(),
        legend.position = "top",
        legend.title.position = "left",
        legend.title = element_text(size = 12, family = "Myriad Pro",
                                    margin = margin(0, 5, 0, 12, unit = "mm")),
        legend.key.spacing.x = unit(5, "mm"),
        legend.key.size = unit(3, "mm")))



preds_p <- ggarrange(pov_p, cbr_p, gini_p, urban_p, clinic_p, gdp_p,
                     nrow = 1, widths = c(1.48, 1, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0, r = 1, b = -0.5, l = 3, unit = "mm"))


fig <- ggarrange(coef_p, NULL, legend, preds_p, nrow = 4, heights = c(1, 0.06, 0.1, 0.5)) +
  annotate("text", x = 0.01, y = 0.985, label = "A", family = "Myriad Pro Bold", size = 12 / .pt) +
  annotate("text", x = 0.01, y = 0.35, label = "B", family = "Myriad Pro Bold", size = 12 / .pt)

ggsave(filename = "~/Brazil-measles/figures/mod1_coefs_mod1ab_preds_v2.png", 
       plot = fig, height = 5.5, width = 7.25, units = "in", bg='white', dpi = 800)





################################################################################
##
## preds for muni FE models (much more time intensive)
##
## these seem to only work when restarting R between predict_response calls
##
################################################################################
library(tidyverse)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(jtools)

load("~/Brazil-measles/data/generated_Data/04.analysis_data_final.RData")

load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1c.RData")
load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1c_vcov.RData")

# model 1c
pov_pred_1c <- predict_response(mod.1c, 
                                terms = list(poverty_rate = seq(17.13, 78.78, 
                                                                length.out = 10)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = mod.1c_vcov,
                                type = "fixed",
                                rg.limit = 2000000)

#save(pov_pred_1c, file = "~/brazil_measles/preds/poverty_pred_1c.RData")
save(pov_pred_1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/poverty_pred_1c.RData")

#quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 33.14 92.87
urban_pred_1c <- predict_response(mod.1c, 
                                  terms = list(pct_urban = seq(33.14, 92.87, 
                                                               length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod.1c_vcov,
                                  type = "fixed",
                                  rg.limit = 2000000)

#save(urban_pred_1c, file = "~/brazil_measles/preds/urban_pred_1c.RData")
save(clinic_pred_1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/clinic_pred_1c.RData")

# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.09167864 0.54545004 
clinic_pred_1c <- predict_response(mod.1c, 
                                    terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                  length.out = 10)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod.1c_vcov,
                                    type = "fixed",
                                    rg.limit = 2000000)

#save(clinic_pred_1c, file = "~/brazil_measles/preds/clinic_pred_1c.RData")
save(clinic_pred_1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/clinic_pred_1c.RData")


# quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 4688.17 41242.88 
gdp_pred_1c <- predict_response(mod.1c, 
                                terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                          length.out = 10)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = mod.1c_vcov,
                                type = "fixed",
                                rg.limit = 2000000)

#save(gdp_pred_1c, file = "~/brazil_measles/preds/gdp_pred_1c.RData")
save(gdp_pred_1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/gdp_pred_1c.RData")


# quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
# 9.170367 17.785892
cbr_pred_1c <- predict_response(mod.1c, 
                                terms = list(cbr = seq(9.170367, 17.785892, length.out = 10)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = mod.1c_vcov,
                                type = "fixed",
                                rg.limit = 2000000) 

#save(cbr_pred_1c, file = "~/brazil_measles/preds/cbr_pred_1c.RData")
save(cbr_pred_1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/cbr_pred_1c.RData")


# quantile(df$gini, probs = c(0.1, 0.9), na.rm=T)
# 42.67 60.17
gini_pred_1c <- predict_response(mod.1c, 
                                 terms = list(gini = seq(42.67, 60.17, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod.1c_vcov,
                                 type = "fixed",
                                 rg.limit = 2000000)

# save(gini_pred_1c, file = "~/brazil_measles/preds/gini_pred_1c.RData")
save(gini_pred_1c, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/gini_pred_1c.RData")



# model 1d
load("~/Brazil-measles/data/generated_Data/04.analysis_data_final.RData")

load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1d.RData")
load("~/Brazil-measles/data/generated_Data/models/vaccination/mod.1d_vcov.RData")

pov_pred_1d <- predict_response(mod.1d, 
                                terms = list(poverty_rate = seq(17.13, 78.78, 
                                                                length.out = 10)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = mod.1d_vcov,
                                type = "fixed",
                                rg.limit = 2000000)

save(pov_pred_1d, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/poverty_pred_1d.RData")

#quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
# 33.14 92.87
urban_pred_1d <- predict_response(mod.1d, 
                                  terms = list(pct_urban = seq(33.14, 92.87, 
                                                               length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod.1d_vcov,
                                  type = "fixed",
                                  rg.limit = 2000000)

save(urban_pred_1d, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/urban_pred_1d.RData")

# quantile(df$clinics_pc, probs = c(0.1, 0.9), na.rm=T)
# 0.09167864 0.54545004 
clinic_pred_1d <-  predict_response(mod.1d, 
                                    terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                  length.out = 10)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod.1d_vcov,
                                    type = "fixed",
                                    rg.limit = 2000000)

save(clinic_pred_1d, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/clinic_pred_1d.RData")


# quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
# 4688.17 41242.88 
gdp_pred_1d <- predict_response(mod.1d, 
                                terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                          length.out = 10)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = mod.1d_vcov,
                                type = "fixed",
                                rg.limit = 2000000)

save(gdp_pred_1d, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/gdp_pred_1d.RData")


# quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
# 9.170367 17.785892
cbr_pred_1d <- predict_response(mod.1d, 
                                terms = list(cbr = seq(9.170367, 17.785892, length.out = 10)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = mod.1d_vcov,
                                type = "fixed",
                                rg.limit = 2000000) 

save(cbr_pred_1d, file = "~/Brazil-measles/data/generated_data/models/vaccination/preds/cbr_pred_1d.RData")




# plotting
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/cbr_pred_1c.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/clinic_pred_1c.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/gdp_pred_1c.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/gini_pred_1c.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/poverty_pred_1c.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/urban_pred_1c.RData")

load("~/Brazil-measles/data/generated_data/models/vaccination/preds/cbr_pred_1d.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/clinic_pred_1d.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/gdp_pred_1d.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/poverty_pred_1d.RData")
load("~/Brazil-measles/data/generated_data/models/vaccination/preds/urban_pred_1d.RData")


gdp_preds <- bind_rows(gdp_pred_1c %>% data.frame() %>% mutate(model = "1c"),
                       gdp_pred_1d %>% data.frame() %>% mutate(model = "1d"))

pov_preds <- bind_rows(pov_pred_1c %>% data.frame() %>% mutate(model = "1c"),
                       pov_pred_1d %>% data.frame() %>% mutate(model = "1d"))

clinic_preds <- bind_rows(clinic_pred_1c %>% data.frame() %>% mutate(model = "1c"),
                       clinic_pred_1d %>% data.frame() %>% mutate(model = "1d"))

urban_preds <- bind_rows(urban_pred_1c %>% data.frame() %>% mutate(model = "1c"),
                       urban_pred_1d %>% data.frame() %>% mutate(model = "1d"))

cbr_preds <- bind_rows(cbr_pred_1c %>% data.frame() %>% mutate(model = "1c"),
                       cbr_pred_1d %>% data.frame() %>% mutate(model = "1d"))

gini_pred_1c <- gini_pred_1c %>% data.frame() %>% mutate(model = "1c")


theme <- theme_bw(base_family = "Myriad Pro", base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 11, margin = margin(1, 0, 0, 0, "mm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        plot.background = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.margin=margin(t = 0.5, r = 3, b = 1.5, l = 0, unit = "mm"))


pov_p <- pov_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  scale_fill_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  labs(x = "Poverty rate", y = "Predicted MCV1 coverage") + 
  coord_cartesian(ylim = c(82, 98), xlim = c(19.7, 76.2)) +
  scale_y_continuous(breaks = c(82, 84, 86, 88, 90, 92, 94, 96, 98),
                     labels = c("82%", "84%", "86%", "88%", "90%", "92%", "94%", "96%", "98%")) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  guides(colour = guide_legend(nrow = 1)) +
  theme +
  theme(axis.text.y = element_text(size = 11, color = "black", 
                                   margin = margin(1, 1, 1, 2, unit = "mm")),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))


urban_p <- urban_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  scale_fill_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  labs(x = "%Urban", y = NULL) + 
  coord_cartesian(ylim = c(82, 98), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) +  theme


clinic_p <- clinic_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  scale_fill_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  labs(x = "Clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(82, 98), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme



gdp_p <- gdp_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  scale_fill_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  labs(x = "GDP per capita", y = NULL) + 
  coord_cartesian(ylim = c(82, 98), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme



cbr_p <- cbr_preds %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  scale_fill_manual(values = c(`1c` = "#8a425f", `1d` = "#42498a")) +
  labs(x = "Births per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(82, 98), xlim = c(9.54, 17.42)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme



gini_p <- gini_pred_1c %>% 
  ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  scale_color_manual(values = c(`1c` = "#8a425f")) +
  scale_fill_manual(values = c(`1c` = "#8a425f")) +
  labs(x = "Gini index", y = NULL) + 
  coord_cartesian(ylim = c(82, 98), xlim = c(43.35, 59.48)) +
  scale_x_continuous(breaks = c(45, 50, 55, 60),
                     labels = c("0.45", "0.5", "0.55", "0.6")) + theme


legend <- get_legend(pov_preds %>% 
                       ggplot(aes(x = x, y = predicted, color = model, fill = model)) +
                       geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, alpha = 0.5) +
                       geom_line(linewidth = 0.3) +
                       scale_color_manual(name = "Time period", 
                                          values = c(`1c` = "#8a425f", `1d` = "#42498a"),
                                          labels = c(`1c` = "2006-2023", `1d` = "2018-2023")) +
                       scale_fill_manual(name = "Time period", 
                                         values = c(`1c` = "#8a425f", `1d` = "#42498a"),
                                         labels = c(`1c` = "2006-2023", `1d` = "2018-2023")) + 
                       theme() +
                       theme(legend.text = element_text(size = 12, family = "Myriad Pro",
                                                        margin = margin(0, 0, 0, 1, unit = "mm")),
                             legend.background = element_blank(),
                             legend.position = "top",
                             legend.title.position = "left",
                             legend.title = element_text(size = 12, family = "Myriad Pro",
                                                         margin = margin(0, 5, 0, 12, unit = "mm")),
                             legend.key.spacing.x = unit(5, "mm"),
                             legend.key.size = unit(3, "mm")))


preds_p <- ggarrange(pov_p, cbr_p, urban_p, clinic_p, gdp_p, gini_p,
                     nrow = 1, widths = c(1.48, 1, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0, r = 0, b = -0.5, l = 1, unit = "mm"))


fig <- ggarrange(legend, preds_p, nrow = 2, heights = c(0.1, 1))


ggsave(filename = "~/Brazil-measles/figures/mod1c_1d_preds_v2.png", 
       plot = fig, height = 2.25, width = 7.25, units = "in", bg='white', dpi = 800)






























theme <- theme_bw(base_family = "Myriad Pro", base_size = 11) +
  theme(text = element_text(color = "black"),
        axis.text.x = element_text(size = 11, color = "black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 11, margin = margin(0, 0, 2, 0, "mm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        plot.background = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin=margin(t = 0.5, r = 3, b = 1.5, l = 0, unit = "mm"),
        legend.background = element_blank())


pov_p_1c <- pov_pred_1c %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate (%)", y = "Predicted MCV1 coverage") + 
  coord_cartesian(ylim = c(86, 98), xlim = c(19.7, 76.2)) +
  scale_y_continuous(breaks = c(86, 88, 90, 92, 94, 96, 98),
                     labels = c("86%", "88%", "90%", "92%", "94%", "96%", "98%")) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme +
  theme(axis.text.y = element_text(size = 11, color = "black", 
                                   margin = margin(1, 1, 1, 2, unit = "mm")),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))


gdp_p_1c <- gdp_pred_1c %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#115870", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "GDP PC", y = NULL) + 
  coord_cartesian(ylim = c(86, 98), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme


urban_p_1c <- urban_pred_1c %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% urban", y = NULL) + 
  coord_cartesian(ylim = c(86, 98), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) + theme



clinic_p_1c <- clinic_pred_1c %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#edbd91", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "Clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(86, 98), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme



cbr_p_1c <- cbr_pred_1c %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate", y = NULL) + 
  coord_cartesian(ylim = c(86, 98), xlim = c(9.54, 17.42)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme

gini_p_1c <- gini_pred_1c %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#ACC196", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Gini index", y = NULL) + 
  coord_cartesian(ylim = c(86, 98), xlim = c(43.35, 59.48)) +
  scale_x_continuous(breaks = c(45, 50, 55, 60),
                     labels = c("0.45", "0.5", "0.55", "0.6")) + theme



preds_1c <- ggarrange(pov_p_1c, cbr_p_1c, urban_p_1c, clinic_p_1c, gdp_p_1c, gini_p_1c,
                      nrow = 1, widths = c(1.4, 1, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0, r = -2, b = -2, l = 1, unit = "mm"))


# 1d
pov_p_1d <- pov_pred_1d %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Poverty rate (%)", y = "Predicted MCV1 coverage") + 
  coord_cartesian(ylim = c(81.8, 92.5), xlim = c(19.7, 76.2)) +
  scale_y_continuous(breaks = c(80, 82, 84, 86, 88, 90, 92),
                     labels = c("80%", "82%", "84%", "86%", "88%", "90%", "92%")) +
  scale_x_continuous(breaks = c(25, 50, 75)) +
  theme +
  theme(axis.text.y = element_text(size = 11, color = "black", 
                                   margin = margin(1, 1, 1, 2, unit = "mm")),
        axis.ticks.y = element_line(linewidth = 0.3, color = "black"))


gdp_p_1d <- gdp_pred_1d  %>% 
  data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#115870", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "GDP PC", y = NULL) + 
  coord_cartesian(ylim = c(81.8, 92.5), xlim = c(6200, 39700)) +
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                     labels = c("10", "20", "30", "40")) + theme


urban_p_1d <- urban_pred_1d %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "% urban", y = NULL) + 
  coord_cartesian(ylim = c(81.8, 92.5), xlim = c(35.6, 90.4)) +
  scale_x_continuous(breaks = c(40, 60, 80)) + theme



clinic_p_1d <- clinic_pred_1d %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#edbd91", alpha = 0.4) +
  geom_line(linewidth = 0.3) +
  labs(x = "Clinics per 1,000", y = NULL) + 
  coord_cartesian(ylim = c(81.8, 92.5), xlim = c(0.11, 0.527)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme



cbr_p_1d <- cbr_pred_1d %>% data.frame() %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linewidth = 0, fill ="#8783D1", alpha = 0.5) +
  geom_line(linewidth = 0.3) +
  labs(x = "Crude birth rate", y = NULL) + 
  coord_cartesian(ylim = c(81.8, 92.5), xlim = c(9.54, 17.42)) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme



preds_1d <- ggarrange(pov_p_1d, cbr_p_1d, urban_p_1d, clinic_p_1d, gdp_p_1d,
                      nrow = 1, widths = c(1.4, 1, 1, 1, 1, 1)) + 
  theme(plot.margin=margin(t = 0, r = -2, b = -2, l = 1, unit = "mm"))




fig <- ggarrange(preds_1c, NULL, preds_1d, ncol = 1, heights = c(1, 0.15, 1)) +
  theme(plot.margin=margin(t = 5, r = 1, b = 0, l = 0, unit = "mm")) +
  annotate("rect", xmin = 0.0788, xmax = 0.9942, ymin = 0.99, ymax = 1.04, 
           color = "black", fill = "gray90", linewidth = 0.2) +
  annotate("rect", xmin = 0.0788, xmax = 0.9942, ymin = 0.455, ymax = 0.505, 
           color = "black", fill = "gray90", linewidth = 0.2) +
  annotate("text", x = 0.53, y = 1.013, label = "2006–2023", family = "Myriad Pro", size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.48, label = "2018–2023", family = "Myriad Pro", size = 12 / .pt)

ggsave(filename = "~/Brazil-measles/figures/mod1c_1d_preds.png", 
       plot = fig, height = 3.6, width = 7.25, units = "in", bg='white', dpi = 800)


