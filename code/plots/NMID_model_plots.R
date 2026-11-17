library(tidyverse)
library(scales)
library(ggplot2)
library(ggpubr)
library(gtools)


################################################################################
##
## correlation plots for NMID mortality v. immune amnesia prevalence 
##
################################################################################

source("~/Brazil-measles/code/utils.R")
load("~/Brazil-measles/data/generated_data/04.analysis_data_final.RData")

# all ages
p1 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d1, y_col = nm_mx_total,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (1-year duration)", 
                     y_axis_text = 'Non-measles ID deaths\nper 10,000 (all ages)',
                     ylim = c(0.35, 40), label_pos = 33)

p2 <- correlation_plot_fun(df, plot = "NMID",
                     x_col = amnesia_prev_d2, y_col = nm_mx_total,
                     x_axis_text = "Immune amnesia prevalence\nper 10,000 (2-year duration)", 
                     y_axis_text = NULL,
                     ylim = c(0.35, 40), label_pos = 33)


p3 <- correlation_plot_fun(df, plot = "NMID",
                     x_col = amnesia_prev_d3, y_col = nm_mx_total,
                     x_axis_text = "Immune amnesia prevalence\nper 10,000 (3-year duration)", 
                     y_axis_text = NULL,
                     ylim = c(0.35, 40), label_pos = 33)

row1 <- ggarrange(p1, p2, p3, nrow = 1, widths = c(1.055, 1, 1))


# ages 1-29
p4 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d1, y_col = nm_mx_1to29,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (1-year duration)", 
                           y_axis_text = 'Non-measles ID deaths\nper 10,000 aged 1-29',
                           ylim = c(0.05, 13), label_pos = 10)

p5 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d2, y_col = nm_mx_1to29,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (2-year duration)", 
                           y_axis_text = NULL,
                           ylim = c(0.05, 13), label_pos = 10)


p6 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d3, y_col = nm_mx_1to29,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (3-year duration)", 
                           y_axis_text = NULL,
                           ylim = c(0.05, 13), label_pos = 10)

row2 <- ggarrange(p4, p5, p6, nrow = 1, widths = c(1.055, 1, 1))



# ages 1-19
p7 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d1, y_col = nm_mx_1to19,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (1-year duration)", 
                           y_axis_text = 'Non-measles ID deaths\nper 10,000 aged 1-19',
                           ylim = c(0.05, 13), label_pos = 10)

p8 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d2, y_col = nm_mx_1to19,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (2-year duration)", 
                           y_axis_text = NULL,
                           ylim = c(0.05, 13), label_pos = 10)


p9 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d3, y_col = nm_mx_1to19,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (3-year duration)", 
                           y_axis_text = NULL,
                           ylim = c(0.05, 13), label_pos = 10)

row3 <- ggarrange(p7, p8, p9, nrow = 1, widths = c(1.055, 1, 1))



# ages 1-9
p10 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d1, y_col = nm_mx_1to9,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (1-year duration)", 
                           y_axis_text = 'Non-measles ID deaths\nper 10,000 aged 1-9',
                           ylim = c(0.08, 23), label_pos = 17)

p11 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d2, y_col = nm_mx_1to9,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (2-year duration)", 
                           y_axis_text = NULL,
                           ylim = c(0.08, 23), label_pos = 17)


p12 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = amnesia_prev_d3, y_col = nm_mx_1to9,
                           x_axis_text = "Immune amnesia prevalence\nper 10,000 (3-year duration)", 
                           y_axis_text = NULL,
                           ylim = c(0.08, 23), label_pos = 17)

row4 <- ggarrange(p10, p11, p12, nrow = 1, widths = c(1.055, 1, 1))




fig <- ggarrange(row1, NULL, row2, NULL, row3, NULL, row4, 
                 nrow = 7, 
                 heights = c(1, 0.1, 1, 0.1, 1, 0.1, 1)) + 
  theme(plot.margin = margin(t = 2, r = -1, b = 1, l = 1, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/nmid_v_amnesia_corr.png", 
       plot = fig, height = 10, width = 8, units = "in", bg='white', dpi = 800)


rm(list = ls())




################################################################################
##
## prediction plots for baseline models
## 
################################################################################

theme <- theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black",
                                    margin = margin(0, 2, 0, 0, unit = "mm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        plot.background = element_blank(),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        plot.margin=margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
        legend.background = element_blank(),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "#e8e6eb", color = "black"))

################################################################################
##
## baseline models, all ages
## 
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f.RData")

#mean(df$nm_mx_total) * 10

preds_3abc <- rbind(pred_3a, pred_3b, pred_3c) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 4.706952, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 (all ages)") + 
  coord_cartesian(ylim = c(1.4, 5.7), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(2, 3, 4, 5), labels = c("2.0", "3.0", "4.0", "5.0")) +
  theme + facet_wrap(~duration)


preds_3def <- rbind(pred_3d, pred_3e, pred_3f) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 4.706952, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 (all ages)") + 
  coord_cartesian(ylim = c(1.4, 5.7), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(2, 3, 4, 5), labels = c("2.0", "3.0", "4.0", "5.0")) +
  theme + facet_wrap(~duration)


mod3_preds <- ggarrange(NULL, preds_3abc, NULL, preds_3def, nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.55, y = 0.96, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.55, y = 0.46, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)



################################################################################
##
## baseline models, ages 1-9
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f_1to9.RData")


# mean(df$nm_mx_1to9) * 10

preds_3abc_1to9 <- rbind(pred_3a_1to9, pred_3b_1to9, pred_3c_1to9) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 0.5839488, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-9") + 
  coord_cartesian(ylim = c(0.33, 1.55), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration)


preds_3def_1to9 <- rbind(pred_3d_1to9, pred_3e_1to9, pred_3f_1to9) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 0.5839488, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-9") + 
  coord_cartesian(ylim = c(0.33, 1.55), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration)


mod3_preds_1to9 <- ggarrange(NULL, preds_3abc_1to9, NULL, preds_3def_1to9, nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)



################################################################################
##
## baseline models, ages 1-19
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f_1to19.RData")

#mean(df$nm_mx_1to19) * 10

preds_3abc_1to19 <- rbind(pred_3a_1to19, pred_3b_1to19, pred_3c_1to19) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 0.4088635, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-19") + 
  coord_cartesian(ylim = c(0.2, 0.87), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration)


preds_3def_1to19 <- rbind(pred_3d_1to19, pred_3e_1to19, pred_3f_1to19) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 0.4088635, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-19") + 
  coord_cartesian(ylim = c(0.2, 0.87), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme + facet_wrap(~duration)


mod3_preds_1to19 <- ggarrange(NULL, preds_3abc_1to19, NULL, preds_3def_1to19, nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)




################################################################################
##
## baseline models, ages 1-29
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f_1to29.RData")

#mean(df$nm_mx_1to29) * 10

preds_3abc_1to29 <- rbind(pred_3a_1to29, pred_3b_1to29, pred_3c_1to29) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 0.4146143, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-29") + 
  coord_cartesian(ylim = c(0.25, 0.74), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration)


preds_3def_1to29 <- rbind(pred_3d_1to29, pred_3e_1to29, pred_3f_1to29) %>% 
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted)) +
  geom_hline(yintercept = 0.4146143, linetype = "dashed", color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#a29bab", alpha = 0.3, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-29") + 
  coord_cartesian(ylim = c(0.25, 0.74), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  #scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme + facet_wrap(~duration)


mod3_preds_1to29 <- ggarrange(NULL, preds_3abc_1to29, NULL, preds_3def_1to29, nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)






# organizing plots

mod3_between_p <- ggarrange(NULL, preds_3abc, preds_3abc_1to29, 
                            preds_3abc_1to19, preds_3abc_1to9, 
                            nrow = 5, heights = c(0.1, 1, 1, 1, 1))

mod3_within_p <- ggarrange(NULL, preds_3def, preds_3def_1to29, 
                           preds_3def_1to19, preds_3def_1to9, 
                           nrow = 5, heights = c(0.1, 1, 1, 1, 1))


mod3_all_preds_p <- ggarrange(mod3_between_p, mod3_within_p) +
  annotate("text", x = 0.275, y = 0.98, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.775, y = 0.98, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)

################################################################################
##
## interaction prediction plots: all ages
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f.RData")


preds_4abc <- rbind(pred_4a, pred_4b, pred_4c) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%  (10th percentile)", 
                                `48.5%` = "48.5%  (50th percentile)",
                                `78.8%` = "78.8%  (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%  (10th percentile)", 
                               `48.5%` = "48.5%  (50th percentile)",
                               `78.8%` = "78.8%  (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 (all ages)") + 
  coord_cartesian(ylim = c(0.8, 53), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                     labels = c("0.0", "10", "20", "30", "40", "50")) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.81, 0.77), 
        legend.title = element_text(size = 11, margin = margin(b = 1, unit = "mm")),
        legend.text = element_text(size = 10),
        legend.key.size = unit(5, "mm"),
        legend.margin = margin(t = 1, r = -7, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))



preds_4def <- rbind(pred_4d, pred_4e, pred_4f) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 (all ages)") + 
  coord_cartesian(ylim = c(0.8, 53), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                     labels = c("0.0", "10", "20", "30", "40", "50")) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))



mod4_preds_total <- ggarrange(NULL, preds_4abc, NULL, preds_4def, nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)




################################################################################
##
## interaction prediction plots: ages 1-9
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f_1to9.RData")

preds_4abc_1to9 <- rbind(pred_4a_1to9, pred_4b_1to9, pred_4c_1to9) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-9") + 
  coord_cartesian(ylim = c(0.1, 3.9), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("0.0", "1.0", "2.0", "3.0", "4.0")) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))



preds_4def_1to9 <- rbind(pred_4d_1to9, pred_4e_1to9, pred_4f_1to9) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-9") + 
  coord_cartesian(ylim = c(0.1, 3.9), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4),
                     labels = c("0.0", "1.0", "2.0", "3.0", "4.0")) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))


mod4_preds_1to9 <- ggarrange(NULL, preds_4abc_1to9, NULL, preds_4def_1to9, 
                             nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", 
           family = "Myriad Pro Bold",
           size = 12 / .pt)




################################################################################
##
## interaction prediction plots: ages 1-19
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f_1to19.RData")


preds_4abc_1to19 <- rbind(pred_4a_1to19, pred_4b_1to19, pred_4c_1to19) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-19") + 
  coord_cartesian(ylim = c(0.05, 2.1), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))


preds_4def_1to19 <- rbind(pred_4d_1to19, pred_4e_1to19, pred_4f_1to19) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-19") + 
  coord_cartesian(ylim = c(0.05, 2.1), xlim = c(4, 89)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))

mod4_preds_1to19 <- ggarrange(NULL, preds_4abc_1to19, NULL, preds_4def_1to19, 
                             nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", 
           family = "Myriad Pro Bold",
           size = 12 / .pt)






################################################################################
##
## interaction prediction plots: ages 1-29
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f_1to29.RData")


preds_4abc_1to29 <- rbind(pred_4a_1to29, pred_4b_1to29, pred_4c_1to29) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-29") + 
  coord_cartesian(ylim = c(0.16, 1.37), xlim = c(4, 89)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))


preds_4def_1to29 <- rbind(pred_4d_1to29, pred_4e_1to29, pred_4f_1to29) %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x * 10, # prevalence is per 1,000 in df
             y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1%    (10th percentile)", 
                                `48.5%` = "48.5%    (50th percentile)",
                                `78.8%` = "78.8%    (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1%    (10th percentile)", 
                               `48.5%` = "48.5%    (50th percentile)",
                               `78.8%` = "78.8%    (90th percentile)")) +
  labs(x = "Immune amnesia prevalence per 10,000",
       y = "Predicted NMID deaths\nper 10,000 aged 1-29") + 
  coord_cartesian(ylim = c(0.16, 1.37), xlim = c(4, 89)) +
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
  theme + facet_wrap(~duration) + 
  theme(legend.position = c(0.122, 0.75), 
        legend.margin = margin(t = 1, r = -10, b = 1, l = 1, unit = "mm"),
        legend.background = element_rect(fill = "white", color = "black", 
                                         linewidth = 0.2))

mod4_preds_1to29 <- ggarrange(NULL, preds_4abc_1to29, NULL, preds_4def_1to29, 
                              nrow = 4, heights = c(0.1, 1, 0.1, 1)) +
  annotate("text", x = 0.53, y = 0.96, label = "Between municipalities", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.53, y = 0.46, label = "Within-municipality", 
           family = "Myriad Pro Bold",
           size = 12 / .pt)





legend <- get_legend(pred_4d_1to29 %>% 
  mutate(poverty_rate = case_when(group == 17.13 ~ "17.1%",
                                  group == 48.53 ~ "48.5%",
                                  group == 78.78 ~ "78.8%")) %>%
  ggplot(aes(x = x, y = predicted, color = poverty_rate, fill = poverty_rate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(name = "Poverty rate", 
                     values = c(`17.1%` = "#D1603D",
                                `48.5%` = "#a3039b",
                                `78.8%` = "#4B3F72"),
                     labels = c(`17.1%` = "17.1% (10th percentile)", 
                                `48.5%` = "48.5% (50th percentile)",
                                `78.8%` = "78.8% (90th percentile)")) +
  scale_fill_manual(name = "Poverty rate", 
                    values = scales::alpha(c(`17.1%` = "#d45202",
                                             `48.5%` = "#db4fd4",
                                             `78.8%` = "#4B3F72"), 0.9),
                    labels = c(`17.1%` = "17.1% (10th percentile)", 
                               `48.5%` = "48.5% (50th percentile)",
                               `78.8%` = "78.8% (90th percentile)")) +
    theme + 
    theme(legend.text = element_text(size = 12, family = "Myriad Pro",
                                     margin = margin(0, 0, 0, 1, unit = "mm")),
          legend.background = element_blank(),
          legend.position = "top",
          legend.title.position = "left",
          legend.title = element_text(size = 12, family = "Myriad Pro",
                                      margin = margin(0, 5, 0, 12, unit = "mm")),
          legend.key.spacing.x = unit(6, "mm"),
          legend.key.size = unit(3, "mm")))




# organizing plots
mod4_between_p <- ggarrange(preds_4abc + theme(legend.position = "none"),
                            preds_4abc_1to29 + theme(legend.position = "none"), 
                            preds_4abc_1to19 + theme(legend.position = "none"),
                            preds_4abc_1to9 + theme(legend.position = "none"), 
                            nrow = 4) +
  theme(plot.margin = margin(-2, 0, 0, 0, unit = "mm"))

mod4_within_p <- ggarrange(preds_4def + theme(legend.position = "none"),
                           preds_4def_1to29 + theme(legend.position = "none"), 
                           preds_4def_1to19 + theme(legend.position = "none"), 
                           preds_4def_1to9 + theme(legend.position = "none"), 
                           nrow = 4) +
  theme(plot.margin = margin(-2, 0, 0, 0, unit = "mm"))

mod4_all_preds_p <- ggarrange(legend, NULL, ggarrange(mod4_between_p, mod4_within_p), 
          ncol = 1, heights = c(0.03, 0.03, 1)) +
  annotate("text", x = 0.275, y = 0.96, label = "Between municipalities", family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.775, y = 0.96, label = "Within-municipality", family = "Myriad Pro Bold",
           size = 12 / .pt)

mod3_all_preds_p
mod4_all_preds_p


ggsave(filename = "~/Brazil-measles/figures/mod3_all_preds.png", 
       plot = mod3_all_preds_p, height = 7, width = 10, units = "in", bg='white', dpi = 800)


ggsave(filename = "~/Brazil-measles/figures/mod4_all_preds.png", 
       plot = mod4_all_preds_p, height = 7, width = 10, units = "in", bg='white', dpi = 800)


################################################################################
##
## plots for main text
##
################################################################################


mod3_between <- ggarrange(preds_3abc + 
                            theme(plot.margin = margin(0, 0, 5, 0, unit = "mm")),
                          preds_3abc_1to29 + coord_cartesian(ylim = c(0.29, 0.75), xlim = c(4, 89)) + 
                            theme(plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                          preds_3abc_1to19 + 
                            theme(plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                          preds_3abc_1to9 + coord_cartesian(ylim = c(0.3, 1.3), xlim = c(4, 89)) +
                            scale_y_continuous(breaks = c(0.3, 0.6, 0.9, 1.2)) +
                            theme(plot.margin = margin(0, 0, 0, 0, unit = "mm")), 
                          nrow = 4) +
  theme(plot.margin = margin(2, 1, 1, 1, unit = "mm")) +
  annotate("text", x = 0.01, y = c(1.002, 0.755, 0.505, 0.243), label = c("A", "B", "C", "D"), family = "Myriad Pro Bold",
           size = 12 / .pt)

ggsave(filename = "~/Brazil-measles/figures/mod3_between.png", 
       plot = mod3_between, height = 8.5, width = 7.25, units = "in", bg='white', dpi = 800)




mod4_between <- ggarrange(legend, NULL,
                          preds_4abc + 
                            theme(legend.position = "none",
                                  strip.text.x = element_text(margin = margin(t = 1, b = 1, unit = "mm")),
                                  plot.margin = margin(0, 0, 5, 0, unit = "mm")),
                          preds_4abc_1to29 + 
                            theme(legend.position = "none",
                                  strip.text.x = element_text(margin = margin(t = 1, b = 1, unit = "mm")),
                                  plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                          preds_4abc_1to19 + 
                            coord_cartesian(ylim = c(0, 1.85), xlim = c(4, 89)) + 
                            theme(legend.position = "none",
                                  strip.text.x = element_text(margin = margin(t = 1, b = 1, unit = "mm")),
                                  plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                          preds_4abc_1to9 + 
                            coord_cartesian(ylim = c(0, 3.5), xlim = c(4, 89)) + 
                            theme(legend.position = "none",
                                  strip.text.x = element_text(margin = margin(t = 1, b = 1, unit = "mm")),
                                  plot.margin = margin(0, 0, 0, 0, unit = "mm")), 
                          nrow = 6, heights = c(0.04, 0.09, 1, 1, 1, 1)) +
  theme(plot.margin = margin(2, 2, 1, 1, unit = "mm")) +
  annotate("text", x = 0.01, y = c(0.98, 0.74, 0.497, 0.243), 
           label = c("A", "B", "C", "D"), family = "Myriad Pro Bold",
           size = 12 / .pt)

ggsave(filename = "~/Brazil-measles/figures/mod4_between.png", 
       plot = mod4_between, height = 8.5, width = 7.25, units = "in", bg='white', dpi = 800)





mod3_within <- ggarrange(preds_3def + coord_cartesian(ylim = c(1.2, 5), xlim = c(4, 89)) + 
                           theme(plot.margin = margin(0, 0, 5, 0, unit = "mm")),
                         preds_3def_1to29 + coord_cartesian(ylim = c(0.25, 0.6), xlim = c(4, 89)) + 
                           theme(plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                         preds_3def_1to19 + 
                           theme(plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                         preds_3def_1to9 + 
                           theme(plot.margin = margin(0, 0, 0, 0, unit = "mm")), 
                         nrow = 4) +
  theme(plot.margin = margin(2, 1, 1, 1, unit = "mm")) +
  annotate("text", x = 0.01, y = c(1.002, 0.755, 0.505, 0.243), 
           label = c("A", "B", "C", "D"), family = "Myriad Pro Bold",
           size = 12 / .pt)

ggsave(filename = "~/Brazil-measles/figures/mod3_within.png", 
       plot = mod3_within, height = 8.5, width = 7.25, units = "in", bg='white', dpi = 800)





mod4_within <- ggarrange(legend, NULL,
                         preds_4def +  coord_cartesian(ylim = c(0, 36), xlim = c(4, 89)) +
                                 theme(legend.position = "none",
                                       plot.margin = margin(0, 0, 5, 0, unit = "mm")),
                               preds_4def_1to29  + coord_cartesian(ylim = c(0.15, 1.12), xlim = c(4, 89)) + 
                                 theme(legend.position = "none",
                                       plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                               preds_4def_1to19 + 
                                 coord_cartesian(ylim = c(0, 2.1), xlim = c(4, 89)) + 
                                 theme(legend.position = "none",
                                       plot.margin = margin(0, 0, 5, 0, unit = "mm")), 
                               preds_4def_1to9 + 
                                 coord_cartesian(ylim = c(0, 4), xlim = c(4, 89)) + 
                                 theme(legend.position = "none",
                                       plot.margin = margin(0, 0, 0, 0, unit = "mm")), 
                               nrow = 6, heights = c(0.04, 0.09, 1, 1, 1, 1)) +
  theme(plot.margin = margin(2, 2, 1, 1, unit = "mm")) +
  annotate("text", x = 0.01, y = c(0.98, 0.735, 0.495, 0.243), 
           label = c("A", "B", "C", "D"), family = "Myriad Pro Bold",
           size = 12 / .pt)


ggsave(filename = "~/Brazil-measles/figures/mod4_within.png", 
       plot = mod4_within, height = 8.5, width = 7.25, units = "in", bg='white', dpi = 800)













# old

fig <- ggarrange(NULL, preds_3def, NULL, preds_3def_1to9, NULL, preds_4abc, ncol = 1,
                 heights = c(0.05, 1, 0.05, 1, 0.05, 1.25),
                 align = "hv") +
  annotate("text", x = 0.54, y = 0.989, label = "Within-municipality", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.54, y = 0.68, label = "Within-municipality", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.54, y = 0.372, label = "Between municipalities", 
           family = "Myriad Pro Bold",
           size = 12 / .pt)  +
  annotate("text", x = 0.02, y = 0.989, label = "A", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.02, y = 0.68, label = "B", 
           family = "Myriad Pro Bold",
           size = 12 / .pt) +
  annotate("text", x = 0.02, y = 0.372, label = "C", 
           family = "Myriad Pro Bold",
           size = 12 / .pt)

ggsave(filename = "~/Brazil-measles/figures/amnesia_main.png", 
       plot = fig, height = 7.5, width = 7.5, units = "in", bg='white', dpi = 800)







################################################################################



theme <- theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        plot.background = element_blank(),
        axis.ticks = element_line(linewidth = 0.3, color = "black"),
        plot.margin=margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "mm"),
        legend.background = element_blank(),
        legend.position = c(0.2, 0.75),
        legend.key.spacing.y = unit(1, "mm"))



fig <- ggarrange(mod3abc_p, NULL, pred_4c, nrow = 3, heights = c(0.6, 0.06, 1)) +
  annotate("text", x = 0.015, y = 0.98, label = "A", family = "Myriad Pro Bold")  +
  annotate("text", x = 0.015, y = 0.59, label = "B", family = "Myriad Pro Bold") +
  theme(plot.margin = margin(0, 0, 1, 0, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/mod3abc_4c_preds.png", 
       plot = fig, height = 6, width = 7.25, units = "in", bg='white', dpi = 800)




