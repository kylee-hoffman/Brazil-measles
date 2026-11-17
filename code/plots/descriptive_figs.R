library(tidyverse)
library(janitor)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(viridis)
library(scales)
library("sf")
library(patchwork)
library(corrplot)


################################################################################
##
##
## finding best lagged measles incidence
##
##
################################################################################
source("~/Brazil-measles/code/utils.R")

load("~/Brazil-measles/data/generated_data/02.analysis_data.RData")

# mortality for ages 1-9
cor.test(df$mv_incid_total, df$nm_mx_1to9)      # -0.0024051
cor.test(df$mv_incid_total_lag1, df$nm_mx_1to9) # -0.0006102
cor.test(df$mv_incid_total_lag2, df$nm_mx_1to9) # -0.0011676
cor.test(df$mv_incid_total_lag3, df$nm_mx_1to9) #  0.008024371
cor.test(df$mv_incid_total_lag4, df$nm_mx_1to9) #  0.001195252
cor.test(df$mv_incid_total_lag5, df$nm_mx_1to9) #  0.006492526

# mortality for ages 1-14
cor.test(df$mv_incid_total, df$nm_mx_1to14)      # -0.0009888
cor.test(df$mv_incid_total_lag1, df$nm_mx_1to14) # -0.0006655
cor.test(df$mv_incid_total_lag2, df$nm_mx_1to14) # -0.0005245
cor.test(df$mv_incid_total_lag3, df$nm_mx_1to14) #  0.0081482
cor.test(df$mv_incid_total_lag4, df$nm_mx_1to14) #  0.0012909
cor.test(df$mv_incid_total_lag5, df$nm_mx_1to14) #  0.0058642


# mortality for all ages
cor.test(df$mv_incid_total, df$nm_mx_total)      # -0.0007988
cor.test(df$mv_incid_total_lag1, df$nm_mx_total) # -0.0033647
cor.test(df$mv_incid_total_lag2, df$nm_mx_total) #  0.0008388
cor.test(df$mv_incid_total_lag3, df$nm_mx_total) #  0.0127636
cor.test(df$mv_incid_total_lag4, df$nm_mx_total) #  0.0113815
cor.test(df$mv_incid_total_lag5, df$nm_mx_total) # -0.0008554




# correlation plots for incidence and nmid mortality for all ages
p0 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = mv_incid_total, y_col = nm_mx_total,
                           x_axis_text = "Measles cases per 10,000", 
                           y_axis_text = 'Non-measles ID deaths per 10,000',
                           ylim = c(0.036, 4), label_pos = 3.3)

p1 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = mv_incid_total_lag1, y_col = nm_mx_total,
                           x_axis_text = "Measles cases per 10,000 1 year prior", y_axis_text = NULL,
                           ylim = c(0.036, 4), label_pos = 3.3)

p2 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = mv_incid_total_lag2, y_col = nm_mx_total,
                           x_axis_text = "Measles cases per 10,000 2 years prior", y_axis_text = NULL,
                           ylim = c(0.036, 4), label_pos = 3.3)

p3 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = mv_incid_total_lag3, y_col = nm_mx_total,
                           x_axis_text = "Measles cases per 10,000 3 years prior", 
                           y_axis_text = 'Non-measles ID deaths per 10,000',
                           ylim = c(0.036, 4), label_pos = 3.3)

p4 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = mv_incid_total_lag4, y_col = nm_mx_total,
                           x_axis_text = "Measles cases per 10,000 4 years prior", y_axis_text = NULL,
                           ylim = c(0.036, 4), label_pos = 3.3)

p5 <- correlation_plot_fun(df, plot = "NMID",
                           x_col = mv_incid_total_lag5, y_col = nm_mx_total,
                           x_axis_text = "Measles cases per 10,000 5 years prior", y_axis_text = NULL,
                           ylim = c(0.036, 4), label_pos = 3.3)

fig <- ggarrange(p0, p1, p2, 
                 NULL, NULL, NULL, 
                 p3, p4, p5,
                 nrow = 3, ncol = 3,
                 widths = c(1.055, 1, 1),
                 heights = c(1, 0.05, 1)) +
  theme(plot.margin = margin (1, 1, 1, 1, "mm"))

ggsave(filename = "~/Brazil-measles/figures/nmid_v_lagged_cases.png", 
       plot = fig, height = 6, width = 9, units = "in", bg='white', dpi = 800)

rm(list = ls())

################################################################################
##
##
## sensitivity test - cancer instead of nmid deaths
##
##
################################################################################
source("~/Brazil-measles/code/utils.R")
load('~/Brazil-measles/data/generated_data/02.analysis_data.RData')

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
cancer <- read_datasus("~/Brazil-measles/data/mortality/all_cancer_deaths.csv",
                       line_skip = 5, drop_cols = "total", values_to = "cancer_deaths") 


dat <- df %>% merge(cancer, by = c("muni_code_6", "year")) %>% 
  mutate(cancer_mx = cancer_deaths / pop_total * 1000)


cor.test(dat$mv_incid_total, dat$cancer_mx)      # -0.01037754 
cor.test(dat$mv_incid_total_lag1, dat$cancer_mx) # -0.01061673
cor.test(dat$mv_incid_total_lag2, dat$cancer_mx) # -0.008021512
cor.test(dat$mv_incid_total_lag3, dat$cancer_mx) # -0.00378682
cor.test(dat$mv_incid_total_lag4, dat$cancer_mx) #  0.002069004
cor.test(dat$mv_incid_total_lag5, dat$cancer_mx) # -0.007480575

# correlation plots for incidence and cancer mortality for all ages
p0 <- correlation_plot_fun(dat, plot = "cancer",
                           x_col = mv_incid_total, y_col = cancer_mx,
                           lag_string = NULL, y_axis_text = 'Cancer deaths per 1,000')

p1 <- correlation_plot_fun(dat, plot = "cancer",
                           x_col = mv_incid_total_lag1, y_col = cancer_mx,
                           lag_string = "1 year prior", y_axis_text = NULL)

p2 <- correlation_plot_fun(dat, plot = "cancer",
                           x_col = mv_incid_total_lag2, y_col = cancer_mx,
                           lag_string = "2 years prior", y_axis_text = NULL)

p3 <- correlation_plot_fun(dat, plot = "cancer",
                           x_col = mv_incid_total_lag3, y_col = cancer_mx,
                           lag_string = "3 years prior", y_axis_text = 'Cancer deaths per 1,000')

p4 <- correlation_plot_fun(dat, plot = "cancer",
                           x_col = mv_incid_total_lag4, y_col = cancer_mx,
                           lag_string = "4 years prior", y_axis_text = NULL)

p5 <- correlation_plot_fun(dat, plot = "cancer",
                           x_col = mv_incid_total_lag5, y_col = cancer_mx,
                           lag_string = "5 years prior", y_axis_text = NULL)

fig <- ggarrange(p0, p1, p2, 
                 NULL, NULL, NULL, 
                 p3, p4, p5,
                 nrow = 3, ncol = 3,
                 widths = c(1.1, 1, 1),
                 heights = c(1, 0.07, 1)) +
  theme(plot.margin = margin (1, 1, 1, 1, "mm"))


ggsave(filename = "~/Brazil-measles/figures/cancer_v_lagged_cases.png", 
       plot = fig, height = 6, width = 9, units = "in", bg='white', dpi = 800)

rm(list = ls())





################################################################################
##
## correlation matrix
##
################################################################################
load("~/Brazil-measles/data/generated_data/04.analysis_data_final.RData")

df$year_n <- as.numeric(as.character(df$year))

cor_df <- df %>% dplyr::select('MCV1 coverage (2-year lag)' = mcv_d1_cov_tc_lag2, 
                               'Measles incidence' = mv_incid_total, 
                               'NMID mortality rate (total)' = nm_mx_total,
                               'NMID mortality rate (1-9)' = nm_mx_1to9,
                               'Crude birth rate' = cbr, 
                               'Crude death rate' = cdr,
                               'Infant mortality rate' = imr,
                               'GDP per capita' = gdp_pc, 
                               'Clinics per capita' = clinics_pc,
                               'Poverty rate' = poverty_rate, 
                               'Gini index' = gini, 
                               '%Urban' = pct_urban,
                               'Year' = year_n)

cor_matrix <- cor(cor_df)

png(filename = "~/Brazil-measles/figures/correlation_matrix_plot.png", width = 5000, height = 5000, res = 600)

corrplot(cor_matrix, method = "color", tl.col = "black")

dev.off()



################################################################################
##
## state-year heat maps (ordered by pop density)
##
################################################################################

theme <- theme_minimal(base_size = 12, base_family = "Myriad Pro") +
  theme(text = element_text(color = "black"),
        axis.title.x = element_text(size = 12, margin = margin(b = 0, t = -1, unit = "mm")),
        axis.title.y = element_text(size = 12, margin = margin(r = -5, l=0, unit = "mm")),
        axis.text.x = element_text(size = 10, angle = 60, vjust = 1.5, hjust = 1.3, color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, linewidth=0.5),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12, margin = margin(b=1, unit = "mm")),
        legend.text = element_text(size = 12, vjust = 0, margin = margin(t = 0.2, b = 0, unit = "mm")),
        legend.position = "top", 
        legend.title.position = "top",
        legend.margin = margin(b = -3, unit = "mm"),
        axis.ticks.x = element_line(linewidth = 0.4),
        plot.margin = margin(t=0.1, r=0.8, b=0.1, l=0.8, unit = "mm"))


p_cov <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          mean_cov = mean(mcv_d1_cov_tc)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = mean_cov)) +
  scale_fill_gradient2(low = "#fcfafc", mid = "#d7b2f7", high = "#300963", 
                       midpoint = 80,
                      limits = c(55, 100),
                      na.value = "transparent", name = "Mean MCV1 coverage (%)",
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = "black",
                                             nbin = 500,
                                             barwidth = 10,
                                             barheight = 0.6, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2006.5, 2022.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme + 
  theme(axis.text.y = element_text(size = 10, color = "black", hjust = 1))


p_incid <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          incid_tot = mean(mv_incid_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = incid_tot)) +
  scale_fill_gradient2(low = "#f7fcf9", mid = "#b9e2fa", high = "#074f78", 
                       midpoint = 2.5,
                       limits = c(0, 0.54),
                      trans = pseudo_log_trans(sigma = 1e-6, base = 10),
                      breaks = c(0, 0.0001, 0.01, 0.5),
                      labels = c("0", "0.0001", "0.01", "0.5"),
                      na.value = "white", name = "Mean MV cases per 1,000", # log scale
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = "black",
                                             nbin = 500,
                                             barwidth = 10,
                                             barheight = 0.6, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2006.5, 2022.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme


p_mort <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          nm_mx_tot = mean(nm_mx_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = nm_mx_tot)) +
  scale_fill_gradient2(low = "#f6f0e6", mid = "#f28b88", high = "#49006b", 
                       midpoint = 0.45,
                       limits = c(0.065, 0.89),
                       breaks = c(0, 0.25, 0.5, 0.75),
                       labels = c("0.0", "0.25", "0.5", "0.75"),
                       na.value = "transparent", name = "Mean NMID deaths per 1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              nbin = 200,
                                              barwidth = 10,
                                              barheight = 0.6, 
                                              frame.linewidth = 0.3,  
                                              direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2006.5, 2022.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.title = element_text(hjust = 0.5))

fig <- ggarrange(p_cov, p_incid, p_mort, nrow = 1, widths = c(1.57, 1, 1)) +
  theme(plot.margin = margin(0, -0.5, 0.2, -0.5, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/state_heatmaps_density.png", 
       plot = fig, height = 4.3, width = 8.2, unit = "in", bg='#ffffff', dpi = 800)


################################################################################
##
## state-year heat maps (ordered by latitude)
##
################################################################################

lats <- read.delim('/Users/kyhoff/Brazil-measles/data/state_latitudes.txt', sep = "\t") %>% 
  arrange(latitude)

lat_order <- lats$state

p_cov <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          mean_cov = mean(mcv_d1_cov_tc)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = mean_cov)) +
  scale_fill_gradient2(low = "#fcfafc", mid = "#d7b2f7", high = "#300963", 
                       midpoint = 80,
                      limits = c(55, 100),
                      na.value = "transparent", name = "Mean MCV1 coverage (%)",
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = "black",
                                             nbin = 500,
                                             barwidth = 10,
                                             barheight = 0.6, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2006.5, 2022.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme + 
  theme(axis.text.y = element_text(size = 10, color = "black", hjust = 1))

p_incid <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          incid_tot = mean(mv_incid_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = incid_tot)) +
  scale_fill_gradient2(low = "#f7fcf9", mid = "#b9e2fa", high = "#074f78", 
                       midpoint = 2.5,
                       limits = c(0, 0.54),
                       trans = pseudo_log_trans(sigma = 1e-6, base = 10),
                       breaks = c(0, 0.0001, 0.01, 0.5),
                       labels = c("0", "0.0001", "0.01", "0.5"),
                      na.value = "white", name = "Mean MV incidence per 1,000", # log scale
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = "black",
                                             nbin = 500,
                                             barwidth = 10,
                                             barheight = 0.6, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2006.5, 2022.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.title = element_text(hjust = 0.5))


p_mort <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          nm_mx_tot = mean(nm_mx_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = nm_mx_tot)) +
  scale_fill_gradient2(low = "#f6f0e6", mid = "#f28b88", high = "#49006b", 
                       midpoint = 0.45,
                       limits = c(0.065, 0.89),
                       breaks = c(0, 0.25, 0.5, 0.75),
                       labels = c("0.0", "0.25", "0.5", "0.75"),
                       na.value = "transparent", name = "Mean NMID Deaths per 1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black",
                                              nbin = 500,
                                              barwidth = 10,
                                              barheight = 0.6, 
                                              frame.linewidth = 0.3,  
                                              direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2006.5, 2022.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.title = element_text(hjust = 0.5))


fig <- ggarrange(p_cov, p_incid, p_mort, nrow = 1, widths = c(1.57, 1, 1)) +
  theme(plot.margin = margin(0, -0.5, 0.2, -0.5, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/state_heatmaps_latitude.png", 
       plot = fig, height = 4.3, width = 8.2, bg='#ffffff', dpi = 800)



################################################################################
##
## age-specific incidence
##
################################################################################
pop <- read.delim("~/Brazil-measles/data/pop/age_structured_pop.csv",
                  sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 11) %>% 
  clean_names() %>% 
  pivot_longer(cols = -"faixa_etaria_1", names_to = "year", values_to = "pop") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         age = factor(faixa_etaria_1,
                      levels = c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                                 "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos", 
                                 "60 a 64 anos", "65 a 69 anos", "70 a 79 anos", "80 anos e mais"),
                      labels = c("0-4", "5-9", "10-14", "15-19",
                                 "20-29", "30-39", "40-49", "50-59", 
                                 "60-64", "65-69", "70-79", "80+"))) %>% 
  dplyr::select(-faixa_etaria_1)


cases_ages1 <- read.delim("~/Brazil-measles/data/measles_cases/age_structured_cases_07-25_sinannet_cnv.csv", 
                         sep = ";", fileEncoding = "Latin1", skip = 5, nrow = 14) %>%
  clean_names() %>% 
  dplyr::select(-c(em_branco_ign, x_1975, x2001, x2006, total)) %>%
  filter(!str_detect(faixa_etaria, "Em")) %>% 
  mutate(across(contains("x2"), ~as.numeric(ifelse(.x == "-", 0, .x))))


cases_ages2 <- read.delim("~/Brazil-measles/data/measles_cases/age_structured_cases_02-06_sinanwin_cnv.csv", 
                         sep = ";", fileEncoding = "Latin1", skip = 5, nrow = 14) %>%
  clean_names() %>% 
  dplyr::select(-total) %>%
  filter(!str_detect(faixa_etaria, "Em")) %>% 
  mutate(across(contains("x2"), ~as.numeric(ifelse(.x == "-", 0, .x))))


cases_ages <- merge(cases_ages1, cases_ages2, by = "faixa_etaria") %>% mutate(x2004 = 0)


# pop data is 0-4. need to aggregate <1 and 1-4 cases
combined <- cases_ages %>%
  filter(faixa_etaria %in% c("<1 Ano", "1-4")) %>%
  summarise(across(-faixa_etaria, sum)) %>%
  mutate(faixa_etaria = "0-4")

incid_ages <- cases_ages %>% 
  filter(!faixa_etaria %in% c("<1 Ano", "1-4")) %>% 
  bind_rows(combined) %>% 
  mutate(age = factor(faixa_etaria,
                      levels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39",
                                 "40-49", "50-59", "60-64", "65-69", "70-79", "80 e +"),
                      labels = c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39", 
                                 "40-49", "50-59", "60-64", "65-69", "70-79", "80+"))) %>% 
  dplyr::select(-faixa_etaria) %>% 
  pivot_longer(cols = -"age", names_to = "year", values_to = "cases") %>% 
  mutate(year = as.numeric(substr(year, 2, 5))) %>% 
  merge(pop, by = c("age", "year")) %>% 
  mutate(incid = cases / pop * 1000)


breaks <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.4633)
trans <- pseudo_log_trans()
vals <- rescale(trans$transform(breaks))

fig <- incid_ages %>% 
  ggplot(aes(x = year, y = age)) +
  geom_tile(aes(fill = incid)) +
  scale_fill_gradientn(colours = c("white", "#fcfcfa", "#DEF5E5FF", "#ADE3C0FF", "#6CD3ADFF","#43BBADFF",
                                   "#35A1ABFF", "#3487A6FF", "#366DA0FF", "#3D5296FF", "#050391"),
                       name = "Measles incidence\nper 1,000",
                       values = vals,
                       transform = trans,
                       breaks = breaks,
                       guide = guide_coloursteps(frame.colour = "black",                            
                                                 even.steps = T,
                                                # show.limits = T,
                                                 barwidth = 1,
                                                 barheight = 12,
                                                 frame.linewidth = 0.2,
                                                 direction = 'vertical'),
                       labels = c("0.0", expression(10^-5), expression(10^-4), expression(10^-3), 
                                  expression(10^-2), 0.1, 0.2, 0.3, 0.4, 0.4633)
                       ) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.6, 2024.4), ylim = c(1.1, 9.9)) +
  labs(x= "Year", y = "Age group") +
  theme_bw(base_family = "Myriad Pro", base_size = 12) +
  theme(text = element_text(color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 60, vjust = 1.3, hjust = 1.3),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 0.3),
        axis.ticks = element_line(size = 0.3, color = "black"),
        plot.margin=margin(t = 1, r = 3, b = 1, l = 1, unit = "mm"))


ggsave(filename = "~/Brazil-measles/figures/age_structured_incidence.png", 
       plot = fig, height = 4, width = 7, units = "in", bg='white', dpi = 800)


rm(list = ls())




################################################################################
##
##
## correlating vaccination and cases + finding best lagged coverage
##
##
################################################################################
load("~/Brazil-measles/data/generated_data/04.analysis_data_final.RData")

# correlation plots for incidence and cancer mortality for all ages
p0 <- correlation_plot_vax_fun(df, plot = "cases/vax",
                           x_col = mcv_d1_cov_tc, y_col = mv_incid_total,
                           lag_string = NULL, y_axis_text = 'Measles cases per 1,000')

p1 <- correlation_plot_vax_fun(df, plot = "cases/vax",
                           x_col = mcv_d1_cov_tc_lag1, y_col = mv_incid_total,
                           lag_string = "1 year prior", y_axis_text = NULL)

p2 <- correlation_plot_vax_fun(df, plot = "cases/vax",
                           x_col = mcv_d1_cov_tc_lag2, y_col = mv_incid_total,
                           lag_string = "2 years prior", y_axis_text = NULL)

p3 <- correlation_plot_vax_fun(df, plot = "cases/vax",
                           x_col = mcv_d1_cov_tc_lag3, y_col = mv_incid_total,
                           lag_string = "3 years prior", y_axis_text = 'Measles cases per 1,000')

p4 <- correlation_plot_vax_fun(df, plot = "cases/vax",
                           x_col = mcv_d1_cov_tc_lag4, y_col = mv_incid_total,
                           lag_string = "4 years prior", y_axis_text = NULL)

p5 <- correlation_plot_vax_fun(df, plot = "cases/vax",
                           x_col = mcv_d1_cov_tc_lag5, y_col = mv_incid_total,
                           lag_string = "5 years prior", y_axis_text = NULL)

fig <- ggarrange(p0, p1, p2, 
                 NULL, NULL, NULL, 
                 p3, p4, p5,
                 nrow = 3, ncol = 3,
                 widths = c(1.055, 1, 1),
                 heights = c(1, 0.05, 1)) +
  theme(plot.margin = margin (1, 3, 1, 1, "mm"))


ggsave(filename = "~/Brazil-measles/figures/cases_v_lagged_coverage.png", 
       plot = fig, height = 5.5, width = 8, units = "in", bg='white', dpi = 800)

rm(list = ls())




################################################################################
##
## geometry for maps
##
################################################################################
load("~/Brazil-measles/data//generated_data/04.analysis_data_final.RData")


geom <- read_sf('~/Brazil-measles/data/geography/municipalities_2020/BR_Municipios_2020.shp') %>% 
  mutate(muni_code_6 = substring(CD_MUN, 1, 6)) %>% 
  filter(!str_detect(NM_MUN, "Área Operacional")) %>% # water area
  dplyr::select(muni_code_6, geometry)



cases_cumul <- df %>% 
  filter(year %in% 2018:2021) %>% 
  group_by(muni_code_6) %>% 
  summarise(cases = sum(mv_cases_total)) %>% 
  merge(df %>% filter(year == 2021) %>% dplyr::select(muni_code_6, pop_total), by = "muni_code_6") %>% 
  mutate(incid = cases / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, incid) %>% 
  right_join(geom, by = "muni_code_6")

st_write(cases_cumul, "~/Brazil-measles/data/geography/cumulative_cases_18_21/cumulative_cases_18_21.shp")


nm_mortality <- df %>% 
  dplyr::select(muni_code_6, year, nm_mx) %>% 
  filter(year %in% 2018:2024) %>% 
  pivot_wider(names_from = year,
              values_from = nm_mx,
              names_prefix = "mort") %>% 
  right_join(geom, by = "muni_code_6")

st_write(nm_mortality, "~/Brazil-measles/data/geography/nm_mortality/nm_mortality.shp")


coverage <- df %>% 
  dplyr::select(muni_code_6, year, mcv_d1_cov_tc) %>% 
  pivot_wider(names_from = year,
              values_from = mcv_d1_cov_tc,
              names_prefix = "cov") %>% 
  right_join(geom, by = "muni_code_6")

st_write(coverage, "~/Brazil-measles/data/geography/coverage/coverage.shp")



