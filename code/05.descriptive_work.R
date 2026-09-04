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


load("~/Brazil-measles/data/analysis_data.RData")

cor_df <- df %>% dplyr::select(`MCV1 coverage` = mcv_d1_cov_tc, `Measles incidence` = mv_incid_total, 
                               `Non-measles mort. rate` = nm_mx,
                               `GDP per capita` = gdp_pc, `Clinics per capita` = clinics_pc,
                               `Poverty rate` = poverty_rate, 
                               `Literacy rate` = literacy_rate, CBR = cbr, `%Urban` = pct_urban)

cor_matrix <- cor(cor_df)

png(filename = "~/Brazil-measles/figures/correlation_matrix_plot.png", width = 2000, height = 2000, res = 300)

corrplot(cor_matrix, method = "color", tl.col = "black")

dev.off()

################################################################################
##
## state-year heat maps (ordered by pop density)
##
################################################################################
load('~/Brazil-measles/data/plotting_data.RData')

theme <- theme_minimal(base_size = 12, base_family = "Myriad Pro") +
  theme(text = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = -3, l=0, unit = "mm")),
        axis.text.x = element_text(size = 12, angle = 60, vjust = 1.4, hjust = 1.3, color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, linewidth=0.6),
        panel.background = element_rect(color = "white"),
        panel.grid.major = element_blank(),
        legend.title = element_text(size = 12, margin = margin(b=2, unit = "mm")),
        legend.text = element_text(size = 12, vjust = 0, margin = margin(t = 0, b = 0, unit = "mm")),
        legend.position = "top", 
        legend.title.position = "top",
        legend.margin = margin(b = -3, unit = "mm"),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.4),
        plot.margin = margin(t=0.5, r=0.5, b=0.5, l=0.5, unit = "mm"))


p_cov <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          mean_cov = mean(mcv_d1_cov_tc)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = mean_cov)) +
  scale_fill_gradient(low = "#fcfafc",high = "#510d54", 
                      limits = c(50, 100),
                      breaks = c(60, 70, 80, 90, 100),
                      labels = c("60%", "70%", "80%", "90%", "100%"),
                      na.value = "transparent", name = "Mean MCV1 coverage",
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = NA,
                                             nbin = 500,
                                             barwidth = 12,
                                             barheight = 0.8, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme + 
  theme(axis.text.y = element_text(size = 12, color = "black", hjust = 1,
                                   margin = margin(l = 5, unit = "mm")))


p_incid <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          incid_tot = mean(mv_incid_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = incid_tot)) +
  scale_fill_gradient(low = "#f5f9ff", high = "#01155c", 
                      trans = pseudo_log_trans(sigma = 1e-7, base = 10),
                      breaks = c(0, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 0.5),
                      labels = scales::label_parse()(c("0", "10^-6", "10^-5", "10^-4", "10^-3", "10^-2", "10^-1", "0.5")),
                      na.value = "white", name = "Mean MV incidence per 1,000", # log scale
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = NA,
                                             nbin = 500,
                                             barwidth = 12,
                                             barheight = 0.8, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.position = "top", 
                 legend.title.position = "top")


p_mort <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          nm_mx_tot = mean(nm_mx)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = nm_mx_tot)) +
  scale_fill_gradient2(low = "#f6f0e6", mid = "#f28b88", high = "#49006b", 
                       midpoint = 0.5,
                       limits = c(0, 1.0501),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", 0.25, 0.5, 0.75, " 1"),
                       na.value = "transparent", name = "Mean NMID Deaths per 1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 12,
                                              barheight = 0.8, 
                                              frame.linewidth = 0.3,  
                                              direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme + theme(legend.position = "top",
                 legend.title.position = "top",
                axis.text.y = element_text(size = 12, color = "black", hjust = 1, margin = margin(l = 5, unit = "mm")))


p_susc <- df %>% 
  group_by(state, year) %>% 
  reframe(susc_prop = sum(susceptible_pop_u10) / sum(pop_total) * 1000) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = susc_prop)) +
  scale_fill_gradient2(low = "#f7fcf9", mid = "#a1d6cb", high = "#01346e", 
                       midpoint = 50,
                       na.value = "transparent", name = "Susceptile individuals per 1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 12,
                                              barheight = 0.8, 
                                              frame.linewidth = 0.3,  
                                              direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.position = "top",
                legend.title.position = "top")


fig <- p_cov + p_incid + p_mort + p_susc +
  plot_layout(widths = c(1, 1, 1, 1), nrow = 2, ncol = 2) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 12, hjust = -1, vjust = 2, family = "Myriad Pro Bold"))


ggsave(filename = "~/Brazil-measles/figures/state_heatmaps_density.png", 
       plot = fig, height = 10, width = 9, bg='#ffffff')


################################################################################
##
## state-year heat maps (ordered by latitude)
##
################################################################################
load('~/Brazil-measles/data/plotting_data.RData')

lats <- read.delim('/Users/kyhoff/Brazil-measles/data/state_latitudes.txt', sep = "\t") %>% 
  arrange(latitude)

lat_order <- lats$state

theme <- theme_minimal(base_size = 12, base_family = "Myriad Pro") +
  theme(text = element_text(color = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, margin = margin(r = -3, l=0, unit = "mm")),
        axis.text.x = element_text(size = 12, angle = 60, vjust = 1.4, hjust = 1.3, color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_rect(color = "black", fill=NA, linewidth=0.6),
        panel.background = element_rect(color = "white"),
        panel.grid.major = element_blank(),
        legend.title = element_text(size = 12, margin = margin(b=2, unit = "mm")),
        legend.text = element_text(size = 12, vjust = 0, margin = margin(t = 0, b = 0, unit = "mm")),
        legend.position = "top", 
        legend.title.position = "top",
        legend.margin = margin(b = -3, unit = "mm"),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.4),
        plot.margin = margin(t=0.5, r=0.5, b=0.5, l=0.5, unit = "mm"))


p_cov <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          mean_cov = mean(mcv_d1_cov_tc)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = mean_cov)) +
  scale_fill_gradient(low = "#fcfafc",high = "#510d54", 
                      limits = c(50, 100),
                      breaks = c(60, 70, 80, 90, 100),
                      labels = c("60%", "70%", "80%", "90%", "100%"),
                      na.value = "transparent", name = "Mean MCV1 coverage",
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = NA,
                                             nbin = 500,
                                             barwidth = 12,
                                             barheight = 0.8, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme + 
  theme(axis.text.y = element_text(size = 12, color = "black", hjust = 1,
                                   margin = margin(l = 5, unit = "mm")))


p_incid <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          incid_tot = mean(mv_incid_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = incid_tot)) +
  scale_fill_gradient(low = "#f5f9ff", high = "#01155c", 
                      trans = pseudo_log_trans(sigma = 1e-7, base = 10),
                      breaks = c(0, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 0.5),
                      labels = scales::label_parse()(c("0", "10^-6", "10^-5", "10^-4", "10^-3", "10^-2", "10^-1", "0.5")),
                      na.value = "white", name = "Mean MV incidence per 1,000", # log scale
                      guide = guide_colorbar(frame.colour = "black",
                                             ticks.colour = NA,
                                             nbin = 500,
                                             barwidth = 12,
                                             barheight = 0.8, 
                                             frame.linewidth = 0.3,  
                                             direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.position = "top", 
                legend.title.position = "top")


p_mort <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          nm_mx_tot = mean(nm_mx)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = nm_mx_tot)) +
  scale_fill_gradient2(low = "#f6f0e6", mid = "#f28b88", high = "#49006b", 
                       midpoint = 0.5,
                       limits = c(0, 1.0501),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c("0", 0.25, 0.5, 0.75, " 1"),
                       na.value = "transparent", name = "Mean NMID Deaths per 1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 12,
                                              barheight = 0.8, 
                                              frame.linewidth = 0.3,  
                                              direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme + theme(legend.position = "top",
                legend.title.position = "top",
                axis.text.y = element_text(size = 12, color = "black", hjust = 1, margin = margin(l = 5, unit = "mm")))


p_susc <- df %>% 
  group_by(state, year) %>% 
  reframe(susc_prop = sum(susceptible_pop_u10) / sum(pop_total) * 1000) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = factor(state, levels = lat_order))) +
  geom_raster(aes(fill = susc_prop)) +
  scale_fill_gradient2(low = "#f7fcf9", mid = "#a1d6cb", high = "#01346e", 
                       midpoint = 50,
                       na.value = "transparent", name = "Susceptile individuals per 1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 12,
                                              barheight = 0.8, 
                                              frame.linewidth = 0.3,  
                                              direction = 'horizontal')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = NULL) +
  theme + theme(legend.position = "top",
                legend.title.position = "top")


fig <- p_cov + p_incid + p_mort + p_susc +
  plot_layout(widths = c(1, 1, 1, 1), nrow = 2, ncol = 2)


ggsave(filename = "~/Brazil-measles/figures/state_heatmaps_latitude.png", 
       plot = fig, height = 10, width = 9, bg='#ffffff')



################################################################################
##
## age-specific incidence
##
################################################################################
pop <- read.delim("~/Brazil-measles/data/pop_data/age_structured_pop_ibge_cnv.csv",
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


cases_ages1 <- read.delim("~/brazil_measles/data/measles_cases/age_structured_cases_07-25_sinannet_cnv.csv", 
                         sep = ";", fileEncoding = "Latin1", skip = 5, nrow = 14) %>%
  clean_names() %>% 
  dplyr::select(-c(em_branco_ign, x_1975, x2001, x2006, total)) %>%
  filter(!str_detect(faixa_etaria, "Em")) %>% 
  mutate(across(contains("x2"), ~as.numeric(ifelse(.x == "-", 0, .x))))


cases_ages2 <- read.delim("~/brazil_measles/data/measles_cases/age_structured_cases_02-06_sinanwin_cnv.csv", 
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

# case counts only
fig <- incid_ages %>% 
  ggplot(aes(x = year, y = age)) +
  geom_tile(aes(fill = cases)) +
  scale_fill_gradientn( 
    transform = "pseudo_log",
    breaks = c(0, 1, 5, 10, 50, 100, 1000, 5000),  
    colors = c("#fcfcfa", "#DEF5E5FF", "#ADE3C0FF", "#6CD3ADFF", "#43BBADFF",
               "#35A1ABFF", "#3487A6FF","#366DA0FF", "#3D5296FF", "#403A75FF"),
    guide = guide_coloursteps(frame.colour = "black",                            
                              even.steps = T,                                 
                              show.limits = T,
                              barwidth = 1,
                              barheight = 12, 
                              frame.linewidth = 0.3,  
                              direction = 'vertical')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.6, 2024.4), ylim = c(1.1, 9.9)) +
  labs(x= "Year", y = "Age group")

ggsave(filename = "~/brazil_measles/figures/age_structured_cases.png", 
       plot = fig, height = 4, width = 7, units = "in", bg='white')



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
                                                 frame.linewidth = 0.3,
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


ggsave(filename = "~/brazil_measles/figures/age_structured_incidence.png", 
       plot = fig, height = 4, width = 7, units = "in", bg='white')


rm(list = ls())



################################################################################
##
## geometry for maps
##
################################################################################
load("~/Brazil-measles/data/analysis_data.RData")


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

susc <- df %>% 
  mutate(susceptible_prop = susceptible_pop_u10 / pop_total * 1000) %>% 
  dplyr::select(muni_code_6, year, susceptible_prop) %>% 
  pivot_wider(names_from = year,
              values_from = susceptible_prop,
              names_prefix = "susc") %>% 
  right_join(geom, by = "muni_code_6")

st_write(susc, "~/Brazil-measles/data/geography/susceptible_pop/susceptible_pop.shp")




