library(tidyverse)
library(janitor)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(viridis)
library(scales)


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
## state-year plots
##
################################################################################
load('~/brazil_measles/generated_data/muni-year_panel_02-24.RData')




theme1 <- theme_minimal(base_size = 14, base_family = "Myriad Pro") +
  theme(text = element_text(color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14, margin = margin(r = -3, l=0, unit = "mm")),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        panel.border = element_rect(color = "black", fill=NA, linewidth=0.6),
        panel.background = element_rect(color = "white"),
        panel.grid.major = element_blank(), 
        legend.title = element_text(size = 14, margin = margin(b=4, l=-1, unit = "mm")),
        legend.text = element_text(size = 14),
        legend.margin = margin(l=-1, r=1, unit = "mm"),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.4),
        plot.margin = margin(t=1, r=1, b=1, l=1, unit = "mm"))



fig_mort <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          nm_mx_tot = mean(nm_mx), 
          incid_tot = mean(mv_incid_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = nm_mx_tot)) +
  scale_fill_gradient2(low = "#f6f0e6", mid = "#f28b88", high = "#49006b", 
                       midpoint = 0.5,
                       na.value = "transparent", name = "Mean NMID\nDeaths per \n1,000",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 1.1,
                                              barheight = 18, 
                                              frame.linewidth = 0.3,  
                                              direction = 'vertical')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme1 +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.4, hjust = 1.3, color = "black"))



ggsave(filename = "~/brazil_measles/figures/state_mort.png", plot = fig_mort, height = 5.75, width = 7, bg='#ffffff')



fig_vax <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          mean_cov = mean(mcv_d1_cov_tc)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = mean_cov)) +
  scale_fill_gradient(low = "#fcfafc",high = "#510d54", 
                   #    midpoint = 85,
                       na.value = "transparent", name = "Mean MCV1\ncoverage",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 1.1,
                                              barheight = 18, 
                                              frame.linewidth = 0.3,  
                                              direction = 'vertical')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme1 +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.4, hjust = 1.3, color = "black"))


fig_incid <- df %>% 
  group_by(state, year) %>% 
  reframe(pop = sum(pop_total),
          year = unique(year),
          incid_tot = mean(mv_incid_total)) %>% 
  ggplot(aes(x = as.numeric(as.character(year)), y = state)) +
  geom_raster(aes(fill = incid_tot)) +
  scale_fill_gradient(low = "#f5f9ff", high = "#01155c", 
                      trans = pseudo_log_trans(sigma = 1e-7, base = 10),
                      breaks = c(0, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.5),
                      labels = c("0.0", "10⁻⁶", "10⁻⁵", "10⁻⁴", "10⁻³", "0.01", "0.1", "0.5"),
                      na.value = "white", name = "Mean measles\nincidence\nper 1,000\n(log scale)",
                      guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = NA,
                                              nbin = 500,
                                              barwidth = 1.1,
                                              barheight = 18, 
                                              frame.linewidth = 0.3,  
                                              direction = 'vertical')) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  coord_cartesian(xlim = c(2002.5, 2023.5), ylim = c(1.1, 26.9)) +
  labs(x= "Year", y = "State") +
  theme1 +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.4, hjust = 1.3, color = "black"),
        plot.margin = margin(t=1, r=0, b=1, l=1, unit = "mm"))
  

fig <-  ggarrange(fig_vax, fig_incid)

ggsave(filename = "~/brazil_measles/figures/state_vax_incid2.png", plot = fig, height = 6, width = 14, bg='#ffffff')


###########################################################################################


reg_data <- df %>% filter(!muni_code_6 %in% c(150475, 421265, 422000, 431454, 
                                              500627, 220095, 220672,500390, 
                                              510452, 510454) 
                          & year > 2003)
# munis missing gdp_pc for 2002-2004: 220095, 500390, 510452, 510454
# muni missing gdp_pc for 2002-2008: 220672

