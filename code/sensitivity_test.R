library(tidyverse)
library(ggplot2)
library(janitor)
library(tsibble)
library(ggeffects)
library(feasts)


cases <- bind_rows(read.delim("/Users/kyhoff/Downloads/sinanwin_cnv_exantbr173817136_25_170_168.csv", 
           sep = ";", fileEncoding = "Latin1", skip = 5, nrow = 7) %>%
  clean_names() %>%
  filter(regiao_de_residencia == "Total") %>% # no ignored region cases
  select(-total) %>% 
  mutate(x2004 = "0") %>%
  pivot_longer(cols = -regiao_de_residencia, values_to = "measles_cases", names_to = "year") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         measles_cases = as.numeric(measles_cases)) %>% 
  select(-regiao_de_residencia),
  
  read.delim("/Users/kyhoff/Downloads/sinannet_cnv_exantbr174245136_25_170_168.csv", 
                          sep = ";", fileEncoding = "Latin1", skip = 5, nrow = 7) %>%
  clean_names() %>%
  filter(!str_detect(regiao_de_residencia, "Igno|Tot")) %>% 
  select(-c(total, em_branco_ign, x2001, x2006)) %>% 
  mutate(across(starts_with("x"), as.numeric)) %>%
  pivot_longer(cols = -regiao_de_residencia, values_to = "measles_cases", names_to = "year") %>% 
  mutate(year = as.numeric(substr(year, 2, 5))) %>% 
  group_by(year ) %>% 
  reframe(year = unique(year),
          measles_cases = sum(measles_cases, na.rm = T)))


# Deaths by Residence by Year of Death according to Region,
# ICD-10 Category: C25 Malignant neoplasm of the pancreas,
# Period: 2000-2024
panc <- read.delim("/Users/kyhoff/Downloads/sim_cnv_obt10uf170542136_25_170_168.csv", 
                 sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 7) %>%
  clean_names() %>%
  filter(regiao == "Total") %>% # no region ignored cases
  select(-total) %>% 
  pivot_longer(cols = -regiao, values_to = "pancreatic_deaths", names_to = "year") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         pancreatic_deaths = as.numeric(pancreatic_deaths)) %>% 
  select(-regiao)

# Deaths by Residence by Year of Death according to Region.
# ICD-10 Group: Malignant neoplasms of the lip, oral cavity and pharynx; Malignant neoplasms of the digestive organs; Malignant neoplasms of bones and articular cartilage; Melanoma and other malignant neoplasms of the skin; Malignant neoplasms of the breast; Malignant neoplasms of the female genital organs; Malignant neoplasms of the male genital organs; Malignant neoplasms of the urinary tract; Malignant neoplasms of the thyroid and other endocrine glands.
# Period: 2000-2024
cancer <- read.delim("/Users/kyhoff/Downloads/sim_cnv_obt10uf185631136_25_170_168.csv", 
                   sep = ";", fileEncoding = "Latin1", skip = 4, nrow = 7) %>%
  clean_names() %>%
  filter(regiao == "Total") %>% # no region ignored cases
  select(-total) %>% 
  pivot_longer(cols = -regiao, values_to = "cancer_deaths", names_to = "year") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         cancer_deaths = as.numeric(cancer_deaths)) %>% 
  select(-regiao)


# Resident Population - Study of Population Estimates by Municipality, Age and Sex 2000-2025 - Brazil
# Resident population by year according to region
# Period: 2001-2024
pop <- read.delim("/Users/kyhoff/Downloads/ibge_cnv_popsvs2024br182510136_25_170_168.csv", 
                   sep = ";", fileEncoding = "Latin1", skip = 3, nrow = 7) %>%
  clean_names() %>%
  filter(regiao == "Total") %>% # no region ignored cases
  pivot_longer(cols = -regiao, values_to = "pop", names_to = "year") %>% 
  mutate(year = as.numeric(substr(year, 2, 5)),
         pop = as.numeric(pop)) %>% 
  select(-regiao)



data <- merge(cases, panc, by = "year") %>%
  merge(pop, by = "year") %>%
  merge(cancer, by = "year") %>% 
  mutate(pancreatic_deaths_p100k = pancreatic_deaths / pop * 100000,
         cancer_deaths_p100k = cancer_deaths / pop * 100000,
         measles_incidence_p100k = measles_cases / pop * 100000) %>% 
  as_tsibble(index = year)


data %>%  
  CCF(y = cancer_deaths_p100k, x = measles_incidence_p100k, type = "correlation", lag_max = 5) %>% 
  mutate(lag = as.numeric(str_sub(lag, end = -2))) %>% 
  ggplot(aes(x = lag, y = ccf)) +
  geom_hline(aes(yintercept = 0.409), linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(fill = as.factor(lag)), size = 2.25, shape = 21, stroke = 0.2) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0.01, 0.5)) +
  labs(y = "Correlation", x = "Years lagged") + #, title = "Correlation b/w total annual mean ID mx and annual mean MV incid") +
  theme_bw(base_size=12, base_family = "Myriad Pro") + 
  theme(plot.title = element_text(size = 7),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11, color = "black"),
        axis.ticks = element_line(linewidth=0.3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin=margin(0.2,0.2,0,0.5, unit = "mm"))

data %>% 
  ggplot(aes(x=dplyr::lag(measles_incidence_p100k, 1), y=cancer_deaths_p100k)) +
  geom_point(size = 2) + 
  scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10), labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) + 
#  coord_cartesian(xlim = c(0.00073, 7.1)) +
 # labs(x= "Measles incidence per 100,000", y = NULL) +
  geom_smooth(method = "lm", fill = "#94D4D2", color = "#8ebfbe", linewidth = 0.8)



data %>% ggplot(aes(x=measles_incidence_p100k,y=pancreatic_deaths_p100k,color=year)) + geom_point() +
  scale_x_log10() + scale_y_log10() + 
  geom_smooth(method = "lm")

data %>% ggplot(aes(x=measles_incidence_p100k,y=cancer_deaths_p100k,color=year)) + geom_point() +
  scale_x_log10() + scale_y_log10() + 
  geom_smooth(method = "lm")


summary(lm(pancreatic_deaths_p100k ~ measles_incidence_p100k, data=data))$r.squared

summary(lm(cancer_deaths_p100k ~ log(measles_incidence_p100k), data=data %>% filter(year != 2004)))$r.squared





