load('~/Brazil-measles/data/muni-year_panel_01-24.RData')

# find missing values
# colnames(df)[colSums(is.na(df)) > 0]
# [1] "amnesia_prev_d1"     "amnesia_prev_d2"     "amnesia_prev_d3"     "mcv_d1_cov_lag2"     "mcv_d1_cov_tc_lag2" 
# [6] "gdp_pc"              "clinics_pc"          "pct_urban"           "poverty_rate"        "literacy_rate"      
# [11] "susceptible_pop_u10"

# unique(df$muni_code_6[is.na(df$poverty_rate)])
# 150475 220095 220672 220779 240615 270375 290327 291955 320225 330285 421265 
# 422000 430003 430047 430107 430222 430223 430258 430461 430462 430465 430583 
# 430593 430613 430843 431065 431087 431123 431217 431346 431413 431417 431446 
# 431454 431531 431595 431673 431697 431861 431936 432146 432377 500390 500627 
# 510185 510325 510336 510343 510452 510454 510617 510619 510631 510757 510774
# 510776 510779 510788 510835 520485 520815 521015 521225


# municipalities that have existed since at least 2000
census00_munis <- read_xlsx('~/Brazil-measles/data/literacy/ibge_literacy_rate_2000.xlsx', 
                   skip = 5, n_max = 5507) %>% 
  clean_names() %>% 
  mutate(muni_code_6 = substr(x1, 1, 6)) %>% 
  dplyr::select(muni = x2, muni_code_6)

# only include those munis
# only include years with all data
df_00census_06_23 <- df %>% 
  filter(muni_code_6 %in% census00_munis$muni_code_6 & as.numeric(as.character(year)) %in% 2006:2023)

# colnames(df_00census_06_23)[colSums(is.na(df_00census_06_23)) > 0]
# susceptible_pop_u10
# only missing for muni 221038 2006-2009 (missing vaccination data from 1990s)

# we restrict analyses to municipalities that have existed since at least 2000.
# For susceptibile pops, we further exclude one municipality that is missing
# vaccination data for 1994-2000 (unable to calculate # unvaccinated children).

df <- df_00census_06_23

anyNA(df %>% dplyr::select(-susceptible_pop_u10))

save(df, file = '~/Brazil-measles/data/analysis_data.RData')

# keep all years for descriptive plots
plot_df <- df %>% 
  filter(muni_code_6 %in% census00_munis$muni_code_6)

save(plot_df, file = '~/Brazil-measles/data/plotting_data.RData')
