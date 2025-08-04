library(tidyr)
library(dplyr)

load("clean_brazil_data.RData")

# Removing MM1_coverage_lag2-6; MMR2_coverage_lag2-6; tetra_coverage_lag2-6; coverage, coverage_lag2-6; coverage2.
remove <- c("MMR1_coverage_lag2", "MMR1_coverage_lag3", "MMR1_coverage_lag4", "MMR1_coverage_lag5", "MMR1_coverage_lag6", "MMR2_coverage_lag2", 
            "MMR2_coverage_lag3", "MMR2_coverage_lag4", "MMR2_coverage_lag5", "MMR2_coverage_lag6", "tetra_coverage_lag2", "tetra_coverage_lag3", 
            "tetra_coverage_lag4", "tetra_coverage_lag5", "tetra_coverage_lag6", "coverage_lag2", "coverage_lag3", "coverage_lag4", "coverage_lag5", 
            "coverage_lag6", "coverage", "coverage2")

df <- df %>% select(-all_of(remove))

# Replacing with coverage_dose1 and coverage_dose2.
df <- df %>%
  mutate(coverage_dose1 = ifelse(is.na(monovalent_coverage) & is.na(MMR1_coverage), NA_real_, rowSums(cbind(monovalent_coverage, MMR1_coverage), na.rm = TRUE)),
         coverage_dose2 = ifelse(is.na(MMR2_coverage) & is.na(tetra_coverage), NA_real_, rowSums(cbind(MMR2_coverage, tetra_coverage), na.rm = TRUE)))

# Adding new lagged variables.
df <- df %>% mutate(coverage_dose1_lag1 = dplyr::lag(coverage_dose1, 1, order_by = year),
                    coverage_dose1_lag2 = dplyr::lag(coverage_dose1, 2, order_by = year),
                    coverage_dose1_lag3 = dplyr::lag(coverage_dose1, 3, order_by = year),
                    coverage_dose1_lag4 = dplyr::lag(coverage_dose1, 4, order_by = year),
                    coverage_dose1_lag5 = dplyr::lag(coverage_dose1, 5, order_by = year),
                    coverage_dose1_lag6 = dplyr::lag(coverage_dose1, 6, order_by = year),
                    coverage_dose1_lag7 = dplyr::lag(coverage_dose1, 7, order_by = year),
                    coverage_dose1_lag8 = dplyr::lag(coverage_dose1, 8, order_by = year),
                    coverage_dose1_lag9 = dplyr::lag(coverage_dose1, 9, order_by = year),
                    
                    coverage_dose2_lag1 = dplyr::lag(coverage_dose2, 1, order_by = year),
                    coverage_dose2_lag2 = dplyr::lag(coverage_dose2, 2, order_by = year),
                    coverage_dose2_lag3 = dplyr::lag(coverage_dose2, 3, order_by = year),
                    coverage_dose2_lag4 = dplyr::lag(coverage_dose2, 4, order_by = year),
                    coverage_dose2_lag5 = dplyr::lag(coverage_dose2, 5, order_by = year),
                    coverage_dose2_lag6 = dplyr::lag(coverage_dose2, 6, order_by = year),
                    coverage_dose2_lag7 = dplyr::lag(coverage_dose2, 7, order_by = year),
                    coverage_dose2_lag8 = dplyr::lag(coverage_dose2, 8, order_by = year),
                    coverage_dose2_lag9 = dplyr::lag(coverage_dose2, 9, order_by = year))
names(df)

# Saving results.
save(df, file = "clean_brazil_data.RData")
View(df)
