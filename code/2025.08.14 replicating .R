
# Setting-up.
rm(list=ls())
setwd("/Users/simoncooper/Documents/measles")
getwd()
load("clean_brazil_data.RData")

library(tidyverse)
library(readr)
library(dplyr)
library(stargazer)
library(plm)
library(fixest)

# Making a UBS per 1,000 variable, as this is what Isabella uses.
df <- df %>%
  mutate(UBS_p1k = UBS_p100k/100)

# Putting in same panel data format that Isabella used.
panel <- pdata.frame(df, index = c("muni_code", "year"))
panel$year <- as.numeric(as.character(panel$year))
table(panel$year)

replication_panel <- panel %>%
  filter(year >= 2013 & year <= 2021,
         state_name == "São Paulo")
table(replication_panel$year)

### Replicating Isabella's work. 

## Poisson models.

# Model 1: Poisson with no covariates.
poisson <- glm(measles_cases ~ coverage_dose2, 
                     data = replication_panel, 
                     family = poisson(link = "log"),
                     offset = log(population))
summary(poisson)

poisson_lag2 <- glm(measles_cases ~ coverage_dose2_lag2,
                    data = replication_panel,
                    family = poisson(link = "log"),
                    offset = log(population))
summary(poisson_lag2)

poisson_lag3 <- glm(measles_cases ~ coverage_dose2_lag3,
                    data = replication_panel, 
                    family = poisson(link = "log"),
                    offset = log(population))
summary(poisson_lag3)

stargazer(poisson, poisson_lag2, poisson_lag3,
          type = "html",
          title = "Model 1: Poisson with no covariates",
          align = TRUE,
          dep.var.labels = "Reported Cases | Poisson Model",
          column.labels = c("Second Dose Coverage", "Coverage Lag 2", "Coverage Lag 3"),
          covariate.labels = c("Second Dose Coverage", "Coverage Lag 2", "Coverage Lag 3", "Intercept"),
          omit.stat = "ser",
          digits = 5,
          out = "poisson1.html")

# Model 2: Poisson with GDP per capita.
poisson_gdp_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + log(GDP_PC),
                    data = replication_panel, 
                    family = poisson(link = "log"),
                    offset = log(population))
summary(poisson_gdp_lag3)

poisson_gdp_lag2 <- glm(measles_cases ~ coverage_dose2_lag2 + log(GDP_PC),
                        data = replication_panel, 
                        family = poisson(link = "log"),
                        offset = log(population))
summary(poisson_gdp_lag2)

# Model 3: Poisson with population density.
poisson_pop_density_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + log(GDP_PC) + pop_density,
                        data = replication_panel, 
                        family = poisson(link = "log"),
                        offset = log(population))
summary(poisson_pop_density_lag3)


# Model 4: Poisson with UBS variable(s).
poisson_UBS_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + asinh(UBS_p1k) + log(GDP_PC),
                                data = replication_panel, 
                                family = poisson(link = "log"),
                                offset = log(population))
summary(poisson_UBS_lag3)

# Note: some UBS_p1k values were 0, causing some problems when I log transformed them. Instead, I used an inverse hyperbolic sine transformation.

poisson_UBS2_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + asinh(UBS_p1k) + log(GDP_PC) + pop_density,
                         data = replication_panel,
                         family = poisson(link = "log"),
                         offset = log(population))
summary(poisson_UBS2_lag3)

stargazer(poisson_gdp_lag3, poisson_pop_density_lag3, poisson_UBS_lag3,
          type = "html",
          title = "Models 2, 3, and 4: Poisson with GDP, pop density, and UBS",
          align = TRUE,
          dep.var.labels = "Reported Cases | Poisson Model",
          column.labels = c("GDP per capita", "Population density", "UBS density with some covariates"),
          omit.stat = c("ser", "f", "rsq", "adj.rsq"),
          digits = 5,
          out = "poisson2.html")

# Model 5: Poisson with doctor coverage variable.
poisson_doctors_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + pop_density + asinh(UBS_p1k) + log(GDP_PC) + doctors_p100k,
                            data = replication_panel, 
                            family = poisson(link = "log"),
                            offset = log(population))
summary(poisson_doctors_lag3)

# Model 6: Poisson with nurse coverage variable.
poisson_nurses_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + pop_density + asinh(UBS_p1k) + log(GDP_PC) + nurses_p100k,
                            data = replication_panel, 
                            family = poisson(link = "log"),
                            offset = log(population))
summary(poisson_nurses_lag3)

# Model 7: Poisson with nurse and UBS coverage interaction term.
poisson_interaction_lag3 <- glm(measles_cases ~ coverage_dose2_lag3 + pop_density + asinh(UBS_p1k) + log(GDP_PC) + nurses_p100k + nurses_p100k*asinh(UBS_p1k),
                                data = replication_panel, 
                                family = poisson(link = "log"),
                                offset = log(population))
summary(poisson_interaction_lag3)

stargazer(poisson_doctors_lag3, poisson_nurses_lag3, poisson_interaction_lag3,
          type = "html",
          title = "Models 5, 6, and 7: Poisson with doctors, nurses, and interaction",
          align = TRUE,
          dep.var.labels = "Reported Cases | Poisson Model",
          column.labels = c("Doctor density", "Nurse density", "Nurse density interacted with UBS density"),
          omit.stat = "ser",
          digits = 5,
          out = "poisson3.html")

## Fixed effects models.

# Model 8: FE Poisson. 
fe <- fepois(measles_cases ~ coverage_dose2_lag3 | muni_code,
             data = replication_panel,
             offset = ~log(population))
summary(fe)

# Model 9: FE Poisson, GDP.
fe_GDP <- fepois(measles_cases ~ coverage_dose2_lag3 + asinh(GDP_PC) | muni_code,
                 data = replication_panel,
                 offset = ~log(population))

# Model 10: FE Poisson, GDP + Population density.
fe_pop_density <- fepois(measles_cases ~ coverage_dose2_lag3 + log(GDP_PC) + pop_density | muni_code,
                         data = replication_panel,
                         offset = ~log(population))

# Model 11: FE Poisson, GDP + Population density + UBS density.
fe_ubs <- fepois(measles_cases ~ coverage_dose2_lag3 + log(GDP_PC) + pop_density + UBS_p1k | muni_code,
                 data = replication_panel,
                 offset = ~log(population))

# Model 12: FE Poisson, GDP + Population density + UBS density + nurse density.
fe_nurses <- fepois(measles_cases ~ coverage_dose2_lag3 + log(GDP_PC) + pop_density + UBS_p1k + nurses_p100k | muni_code,
                    data = replication_panel,
                    offset = ~log(population))

# Model 13: FE Poisson, GDP + Population density + UBS density + nurse density + doctor density.
fe_doctors <- fepois(measles_cases ~ coverage_dose2_lag3 + log(GDP_PC) + pop_density + UBS_p1k + nurses_p100k + doctors_p100k | muni_code,
                     data = replication_panel,
                     offset = ~log(population))

# Extracting results.
irr_table <- function(model, coverage_coef_name = "Coverage_lag3", 
                      coverage_multiplier = 0.1, confidence_level = 0.95) {
  
  # Calculate z-score for confidence interval
  alpha <- 1 - confidence_level
  z <- qnorm(1 - alpha/2)
  
  # Extract coefficient summary from fixest object
  coefs <- summary(model)$coeftable
  
  # Calculate confidence intervals
  lower <- coefs[, "Estimate"] - z * coefs[, "Std. Error"]
  upper <- coefs[, "Estimate"] + z * coefs[, "Std. Error"]
  
  # Create IRR table for all coefficients
  IRR_table <- cbind(
    IRR = exp(coefs[, "Estimate"]),
    Lower_CI = exp(lower),
    Upper_CI = exp(upper)
  )
  
  # Special handling for coverage coefficient if it exists
  coverage_result <- NULL
  if (coverage_coef_name %in% rownames(coefs)) {
    # Get original coefficient and SE
    beta_cov <- coefs[coverage_coef_name, "Estimate"]
    se_cov <- coefs[coverage_coef_name, "Std. Error"]
    
    # Scale by multiplier (e.g., 0.1 for 10% change)
    beta_scaled <- beta_cov * coverage_multiplier
    se_scaled <- se_cov * coverage_multiplier
    
    # Calculate scaled confidence interval
    lower_scaled <- beta_scaled - z * se_scaled
    upper_scaled <- beta_scaled + z * se_scaled
    
    # Convert to IRR
    coverage_result <- c(
      IRR = exp(beta_scaled),
      Lower_CI = exp(lower_scaled),
      Upper_CI = exp(upper_scaled)
    )
    
    # Create name for the scaled coefficient
    multiplier_pct <- coverage_multiplier * 100
    names(coverage_result) <- c(
      paste0("IRR_", multiplier_pct, "pct"),
      paste0("Lower_", confidence_level*100, "_CI"),
      paste0("Upper_", confidence_level*100, "_CI")
    )
  }
  
  # Return results
  return(list(
    IRR_table = round(IRR_table, 3),
    coverage_scaled = if(!is.null(coverage_result)) round(coverage_result, 3) else NULL
  ))
}


fe_result <- irr_table(fe)
fe_GDP_result <- irr_table(fe_GDP) 
fe_pop_density_result <- irr_table(fe_pop_density)
fe_ubs_result <- irr_table(fe_ubs)
fe_nurses_result <- irr_table(fe_nurses)
fe_doctors_result <- irr_table(fe_doctors)
fe_result
fe_GDP_result
fe_pop_density_result
fe_ubs_result 
fe_nurses_result
fe_doctors_result
