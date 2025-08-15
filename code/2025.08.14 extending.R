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

# Putting in same panel data format that Isabella used.
panel <- pdata.frame(df, index = c("muni_code", "year"))
panel$year <- as.numeric(as.character(panel$year))
table(panel$year)

### Extending Isabella's work. 

## Poisson models.

# Model 1: Poisson with no covariates.
# Identifying whether no lag, 1-yr, 2-yr, 3-yr, 4-yr, or 5-yr lag fits best here.
# Looks like a 2-year lag works best.

poisson <- glm(measles_cases ~ coverage_dose2, 
               data = panel, 
               family = poisson(link = "log"),
               offset = log(population))
summary(poisson)

poisson_lag1 <- glm(measles_cases ~ coverage_dose2_lag1,
                    data = panel,
                    family = poisson(link = "log"),
                    offset = log(population))

poisson_lag2 <- glm(measles_cases ~ coverage_dose2_lag2,
                    data = panel,
                    family = poisson(link = "log"),
                    offset = log(population))

# Doesn't converge.
# poisson_lag3 <- glm(measles_cases ~ coverage_dose2_lag3,
#                    data = panel, 
#                    family = poisson(link = "log"),
#                    offset = log(population))

# Doesn't converge.
# poisson_lag4 <- glm(measles_cases ~ coverage_dose2_lag4,
#                    data = panel, 
#                    family = poisson(link = "log"),
#                    offset = log(population))

# poisson_lag5 <- glm(measles_cases ~ coverage_dose2_lag5,
#                    data = panel, 
#                    family = poisson(link = "log"),
#                    offset = log(population))

stargazer(poisson, poisson_lag1, poisson_lag2,
          type = "html",
          title = "Model 1: Poisson with no covariates",
          align = TRUE,
          dep.var.labels = "Reported Cases | Poisson Model",
          column.labels = c("Second Dose Coverage", "Coverage Lag 1", "Coverage Lag 2"),
          covariate.labels = c("Second Dose Coverage", "Coverage Lag 1", "Coverage Lag 2", "Intercept"),
          omit.stat = "ser",
          digits = 5,
          out = "poisson_nocovars.html")

# Model 2: Poisson with GDP per capita, population density, UBS coverage.
poisson_covars_lag2 <- glm(measles_cases ~ coverage_dose2_lag2 + asinh(UBS_p100k) + log(GDP_PC) + pop_density,
                         data = panel,
                         family = poisson(link = "log"),
                         offset = log(population))
summary(poisson_covars_lag2)

# Model 3: Poisson with doctor coverage variable.
poisson_doctors_lag2 <- glm(measles_cases ~ coverage_dose2_lag2 + asinh(UBS_p100k) + log(GDP_PC) + pop_density + doctors_p100k,
                            data = panel, 
                            family = poisson(link = "log"),
                            offset = log(population))
summary(poisson_doctors_lag2)

# Model 4: Poisson with nurse coverage variable.
poisson_nurses_lag2 <- glm(measles_cases ~ coverage_dose2_lag2 + asinh(UBS_p100k) + log(GDP_PC) + pop_density + nurses_p100k,
                           data = panel, 
                           family = poisson(link = "log"),
                           offset = log(population))
summary(poisson_nurses_lag2)

# Model 7: Poisson with nurse and UBS coverage interaction term.
poisson_interaction_lag2 <- glm(measles_cases ~ coverage_dose2_lag2 + asinh(UBS_p100k) + log(GDP_PC) + nurses_p100k + pop_density + nurses_p100k*asinh(UBS_p100k),
                                data = panel, 
                                family = poisson(link = "log"),
                                offset = log(population))
summary(poisson_interaction_lag2)

# Models are valid.
sapply(list(poisson_covars_lag2, poisson_doctors_lag2, poisson_nurses_lag2, poisson_interaction_lag2), 
       function(x) !is.null(x) && class(x)[1] == "glm")

# Models converge.
sapply(list(poisson_covars_lag2, poisson_doctors_lag2, poisson_nurses_lag2, poisson_interaction_lag2), 
       function(x) x$converged)

# No NA coefficients.
lapply(list(poisson_covars_lag2, poisson_doctors_lag2, poisson_nurses_lag2, poisson_interaction_lag2), 
       function(x) any(is.na(coef(x))))

# Note: excluding model with interaction term. 
stargazer(poisson_covars_lag2, poisson_doctors_lag2, poisson_nurses_lag2,
          type = "html",
          title = "Models 5, 6, and 7: Poisson with doctors, nurses, and interaction",
          align = TRUE,
          dep.var.labels = "Reported Cases | Poisson Model",
          column.labels = c("Covariates", "Doctor density", "Nurse density"),
          omit.stat = "ser",
          digits = 5,
          order = c("coverage_dose2_lag2", "asinh(UBS_p100k)", "log(GDP_PC)", "pop_density", 
                    "doctors_p100k", "nurses_p100k"),
          out = "poisson_covars.html")

## Poisson models with fixed effects.

# Model 8: FE Poisson. 
fe <- fepois(measles_cases ~ coverage_dose2_lag2 | muni_code,
             data = panel,
             offset = ~log(population))
summary(fe)

# Model 9: FE Poisson, GDP.
fe_GDP <- fepois(measles_cases ~ coverage_dose2_lag2 + asinh(GDP_PC) | muni_code,
                 data = panel,
                 offset = ~log(population))
summary(fe_GDP)

# Model 10: FE Poisson, GDP + Population density.
fe_pop_density <- fepois(measles_cases ~ coverage_dose2_lag2 + log(GDP_PC) + pop_density | muni_code,
                         data = panel,
                         offset = ~log(population))
summary(fe_pop_density)

# Model 11: FE Poisson, GDP + Population density + UBS density.
fe_ubs <- fepois(measles_cases ~ coverage_dose2_lag2 + log(GDP_PC) + pop_density + UBS_p100k | muni_code,
                 data = panel,
                 offset = ~log(population))

# Model 12: FE Poisson, GDP + Population density + UBS density + nurse density.
fe_nurses <- fepois(measles_cases ~ coverage_dose2_lag2 + log(GDP_PC) + pop_density + UBS_p100k + nurses_p100k | muni_code,
                    data = panel,
                    offset = ~log(population))

# Model 13: FE Poisson, GDP + Population density + UBS density + nurse density + doctor density.
fe_doctors <- fepois(measles_cases ~ coverage_dose2_lag2 + log(GDP_PC) + pop_density + UBS_p100k + nurses_p100k + doctors_p100k | muni_code,
                     data = panel,
                     offset = ~log(population))

# Displaying results.
etable(fe, fe_GDP, fe_pop_density, fe_ubs, fe_nurses, fe_doctors,
       title = "Models 8-13: Poisson fixed effects",
       headers = c("No covariates", "GDP", "Pop Density", "UBS", "Nurses", "Doctors"),
       dict = c("coverage_dose2_lag2" = "Coverage Dose 2 (lag 2)",
                "asinh(GDP_PC)" = "asinh(GDP PC)",
                "log(GDP_PC)" = "log(GDP PC)",
                "pop_density" = "Population Density",
                "UBS_p100k" = "UBS per 100k",
                "nurses_p100k" = "Nurses per 100k",
                "doctors_p100k" = "Doctors per 100k"),
       tex = FALSE,
       file = "poisson_fe.tex")

# Extracting results.
irr_table <- function(model, coverage_coef_name = "Coverage_lag2", 
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


options(scipen = 999)

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
