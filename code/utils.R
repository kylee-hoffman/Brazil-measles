read_datasus <- function(file, line_skip, drop_cols, values_to) {
  read.delim(file, sep = ";", fileEncoding = "Latin1", dec = ",", skip = line_skip, nrows = 5597) %>% 
    clean_names() %>% 
    dplyr::select(-any_of(drop_cols)) %>%
    filter(!str_detect(municipio, "IGNORADO|Total|EXTINTO|510183")) %>% # 510183 was est. in 2025
    pivot_longer(cols = -"municipio", values_to = values_to) %>% 
    mutate(year = substr(name, 2, 5),
           muni_code_6 = str_split_i(municipio, " ", 1)) %>% 
    dplyr::select(-c(municipio, name)) %>% 
    mutate(across(-c(muni_code_6, year), ~ as.numeric(ifelse(.x == "-", 0, .x))))
}


read_cases <- function(file, skip, drop_cols) {
  read.delim(file, sep = ";", fileEncoding = "Latin1", skip = skip, nrow = 5597) %>%
    clean_names() %>% 
    dplyr::select(-any_of(drop_cols)) %>%
    filter(!str_detect(municipio_de_residencia, "IGNORADO|510183")) %>% 
    mutate(across(contains("x"), ~ as.numeric(ifelse(.x == "-", 0, .x))))
}


read_gdp <- function(file) {
  read_xlsx(file) %>% 
    clean_names() %>%
    rename(year = ano, gdp_pc = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00) %>%
    mutate(muni_code_6 = as.numeric(substr(as.character(codigo_do_municipio), 1, 6))) %>% 
    dplyr::select(muni_code_6, year, gdp_pc)
}


read_coverage <- function(file, year) {
  read_xlsx(file) %>% 
    clean_names() %>% 
    filter(!(municipio_residencia %in% c("Totais", "", NA))) %>% 
    mutate(muni_code_6 = substr(municipio_residencia, 1, 6),
           MMR1_coverage = as.numeric(triplice_viral_1_dose) * 100,
           year = year) %>% 
    dplyr::select(muni_code_6, year, MMR1_coverage)
}


# incidence rate ratio table
rr_tab <- function(mod) {
  cov.mod <- vcov(mod)
  std.err <- sqrt(diag(cov.mod))
  
  r.est <- na.omit(cbind(IRR = coef(mod, complete = FALSE), 
                         "robust_SE" = std.err,
                         "Pr(>|z|)" = 2 * pnorm(abs(coef(mod, complete = FALSE)/std.err), lower.tail = FALSE),
                         "lower CI" = coef(mod, complete = FALSE) - 1.96 * std.err,
                         "upper CI" = coef(mod, complete = FALSE) + 1.96 * std.err))
  
  deltas <- lapply(1:length(coef(mod, complete = FALSE)), function(i) as.formula(paste0("~ exp(x", i, ")")))
  s <- deltamethod(deltas, coef(mod, complete = FALSE), cov.mod)
  
  rexp.est <- exp(r.est[, -3]) # exponentiate estimates and confidence intervals, dropping p values
  rexp.est[, "robust_SE"] <- s # replace SEs with delta method estimates
  
  tab <- round(rexp.est, 3) %>% as.data.frame() %>% rownames_to_column() %>% 
    mutate(CI = paste0("(", `lower CI`, ", ", `upper CI`, ")")) %>% 
    dplyr::select(Term = rowname, IRR, CI)
}
