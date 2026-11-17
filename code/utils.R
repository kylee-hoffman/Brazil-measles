# reading data
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
  read_xlsx(file, col_types = "text", .name_repair = "unique_quiet") %>% 
    clean_names() %>% 
    filter(!(municipio_residencia %in% c("Totais", "", NA))) %>% 
    mutate(muni_code_6 = substr(municipio_residencia, 1, 6),
           MMR1_coverage = as.numeric(triplice_viral_1_dose) * 100,
           year = year) %>% 
    dplyr::select(muni_code_6, year, MMR1_coverage)
}


# make correlation plots
correlation_plot_vax_fun <- function(df, plot, x_col, y_col, lag_string, y_axis_text) {
  # correlation coefficient
  r <- df %>% summarise(r = cor({{ x_col }}, {{ y_col }}, method = "pearson")) %>%
    pull(r) %>% round(3) %>% format(nsmall = 3)
  
  ggplot(df %>% filter({{ y_col }} > 0), 
         aes(x = {{ x_col }}, y = {{ y_col }})) +
    geom_point(shape = 21, color = '#0a4466', stroke = 0.3, fill = '#accfe3', alpha = 0.5, size = 1) + 
    geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
    labs(x = paste("MCV1 coverage", lag_string), y = y_axis_text) +
    #scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
    #              labels = c("0.001", "0.01", "0.1", "1.0", "10.0")) +
    scale_y_log10() +
    coord_cartesian(xlim = c(6.5, 97), ylim = c(0.0001, 15)) +
    annotate("text", x = 7, y = 11, family = "Myriad Pro Semibold", size = 3.7, hjust = 0,
             label = paste("Pearson's r:", r)) + 
    theme_bw(base_family = "Myriad Pro", base_size = 12) +
    theme(text = element_text(color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.3),
          axis.ticks = element_line(size = 0.3, color = "black"),
          plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"))
}



# make correlation plots
correlation_plot_fun <- function(df, plot, x_col, y_col, x_axis_text, y_axis_text, 
                                 ylim, label_pos) {
  # correlation coefficient
  r <- df %>% summarise(r = cor({{ x_col }}, {{ y_col }}, method = "pearson")) %>%
    pull(r) %>% round(3) %>% format(nsmall = 3)
  
  color = case_when(plot == "NMID" ~ "#87406f",
                    plot == "cancer" ~ "gray30")
  
  fill = case_when(plot == "NMID" ~ "#f5bfe3",
                   plot == "cancer" ~ "gray90")
  
  ggplot(df %>% filter({{ x_col }} > 0 & {{ y_col }} > 0), 
         aes(x = {{ x_col }} * 10, y = {{ y_col }} * 10)) +
    geom_point(shape = 21, color = color, stroke = 0.3, fill = fill, alpha = 0.5, size = 1) + 
    geom_smooth(method = "lm", color = "black", fill = "#b6b9ba", alpha = 0.6, linewidth = 0.3) + 
    labs(x = x_axis_text, y = y_axis_text) +
    scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100),
                  labels = c("0.01", "0.1", "1.0", "10.0", "100.0")) +
    scale_y_log10() +
    coord_cartesian(xlim = c(0.0014, 57.5), ylim = ylim) +
    annotate("text", x = 0.0015, y = label_pos, family = "Myriad Pro Semibold", size = 3.7, hjust = 0,
             label = paste("Pearson's r:", r)) + 
    theme_bw(base_family = "Myriad Pro", base_size = 12) +
    theme(text = element_text(color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.3),
          plot.background = element_blank(),
          axis.ticks = element_line(linewidth = 0.3, color = "black"),
          plot.margin=margin(t = 0, r = 4, b = 0, l = 0, unit = "mm"))
}



# lm coef table
lm_tab <- function(model, vcov) {
  as.data.frame(coeftest(model, vcov = vcov)[,]) %>% 
    rownames_to_column(var = "Term") %>% 
    mutate(sig = cut(`Pr(>|t|)`, 
                     breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                     labels = c("***", "**", "*", ""), 
                     right = FALSE),
           Estimate = paste0(round(Estimate, 3), sig),
           SE = as.character(round(`Std. Error`, 3))) %>% 
    dplyr::select(Term, Estimate, SE)
}


# odds ratio table
or_tab_fun <- function(model, vcov) {
  coefs <- as.data.frame(coeftest(model, vcov = vcov)[,]) %>% 
    rownames_to_column(var = "Term") %>% 
    filter(!str_detect(Term, "Intercept|muni_code_6|state"))
  
  ci <- coefci(model, vcov = vcov) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "Term") %>% 
    filter(!str_detect(Term, "Intercept|muni_code_6|state"))
  
  return(
    merge(coefs, ci, by = "Term", all = T) %>% 
      mutate(sig = cut(`Pr(>|z|)`, 
                       breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                       labels = c("***", "**", "*", ".", "")),
             OR = paste0(format(round(exp(Estimate), 2), nsmall = 2), sig),
             lower.CI = exp(`2.5 %`),
             upper.CI = exp(`97.5 %`),
             Robust.CI = paste0("(", round(lower.CI, 2), ", ", round(upper.CI, 2), ")")) %>% 
      dplyr::select(Term, OR, Robust.CI))
}

# incidence rate ratio table
irr_tab_fun <- function(model, vcov) {
  coefs <- as.data.frame(coeftest(model, vcov = vcov)[,]) %>% 
    rownames_to_column(var = "Term") %>% 
    filter(!str_detect(Term, "muni_code_6|state"))
  
  ci <- coefci(model, vcov = vcov) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "Term") %>% 
    filter(!str_detect(Term, "muni_code_6|state"))
  
  return(
    merge(coefs, ci, by = "Term", all = T) %>% 
      mutate(sig = cut(`Pr(>|z|)`, 
                       breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                       labels = c("***", "**", "*", ".", "")),
             IRR = paste0(format(round(exp(Estimate), 2), nsmall = 2), sig),
             lower.CI = exp(`2.5 %`),
             upper.CI = exp(`97.5 %`),
             Robust.CI = paste0("(", round(lower.CI, 2), ", ", 
                                round(upper.CI, 2), ")")) %>% 
      dplyr::select(Term, IRR, Robust.CI))
}




# plots of predicted vaccination levels
vax_preds_plot_fun <- function(model, vcov, ylim, ybreaks, ylabs) {
  theme <- theme_bw(base_family = "Myriad Pro", base_size = 11) +
    theme(text = element_text(color = "black"),
          axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 11, margin = margin(0, 0, 2, 0, "mm")),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.3),
          plot.background = element_blank(),
          axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
          axis.ticks.y = element_blank(),
          plot.margin=margin(t = 0.5, r = 3, b = 1.5, l = 0, unit = "mm"),
          legend.background = element_blank())
  
  pov_p <- predict_response(model, 
                            terms = list(poverty_rate = seq(17.13, 78.78, 
                                                            length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = vcov,
                            type = "fixed") %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Poverty rate", y = "Predicted MCV1\ncoverage") + 
    coord_cartesian(ylim = ylim, xlim = c(19.7, 76.2)) +
    scale_y_continuous(breaks = ybreaks,
                       labels = ylabs) +
    scale_x_continuous(breaks = c(25, 50, 75)) +
    theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 2, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  #quantile(df$pct_urban, probs = c(0.1, 0.9), na.rm=T)
  # 33.14 92.87
  urban_p <- predict_response(model, 
                              terms = list(pct_urban = seq(33.14, 92.87, 
                                                           length.out = 20)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = vcov,
                              type = "fixed") %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "%Urban", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(35.6, 90.4)) +
    scale_x_continuous(breaks = c(40, 60, 80)) + theme
  
  
  #quantile(df$clinics_pc_scaled, probs = c(0.1, 0.9), na.rm=T)
  # -1.145079  1.159421 
  clinic_p <-  predict_response(model, 
                                terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                                     length.out = 20)), 
                                margin = "marginalmeans", 
                                ci_level = 0.95,
                                vcov = vcov,
                                type = "fixed") %>% data.frame() %>% 
    ggplot(aes(x = x * sd(df$clinics_pc) + mean(df$clinics_pc),
               y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#edbd91", alpha = 0.4) +
    geom_line(linewidth = 0.3) +
    labs(x = "Clinics per 1,000", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(0.11, 0.527)) +
    scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme
  
  
  # quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
  # 4688.17 41242.88 
  gdp_p <- predict_response(model, 
                            terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                      length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = vcov,
                            type = "fixed") %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#115870", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "GDP PC", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(6200, 39700)) +
    scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                       labels = c("10", "20", "30", "40")) + theme
  
  
  # quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
  # 9.170367 17.785892
  cbr_p <- predict_response(model, 
                            terms = list(cbr = seq(9.170367, 17.785892, length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = vcov,
                            type = "fixed") %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8783D1", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Crude birth rate", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(9.54, 17.42)) +
    scale_x_continuous(breaks = c(10, 12, 14, 16, 18, 20)) + theme
  
  
  # quantile(df$gini, probs = c(0.1, 0.9), na.rm=T)
  # 42.67 60.17
  gini_p <- predict_response(model, 
                             terms = list(gini = seq(42.67, 60.17, length.out = 10)), 
                             margin = "marginalmeans", 
                             ci_level = 0.95,
                             vcov = vcov,
                             type = "fixed") %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#ACC196", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Gini index", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(43.35, 59.48)) +
    scale_x_continuous(breaks = c(45, 50, 55, 60),
                       labels = c("0.45", "0.5", "0.55", "0.6")) + theme
  
  
  return(ggarrange(pov_p, cbr_p, gini_p, urban_p, clinic_p, gdp_p,
                   nrow = 1, widths = c(1.635, 1, 1, 1, 1, 1)) + 
           theme(plot.margin=margin(t = 0, r = 1, b = -0.5, l = 1, unit = "mm")))
  
}





# plot of predicted probs from logit models with state FE
pred_prob_plot_fun.state_fe <- function(model, vcov, ylim, ybreaks) {
  
  theme <- theme_bw(base_family = "Myriad Pro", base_size = 11) +
    theme(text = element_text(color = "black"),
          axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 11, margin = margin(1, 0, 1, 0, "mm")),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black", linewidth = 0.3),
          plot.background = element_blank(),
          axis.ticks.x = element_line(linewidth = 0.3, color = "black"),
          axis.ticks.y = element_blank(),
          plot.margin=margin(t = 0.5, r = 1, b = 1.5, l = 1, unit = "mm"),
          legend.background = element_blank())
  
  
  mcv_p <- predict_response(model,
                            terms = list(mcv_d1_cov_tc_lag2 = seq(0, 100, by = 10)),
                            margin = "marginalmeans",
                            ci_level = 0.95,
                            type = "fixed", 
                            vcov = vcov,
                            rg.limit = 50000) %>%
    data.frame() %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linewidth = 0, fill ="#a7c5e8", alpha = 0.7) +
    geom_line(linewidth = 0.3) +
    scale_x_continuous(breaks = c(0, 20, 40, 60, 80)) +
    scale_y_continuous(labels = scales::percent, breaks = ybreaks) +
    coord_cartesian(ylim = ylim, xlim = c(4.4, 95.6)) +
    labs(x = "MCV1 coverage\n2 years prior", y = NULL) + 
    theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 1, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  
  pov_p <- predict_response(model, 
                            terms = list(poverty_rate = seq(17.13, 78.78, 
                                                            length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#f2d78d", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    coord_cartesian(ylim = ylim, xlim = c(19.8, 76.1)) +
    scale_x_continuous(breaks = c(25, 50, 75)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    labs(x = "Poverty rate\n", y = NULL) + theme
  
  
  urban_p <- predict_response(model, 
                              terms = list(pct_urban = seq(33.14, 92.87, 
                                                           length.out = 10)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              type = "fixed",
                              vcov = vcov,
                              rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#E5C1BD", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "% Urban\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(35.6, 90.4)) +
    scale_y_continuous(labels = scales::percent, breaks = ybreaks) +
    scale_x_continuous(breaks = c(40, 60, 80)) + theme +
    theme(axis.text.y = element_text(size = 11, color = "black", 
                                     margin = margin(1, 1, 1, 1, unit = "mm")),
          axis.ticks.y = element_line(linewidth = 0.3, color = "black"))
  
  
  #quantile(df$clinics_pc_scaled, probs = c(0.1, 0.9), na.rm=T)
  # -1.145079  1.159421 
  clinic_p <- predict_response(model, 
                               terms = list(clinics_pc_scaled = seq(-1.145079, 1.159421,
                                                             length.out = 10)), 
                               margin = "marginalmeans", 
                               ci_level = 0.95,
                               type = "fixed",
                               vcov = vcov,
                               rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x * sd(df$clinics_pc) + mean(df$clinics_pc),
               y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#edbd91", alpha = 0.4) +
    geom_line(linewidth = 0.3) +
    labs(x = "Clinics per 1,000\n", y = NULL) +
    coord_cartesian(ylim = ylim, xlim = c(0.11, 0.527)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    scale_x_continuous(breaks = c(0.1, 0.3, 0.5)) + theme
  
  
  # quantile(df$gdp_pc, probs = c(0.1, 0.9), na.rm=T)
  # 4688.17 41242.88 
  gdp_p <- predict_response(model, 
                            terms = list(gdp_pc = seq(4688.17, 41242.88, 
                                                      length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#114B5F", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "GDP PC\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(6200, 39700)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) +
    scale_x_continuous(breaks = c(10000, 20000, 30000, 40000),
                       labels = c("10", "20", "30", "40")) + theme
  
  
  #quantile(df$gini, probs = c(0.1, 0.9), na.rm=T)
  # 42.67 60.17 
  gini_p <- predict_response(model, 
                             terms = list(gini = seq(42.67, 60.17, 
                                                     length.out = 10)), 
                             margin = "marginalmeans", 
                             ci_level = 0.95,
                             type = "fixed",
                             vcov = vcov,
                             rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#ACC196", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Gini index\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(43.43, 59.41)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  #quantile(df$cbr, probs = c(0.1, 0.9), na.rm=T)
  # 9.170367 17.785892
  cbr_p <- predict_response(model, 
                            terms = list(cbr = seq(9.170367, 17.785892, 
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8783D1", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Births per 1,000\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(9.6, 17.4)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  
  # quantile(df$imr, probs = c(0.1, 0.9), na.rm=T)
  # 0.00000 28.90173 
  imr_p <- predict_response(model, 
                            terms = list(imr = seq(0.00, 28.90173,
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#b05a45", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Infant mortality rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(1.24, 27.64)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  # quantile(df$cdr, probs = c(0.1, 0.9), na.rm=T)
  # 3.949247 9.060420 
  cdr_p <- predict_response(model, 
                            terms = list(cdr = seq(3.949247, 9.060420,
                                                   length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            type = "fixed",
                            vcov = vcov,
                            rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                linewidth = 0, fill ="#8ebfad", alpha = 0.5) +
    geom_line(linewidth = 0.3) +
    labs(x = "Crude death rate\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(4.16, 8.85)) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  travel_p <- predict_response(model, 
                               terms = list(travel = c(0, 1)), 
                               margin = "marginalmeans", 
                               ci_level = 0.95,
                               type = "fixed",
                               vcov = vcov,
                               rg.limit = 50000) %>% data.frame() %>% 
    ggplot(aes(x = x, y = predicted)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.3) +
    geom_point(size = 0.7) +
    labs(x = "Travel\n", y = NULL) + 
    coord_cartesian(ylim = ylim, xlim = c(1.3, 1.7)) +
    scale_x_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.002, 0.004, 0.006, 0.008, 0.01)) + theme
  
  
  return(ggarrange(mcv_p, clinic_p, pov_p, cdr_p, travel_p,
                   urban_p, gini_p, cbr_p, gdp_p, imr_p,
                   nrow = 2, 
                   ncol = 5,
                   widths = c(1.3, 1, 1, 1, 1)
                   ) 
         + theme(plot.margin = margin(2, 0, -1, 6, unit = "mm")))
}
