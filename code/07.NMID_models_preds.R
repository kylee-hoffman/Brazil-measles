library(tidyverse)
library(pscl)
library(parameters)
library(stringr)
library(scales)
library(ggeffects)
library(MASS)
library(sandwich)
library(lmtest)
library(fixest)
library(ggplot2)
library(ggpubr)
library(gtools)
library(rsq)

################################################################################
##
## prediction for baseline models: all ages
##
################################################################################
#3a-3c
load("~/Brazil-measles/data/generated_data/models/NMID/mod3a.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3a_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3b.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3b_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3c.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3c_vcov.RData")

pred_3a <- predict_response(mod3a, 
                            terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod3a_vcov,
                            type = "fixed",
                            rg.limit = 50000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3a, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a.RData")


pred_3b <- predict_response(mod3b, 
                            terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod3b_vcov,
                            type = "fixed",
                            rg.limit = 50000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3b, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b.RData")


pred_3c <- predict_response(mod3c, 
                            terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod3c_vcov,
                            type = "fixed",
                            rg.limit = 50000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3c, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c.RData")


# 3d
load("~/Brazil-measles/data/generated_data/models/NMID/mod3d.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3d_vcov.RData")

# needed to break these up to prevent memory issues
pred_3d_1 <- predict_response(mod3d, 
                            terms = list(amnesia_prev_d1 = c(0, 1.0342, 2.0683, 
                                                             3.1025, 4.1366)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod3d_vcov,
                            type = "fixed",
                            rg.limit = 1000000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")

gc()

pred_3d_2 <- predict_response(mod3d, 
                              terms = list(amnesia_prev_d1 = c(5.1708, 6.2049, 7.2391, 
                                                               8.2733, 9.3074)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod3d_vcov,
                              type = "fixed",
                              rg.limit = 1000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")

pred_3d <- rbind(pred_3d_1, pred_3d_2)

save(pred_3d, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d.RData")



# 3e
load("~/Brazil-measles/data/generated_data/models/NMID/mod3e.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3e_vcov.RData")

pred_3e_1 <- predict_response(mod3e, 
                              terms = list(amnesia_prev_d2 = c(0, 1.0342, 2.0683, 
                                                               3.1025, 4.1366)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod3e_vcov,
                              type = "fixed",
                              rg.limit = 1000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 

gc()

pred_3e_2 <- predict_response(mod3e, 
                              terms = list(amnesia_prev_d2 = c(5.1708, 6.2049, 7.2391, 
                                                               8.2733, 9.3074)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod3e_vcov,
                              type = "fixed",
                              rg.limit = 1000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 

pred_3e <- rbind(pred_3e_1, pred_3e_2)

save(pred_3e, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e.RData")



# 3f
load("~/Brazil-measles/data/generated_data/models/NMID/mod3f.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3f_vcov.RData")

pred_3f_1 <- predict_response(mod3f, 
                              terms = list(amnesia_prev_d3 = c(0, 1.0342, 2.0683, 
                                                               3.1025, 4.1366)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod3f_vcov,
                              type = "fixed",
                              rg.limit = 1000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration") 

gc()

pred_3f_2 <- predict_response(mod3f, 
                              terms = list(amnesia_prev_d3 = c(5.1708, 6.2049, 7.2391, 
                                                               8.2733, 9.3074)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod3f_vcov,
                              type = "fixed",
                              rg.limit = 1000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration") 

pred_3f <- rbind(pred_3f_1, pred_3f_2)

save(pred_3f, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f.RData")


################################################################################
##
## prediction for baseline models: ages 1-9
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to9_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to9_vcov.RData")

pred_3a_1to9 <- predict_response(mod3a_1to9, 
                                 terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod3a_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3a_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a_1to9.RData")


pred_3b_1to9 <- predict_response(mod3b_1to9, 
                                 terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod3b_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3b_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b_1to9.RData")


pred_3c_1to9 <- predict_response(mod3c_1to9, 
                                 terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod3c_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3c_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c_1to9.RData")


# 3d: 1-9
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to9_vcov.RData")

pred_3d_1to9 <- predict_response(mod3d_1to9, 
                                 terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod3d_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 700000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3d_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d_1to9.RData")

rm(mod3d_1to9, mod3d_1to9_vcov)
gc()



# 3e: 1-9
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to9_vcov.RData")

pred_3e_1to9 <- predict_response(mod3e_1to9, 
                                 terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod3e_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 700000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3e_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e_1to9.RData")

rm(mod3e_1to9, mod3e_1to9_vcov)
gc()



# 3f: 1-9
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to9_vcov.RData")

pred_3f_1to9 <- predict_response(mod3f_1to9, 
                                 terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod3f_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 700000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3f_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f_1to9.RData")

rm(mod3f_1to9, mod3f_1to9_vcov)
gc()



################################################################################
##
## prediction for baseline models: ages 1-19
##
################################################################################
# 3a-3c
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3a_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3b_1to19_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3c_1to19_vcov.RData")

pred_3a_1to19 <- predict_response(mod3a_1to19, 
                                  terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3a_1to19_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3a_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a_1to19.RData")


pred_3b_1to19 <- predict_response(mod3b_1to19, 
                                  terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3b_1to19_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3b_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b_1to19.RData")


pred_3c_1to19 <- predict_response(mod3c_1to19, 
                                  terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3c_1to19_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3c_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c_1to19.RData")



# 3d: 1-19
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3d_1to19_vcov.RData")

pred_3d_1to19 <- predict_response(mod3d_1to19, 
                                  terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3d_1to19_vcov,
                                  type = "fixed",
                                  rg.limit = 800000,
                                  condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3d_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d_1to19.RData")

rm(mod3d_1to19, mod3d_1to19_vcov)
gc()


# 3e: 1-19
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3e_1to19_vcov.RData")

pred_3e_1to19 <- predict_response(mod3e_1to19, 
                                  terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3e_1to19_vcov,
                                  type = "fixed",
                                  rg.limit = 800000,
                                  condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3e_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e_1to19.RData")

rm(mod3e_1to19, mod3e_1to19_vcov)
gc()


# 3f: 1-19
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod3f_1to19_vcov.RData")

pred_3f_1to19 <- predict_response(mod3f_1to19, 
                                  terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3f_1to19_vcov,
                                  type = "fixed",
                                  rg.limit = 800000,
                                  condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3f_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f_1to19.RData")

rm(mod3f_1to19, mod3f_1to19_vcov)
gc()





################################################################################
##
## prediction for baseline models: ages 1-29
##
################################################################################
# 3a-3c
load("~/Brazil-measles/data/generated_data/models/NMID/mod3a_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3a_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3b_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3b_1to29_vcov.RData")

load("~/Brazil-measles/data/generated_data/models/NMID/mod3c_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3c_1to29_vcov.RData")

pred_3a_1to29 <- predict_response(mod3a_1to29, 
                                  terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3a_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3a_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3a_1to29.RData")


pred_3b_1to29 <- predict_response(mod3b_1to29, 
                                  terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3b_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3b_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3b_1to29.RData")


pred_3c_1to29 <- predict_response(mod3c_1to29, 
                                  terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3c_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3c_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3c_1to29.RData")



# 3d: 1-29
load("~/Brazil-measles/data/generated_data/models/NMID/mod3d_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3d_1to29_vcov.RData")

pred_3d_1to29 <- predict_response(mod3d_1to29, 
                                  terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3d_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 900000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_3d_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3d_1to29.RData")

rm(mod3d_1to29, mod3d_1to29_vcov)
gc()



# 3e: 1-29
load("~/Brazil-measles/data/generated_data/models/NMID/mod3e_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3e_1to29_vcov.RData")

pred_3e_1to29 <- predict_response(mod3e_1to29, 
                                  terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3e_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 900000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_3e_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3e_1to29.RData")

rm(mod3e_1to29, mod3e_1to29_vcov)
gc()



# 3f: 1-29
load("~/Brazil-measles/data/generated_data/models/NMID/mod3f_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod3f_1to29_vcov.RData")

pred_3f_1to29 <- predict_response(mod3f_1to29, 
                                  terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10)), 
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod3f_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 900000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_3f_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_3f_1to29.RData")

rm(mod3f_1to29, mod3f_1to29_vcov)
gc()




################################################################################
##
## predictions for interaction models: all ages
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/mod4a.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4a_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4b.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4b_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4c.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4c_vcov.RData")

pred_4a <- predict_response(mod4a, 
                            terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10),
                                         poverty_rate = c(17.13, 48.53, 78.78)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod4a_vcov,
                            type = "fixed",
                            rg.limit = 50000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4a, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a.RData")


pred_4b <- predict_response(mod4b, 
                            terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10),
                                         poverty_rate = c(17.13, 48.53, 78.78)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod4b_vcov,
                            type = "fixed",
                            rg.limit = 50000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4b, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b.RData")


pred_4c <- predict_response(mod4c, 
                            terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10),
                                         poverty_rate = c(17.13, 48.53, 78.78)), 
                            margin = "marginalmeans", 
                            ci_level = 0.95,
                            vcov = mod4c_vcov,
                            type = "fixed",
                            rg.limit = 50000,
                            condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4c, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c.RData")


rm(list = ls())
gc()


# 4d
load("~/Brazil-measles/data/generated_data/models/NMID/mod4d.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4d_vcov.RData")
# had to break this up
pred_4d_1 <- predict_response(mod4d,
                              terms = list(amnesia_prev_d1 = c(0, 1.0342),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4d_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4d_1, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1.RData")

gc()

pred_4d_2 <- predict_response(mod4d, 
                                    terms = list(amnesia_prev_d1 = c(2.0683, 3.1025),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4d_2, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_2.RData")

gc()

pred_4d_3 <- predict_response(mod4d, 
                                    terms = list(amnesia_prev_d1 = c(4.1366, 5.1708),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4d_3, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_3.RData")

gc()

pred_4d_4 <- predict_response(mod4d, 
                              terms = list(amnesia_prev_d1 = c(6.2049, 7.2391),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4d_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4d_4, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_4.RData")

gc()


pred_4d_5 <- predict_response(mod4d, 
                              terms = list(amnesia_prev_d1 = c(8.2733, 9.3074),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4d_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4d_5, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_5.RData")


pred_4d <- rbind(pred_4d_1, pred_4d_2, pred_4d_3, pred_4d_4, pred_4d_5)

save(pred_4d, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d.RData")


rm(mod4d, mod4d_vcov, pred_4d_1, pred_4d_2, pred_4d_3, pred_4d_4, pred_4d_5)
gc()



# 4e
load("~/Brazil-measles/data/generated_data/models/NMID/mod4e.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4e_vcov.RData")

pred_4e_1 <- predict_response(mod4e,
                              terms = list(amnesia_prev_d2 = c(0, 1.0342),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4e_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4e_1, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4e_1.RData")

gc()

pred_4e_2 <- predict_response(mod4e, 
                              terms = list(amnesia_prev_d2 = c(2.0683, 3.1025),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4e_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000))  %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4e_2, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4e_2.RData")

gc()

pred_4e_3 <- predict_response(mod4e, 
                              terms = list(amnesia_prev_d2 = c(4.1366, 5.1708),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4e_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4e_3, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4e_3.RData")

gc()

pred_4e_4 <- predict_response(mod4e, 
                              terms = list(amnesia_prev_d2 = c(6.2049, 7.2391),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4e_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4e_4, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4e_4.RData")


gc()

pred_4e_5 <- predict_response(mod4e, 
                              terms = list(amnesia_prev_d2 = c(8.2733, 9.3074),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4e_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4e_5, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4e_5.RData")


pred_4e <- rbind(pred_4e_1, pred_4e_2, pred_4e_3, pred_4e_4, pred_4e_5)

save(pred_4e, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e.RData")


rm(mod4e, mod4e_vcov, pred_4e_1, pred_4e_2, pred_4e_3, pred_4e_4, pred_4e_5)
gc()



# 4f
load("~/Brazil-measles/data/generated_data/models/NMID/mod4f.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4f_vcov.RData")

pred_4f_1 <- predict_response(mod4f,
                              terms = list(amnesia_prev_d3 = c(0, 1.0342),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4f_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4f_1, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4f_1.RData")

gc()

pred_4f_2 <- predict_response(mod4f, 
                              terms = list(amnesia_prev_d3 = c(2.0683, 3.1025),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4f_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4f_2, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4f_2.RData")

gc()

pred_4f_3 <- predict_response(mod4f, 
                              terms = list(amnesia_prev_d3 = c(4.1366, 5.1708),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4f_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4f_3, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4f_3.RData")

gc()

pred_4f_4 <- predict_response(mod4f, 
                              terms = list(amnesia_prev_d3 = c(6.2049, 7.2391),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4f_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration") 
save(pred_4f_4, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4f_4.RData")

gc()

pred_4f_5 <- predict_response(mod4f, 
                              terms = list(amnesia_prev_d3 = c(8.2733, 9.3074),
                                           poverty_rate = c(17.13, 48.53, 78.78)), 
                              margin = "marginalmeans", 
                              ci_level = 0.95,
                              vcov = mod4f_vcov,
                              type = "fixed",
                              rg.limit = 3000000,
                              condition = c(pop_total = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration") 
save(pred_4f_5, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4f_5.RData")


pred_4f <- rbind(pred_4f_1, pred_4f_2, pred_4f_3, pred_4f_4, pred_4f_5)

save(pred_4f, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f.RData")


rm(mod4f, mod4f_vcov)
gc()




################################################################################
##
## predictions for interaction models: ages 1-9
##
################################################################################
# 4a-4c: 1-9
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to9_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to9_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to9_vcov.RData")


pred_4a_1to9 <- predict_response(mod4a_1to9, 
                                 terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10),
                                              poverty_rate = c(17.13, 48.53, 78.78)),  
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod4a_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4a_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a_1to9.RData")


pred_4b_1to9 <- predict_response(mod4b_1to9, 
                                 terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10),
                                              poverty_rate = c(17.13, 48.53, 78.78)),  
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod4b_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4b_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b_1to9.RData")


pred_4c_1to9 <- predict_response(mod4c_1to9, 
                                 terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10),
                                              poverty_rate = c(17.13, 48.53, 78.78)),  
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod4c_1to9_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to9 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4c_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c_1to9.RData")


rm(mod4a_1to9, mod4a_1to9_vcov, mod4b_1to9,
   mod4b_1to9_vcov, mod4c_1to9, mod4c_1to9_vcov)
gc()


# 4d: 1-9
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to9.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to9_vcov.RData")

pred_4d_1to9_1 <- predict_response(mod4d_1to9, 
                                   terms = list(amnesia_prev_d1 = c(0, 1.0342, 2.0683, 
                                                                    3.1025, 4.1366),
                                                poverty_rate = c(17.13, 48.53, 78.78)), 
                                   margin = "marginalmeans", 
                                   ci_level = 0.95,
                                   vcov = mod4d_1to9_vcov,
                                   type = "fixed",
                                   rg.limit = 3000000,
                                   condition = c(pop_1to9 = 10000))

gc()

pred_4d_1to9_2 <- predict_response(mod4d_1to9, 
                                   terms = list(amnesia_prev_d1 = c(5.1708, 6.2049, 7.2391, 
                                                                    8.2733, 9.3074),
                                                poverty_rate = c(17.13, 48.53, 78.78)), 
                                   margin = "marginalmeans", 
                                   ci_level = 0.95,
                                   vcov = mod4d_1to9_vcov,
                                   type = "fixed",
                                   rg.limit = 3000000,
                                   condition = c(pop_1to9 = 10000))

pred_4d_1to9 <- rbind(pred_4d_1to9_1, pred_4d_1to9_2) %>% 
  data.frame() %>% mutate(duration = "1-year duration")

save(pred_4d_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d_1to9.RData")

rm(mod4d_1to9, mod4d_1to9_vcov)
gc()


# 4e: 1-9
pred_4e_1to9_1 <- predict_response(mod4e_1to9, 
                                   terms = list(amnesia_prev_d2 = c(0, 1.0342, 2.0683, 
                                                                    3.1025, 4.1366),
                                                poverty_rate = c(17.13, 48.53, 78.78)), 
                                   margin = "marginalmeans", 
                                   ci_level = 0.95,
                                   vcov = mod4e_1to9_vcov,
                                   type = "fixed",
                                   rg.limit = 3000000,
                                   condition = c(pop_1to9 = 10000))

gc()

pred_4e_1to9_2 <- predict_response(mod4e_1to9, 
                                   terms = list(amnesia_prev_d2 = c(5.1708, 6.2049, 7.2391, 
                                                                    8.2733, 9.3074),
                                                poverty_rate = c(17.13, 48.53, 78.78)), 
                                   margin = "marginalmeans", 
                                   ci_level = 0.95,
                                   vcov = mod4e_1to9_vcov,
                                   type = "fixed",
                                   rg.limit = 3000000,
                                   condition = c(pop_1to9 = 10000))

pred_4e_1to9 <- rbind(pred_4e_1to9_1, pred_4e_1to9_2) %>% 
  data.frame() %>% mutate(duration = "2-year duration")


save(pred_4e_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e_1to9.RData")


rm(mod4e_1to9, mod4e_1to9_vcov)
gc()


# 4f: 1-9
pred_4f_1to9_1 <- predict_response(mod4f_1to9, 
                                   terms = list(amnesia_prev_d3 = c(0, 1.0342, 2.0683, 
                                                                    3.1025, 4.1366),
                                                poverty_rate = c(17.13, 48.53, 78.78)), 
                                   margin = "marginalmeans", 
                                   ci_level = 0.95,
                                   vcov = mod4f_1to9_vcov,
                                   type = "fixed",
                                   rg.limit = 3000000,
                                   condition = c(pop_1to9 = 10000))

gc()

pred_4f_1to9_2 <- predict_response(mod4f_1to9, 
                                   terms = list(amnesia_prev_d3 = c(5.1708, 6.2049, 7.2391, 
                                                                    8.2733, 9.3074),
                                                poverty_rate = c(17.13, 48.53, 78.78)), 
                                   margin = "marginalmeans", 
                                   ci_level = 0.95,
                                   vcov = mod4f_1to9_vcov,
                                   type = "fixed",
                                   rg.limit = 3000000,
                                   condition = c(pop_1to9 = 10000))

pred_4f_1to9 <- rbind(pred_4f_1to9_1, pred_4f_1to9_2) %>% 
  data.frame() %>% mutate(duration = "3-year duration")


save(pred_4f_1to9, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f_1to9.RData")


rm(mod4f_1to9, mod4f_1to9_vcov)
gc()




################################################################################
##
## predictions for interaction models: ages 1-19
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4a_1to19_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4b_1to19_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4c_1to19_vcov.RData")


pred_4a_1to19 <- predict_response(mod4a_1to19, 
                                 terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10),
                                              poverty_rate = c(17.13, 48.53, 78.78)),  
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod4a_1to19_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4a_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a_1to19.RData")


pred_4b_1to19 <- predict_response(mod4b_1to19, 
                                 terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10),
                                              poverty_rate = c(17.13, 48.53, 78.78)),  
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod4b_1to19_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4b_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b_1to19.RData")


pred_4c_1to19 <- predict_response(mod4c_1to19, 
                                 terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10),
                                              poverty_rate = c(17.13, 48.53, 78.78)),  
                                 margin = "marginalmeans", 
                                 ci_level = 0.95,
                                 vcov = mod4c_1to19_vcov,
                                 type = "fixed",
                                 rg.limit = 50000,
                                 condition = c(pop_1to19 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4c_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c_1to19.RData")


rm(mod4a_1to19, mod4a_1to19_vcov, mod4b_1to19,
   mod4b_1to19_vcov, mod4c_1to19, mod4c_1to19_vcov)
gc()



# 4d: 1-19
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4d_1to19_vcov.RData")

# had to break this up
pred_4d_1to19_1 <- predict_response(mod4d_1to19, 
                                    terms = list(amnesia_prev_d1 = c(0, 1.0342, 2.0683),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))
save(pred_4d_1to19_1, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1to19_1.RData")


gc()


pred_4d_1to19_2 <- predict_response(mod4d_1to19, 
                                    terms = list(amnesia_prev_d1 = c(3.1025, 4.1366, 5.1708),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))
save(pred_4d_1to19_2, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1to19_2.RData")


pred_4d_1to19_3 <- predict_response(mod4d_1to19, 
                                    terms = list(amnesia_prev_d1 = c(6.2049, 7.2391, 8.2733, 9.3074),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))
save(pred_4d_1to19_3, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1to19_3.RData")


gc()


pred_4d_1to19 <- rbind(pred_4d_1to19_1, pred_4d_1to19_2, pred_4d_1to19_3) %>% 
  data.frame() %>% mutate(duration = "1-year duration")

save(pred_4d_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d_1to19.RData")


rm(mod4d_1to19, mod4d_1to19_vcov)
gc()



# 4e: 1-19
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4e_1to19_vcov.RData")

pred_4e_1to19_1 <- predict_response(mod4e_1to19, 
                                    terms = list(amnesia_prev_d2 = c(0, 1.0342, 2.0683, 3.1025),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4e_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))

gc()

pred_4e_1to19_2 <- predict_response(mod4e_1to19, 
                                    terms = list(amnesia_prev_d2 = c(4.1366, 5.1708, 6.2049),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4e_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))

pred_4e_1to19_3 <- predict_response(mod4e_1to19, 
                                    terms = list(amnesia_prev_d2 = c(7.2391, 8.2733, 9.3074),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4e_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))

pred_4e_1to19 <- rbind(pred_4e_1to19_1, pred_4e_1to19_2, pred_4e_1to19_3) %>% 
  data.frame() %>% mutate(duration = "2-year duration")


save(pred_4e_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e_1to19.RData")


rm(mod4e_1to19, mod4e_1to19_vcov)
gc()



# 4f: 1-19
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to19.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/childhood/mod4f_1to19_vcov.RData")

pred_4f_1to19_1 <- predict_response(mod4f_1to19, 
                                    terms = list(amnesia_prev_d3 = c(0, 1.0342, 2.0683, 3.1025),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4f_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))

gc()

pred_4f_1to19_2 <- predict_response(mod4f_1to19, 
                                    terms = list(amnesia_prev_d3 = c(4.1366, 5.1708, 6.2049),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4f_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))

gc()

pred_4f_1to19_3 <- predict_response(mod4f_1to19, 
                                    terms = list(amnesia_prev_d3 = c(7.2391, 8.2733, 9.3074),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4f_1to19_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to19 = 10000))


pred_4f_1to19 <- rbind(pred_4f_1to19_1, pred_4f_1to19_2, pred_4f_1to19_3) %>% 
  data.frame() %>% mutate(duration = "3-year duration")

save(pred_4f_1to19, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f_1to19.RData")


rm(mod4f_1to19, mod4f_1to19_vcov)
gc()






################################################################################
##
## predictions for interaction models: ages 1-29
##
################################################################################
load("~/Brazil-measles/data/generated_data/models/NMID/mod4a_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4a_1to29_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4b_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4b_1to29_vcov.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4c_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4c_1to29_vcov.RData")


pred_4a_1to29 <- predict_response(mod4a_1to29, 
                                  terms = list(amnesia_prev_d1 = seq(0, 9.30741, length.out = 10),
                                               poverty_rate = c(17.13, 48.53, 78.78)),  
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod4a_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "1-year duration")
save(pred_4a_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4a_1to29.RData")


pred_4b_1to29 <- predict_response(mod4b_1to29, 
                                  terms = list(amnesia_prev_d2 = seq(0, 9.30741, length.out = 10),
                                               poverty_rate = c(17.13, 48.53, 78.78)),  
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod4b_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "2-year duration") 
save(pred_4b_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4b_1to29.RData")


pred_4c_1to29 <- predict_response(mod4c_1to29, 
                                  terms = list(amnesia_prev_d3 = seq(0, 9.30741, length.out = 10),
                                               poverty_rate = c(17.13, 48.53, 78.78)),  
                                  margin = "marginalmeans", 
                                  ci_level = 0.95,
                                  vcov = mod4c_1to29_vcov,
                                  type = "fixed",
                                  rg.limit = 50000,
                                  condition = c(pop_1to29 = 10000)) %>% 
  data.frame() %>% mutate(duration = "3-year duration")
save(pred_4c_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4c_1to29.RData")


rm(mod4a_1to29, mod4a_1to29_vcov, mod4b_1to29,
   mod4b_1to29_vcov, mod4c_1to29, mod4c_1to29_vcov)
gc()



# 4d: 1-29
load("~/Brazil-measles/data/generated_data/models/NMID/mod4d_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4d_1to29_vcov.RData")

# had to break this up
pred_4d_1to29_1 <- predict_response(mod4d_1to29, 
                                    terms = list(amnesia_prev_d1 = c(0, 1.0342, 2.0683),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))
save(pred_4d_1to29_1, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1to29_1.RData")


gc()


pred_4d_1to29_2 <- predict_response(mod4d_1to29, 
                                    terms = list(amnesia_prev_d1 = c(3.1025, 4.1366, 5.1708),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))
save(pred_4d_1to29_2, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1to29_2.RData")


pred_4d_1to29_3 <- predict_response(mod4d_1to29, 
                                    terms = list(amnesia_prev_d1 = c(6.2049, 7.2391, 8.2733, 9.3074),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4d_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))
save(pred_4d_1to29_3, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/temp/pred_4d_1to29_3.RData")


gc()


pred_4d_1to29 <- rbind(pred_4d_1to29_1, pred_4d_1to29_2, pred_4d_1to29_3) %>% 
  data.frame() %>% mutate(duration = "1-year duration")

save(pred_4d_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4d_1to29.RData")


rm(mod4d_1to29, mod4d_1to29_vcov)
gc()



# 4e: 1-29
load("~/Brazil-measles/data/generated_data/models/NMID/mod4e_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4e_1to29_vcov.RData")

pred_4e_1to29_1 <- predict_response(mod4e_1to29, 
                                    terms = list(amnesia_prev_d2 = c(0, 1.0342, 2.0683, 3.1025),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4e_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))

gc()

pred_4e_1to29_2 <- predict_response(mod4e_1to29, 
                                    terms = list(amnesia_prev_d2 = c(4.1366, 5.1708, 6.2049),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4e_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))

pred_4e_1to29_3 <- predict_response(mod4e_1to29, 
                                    terms = list(amnesia_prev_d2 = c(7.2391, 8.2733, 9.3074),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4e_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))

pred_4e_1to29 <- rbind(pred_4e_1to29_1, pred_4e_1to29_2, pred_4e_1to29_3) %>% 
  data.frame() %>% mutate(duration = "2-year duration")


save(pred_4e_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4e_1to29.RData")


rm(mod4e_1to29, mod4e_1to29_vcov)
gc()



# 4f: 1-29
load("~/Brazil-measles/data/generated_data/models/NMID/mod4f_1to29.RData")
load("~/Brazil-measles/data/generated_data/models/NMID/mod4f_1to29_vcov.RData")

pred_4f_1to29_1 <- predict_response(mod4f_1to29, 
                                    terms = list(amnesia_prev_d3 = c(0, 1.0342, 2.0683, 3.1025),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4f_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))

gc()

pred_4f_1to29_2 <- predict_response(mod4f_1to29, 
                                    terms = list(amnesia_prev_d3 = c(4.1366, 5.1708, 6.2049),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4f_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))

gc()

pred_4f_1to29_3 <- predict_response(mod4f_1to29, 
                                    terms = list(amnesia_prev_d3 = c(7.2391, 8.2733, 9.3074),
                                                 poverty_rate = c(17.13, 48.53, 78.78)), 
                                    margin = "marginalmeans", 
                                    ci_level = 0.95,
                                    vcov = mod4f_1to29_vcov,
                                    type = "fixed",
                                    rg.limit = 3000000,
                                    condition = c(pop_1to29 = 10000))


pred_4f_1to29 <- rbind(pred_4f_1to29_1, pred_4f_1to29_2, pred_4f_1to29_3) %>% 
  data.frame() %>% mutate(duration = "3-year duration")

save(pred_4f_1to29, file = "~/Brazil-measles/data/generated_data/models/NMID/preds/pred_4f_1to29.RData")


rm(mod4f_1to29, mod4f_1to29_vcov)
gc()


