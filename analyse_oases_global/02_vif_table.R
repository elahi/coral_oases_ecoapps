################################################################################
##' @title Create table of variance inflation factors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-04-24
##' @log 2020-07-23: changed gridded data
##'      2021-07-05: updated subjurisdictions
##'      2021-08-06: removed duplicate sites
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
library(tidyverse)

my_scale <- "zed0_"
d0 <- read.csv(paste(path_to_output, my_scale, "vif_table.csv", sep = "")) %>% 
  mutate(zed_scale = "Global")

my_scale <- "zed1_"
d1 <- read.csv(paste(path_to_output, my_scale, "vif_table.csv", sep = "")) %>% 
  mutate(zed_scale = "Ocean")

my_scale <- "zed2_"
d2 <- read.csv(paste(path_to_output, my_scale, "vif_table.csv", sep = "")) %>% 
  mutate(zed_scale = "Jurisdiction")

my_scale <- "zed3_"
d3 <- read.csv(paste(path_to_output, my_scale, "vif_table.csv", sep = "")) %>% 
  mutate(zed_scale = "Sub-jurisdiction")

dat <- rbind(d0, d1, d2, d3) %>% select(-X)
dat

write.csv(dat, paste(path_to_output, "vif_table_long.csv", sep = ""))

## Create matrix of final VIFs
dat_wide <- dat %>% 
  select(zed_scale, covariate, GVIF_final) %>% 
  spread(key = zed_scale, value = GVIF_final) %>% 
  rename(Covariate = covariate) %>% 
  select(Covariate, Global, Ocean, Jurisdiction, `Sub-jurisdiction`)
dat_wide <- format(dat_wide, digits = 2, nsmall = 2)
write.csv(dat_wide, paste(path_to_output, "vif_table_final.csv", sep = ""))

## Create matrix of column sample size
dat_columns <- dat %>% 
  select(zed_scale, covariate, column_n) %>% 
  spread(key = zed_scale, value = column_n) %>% 
  rename(Covariate = covariate) %>% 
  select(Covariate, Global, Ocean, Jurisdiction, `Sub-jurisdiction`)
dat_columns <- format(dat_columns, digits = 2, nsmall = 2)
write.csv(dat_columns, paste(path_to_output, "column_n_table_final.csv", sep = ""))

