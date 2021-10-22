################################################################################
##' @title Compile Bayesian P values
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-30
##' @log  
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES #####
library(tidyverse)
library(viridis)
library(forcats)
library(broom)
library(patchwork)

theme_set(theme_bw(base_size = 11) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

source("R/inverse_logit.R")

##### COMPILE RESULTS #####

my_files <- list.files(path = path_to_model_output, pattern = "bpv")
my_files

## Get files
fit <- read.csv(file = paste(path_to_model_output, my_files[1], sep = ""))
fit0 <- fit %>% mutate(scale = "Cross-basin")

fit <- read.csv(file = paste(path_to_model_output, my_files[2], sep = ""))
fit1 <- fit %>% mutate(scale = "Basin")

fit <- read.csv(file = paste(path_to_model_output, my_files[3], sep = ""))
fit2 <- fit %>% mutate(scale = "Region")

fit <- read.csv(file = paste(path_to_model_output, my_files[4], sep = ""))
fit3 <- fit %>% mutate(scale = "Sub-region")

fit_df <- rbind(fit0, fit1, fit2, fit3) 

## Reorder jurisdiction
unique(fit_df$scale)
fit_df <- fit_df %>%
  mutate(scale = factor(scale, levels = c("Cross-basin", "Basin", "Region", "Sub-region"))) 

fit_df_long <- fit_df %>% 
  select(scale, .variable, mean) %>% 
  spread(key = .variable, value = mean) %>% 
  select(scale, p.chi2b, p.ftd) %>%
  rename(`Spatial extent` = scale, Chi2 = p.chi2b, `Freeman-Tukey` = p.ftd)

fit_df_long
write.csv(fit_df_long, paste(path_to_output, "bpv_summary.csv", sep = ""))
