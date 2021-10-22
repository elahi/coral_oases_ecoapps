################################################################################
##' @title Extract model summaries
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-08-26
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
my_threshold <- "z2_"
this_model <- "occu_hier_binom"

##### PACKAGES #####
library(tidyverse)
library(tidybayes)
library(viridis)
library(forcats)
library(rjags)
library(broom)
library(patchwork)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

my_scale <- "zed2_"

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w2.csv")

## Get JAGS model output
zc <- readRDS(file = paste(path_to_ignored_model_rds, my_threshold, my_scale, this_model,
                           ".rds", sep = ""))

##### PSI #####
psi_df <- zc %>% 
  gather_draws(psi[i])

psi_coefs_prob_df <- psi_df %>%
  median_qi(.width = c(0.5)) %>% 
  ungroup() 

psi_coefs_prob_df <- cbind(psi_coefs_prob_df, 
                      cell_2.5 = rep(dat$cell_2.5, 2))

write.csv(psi_coefs_prob_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "psi_coefs_prob_0.5_df", ".csv", sep = ""))
