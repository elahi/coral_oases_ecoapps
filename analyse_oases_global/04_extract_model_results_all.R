################################################################################
##' @title Extract model summaries, all
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
library(MCMCvis)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

source("R/inverse_logit.R")

##### ZED0 #####

my_scale <- "zed0_"

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w0.csv")

## Get JAGS model output
zc <- readRDS(file = paste(path_to_model_rds, my_threshold, my_scale, this_model,
                           ".rds", sep = ""))

# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
colnames(X)

source("R/extract_model_results.R")

##### ZED1 #####

my_scale <- "zed1_"

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w1.csv")

## Get JAGS model output
zc <- readRDS(file = paste(path_to_model_rds, my_threshold, my_scale, this_model,
                           ".rds", sep = ""))

# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
colnames(X)

source("R/extract_model_results.R")

##### ZED2 #####

my_scale <- "zed2_"

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w2.csv")

## Get JAGS model output
zc <- readRDS(file = paste(path_to_model_rds, my_threshold, my_scale, this_model,
                           ".rds", sep = ""))

# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
colnames(X)

source("R/extract_model_results.R")

##### ZED3 #####

my_scale <- "zed3_"

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w3.csv")

## Get JAGS model output
zc <- readRDS(file = paste(path_to_model_rds, my_threshold, my_scale, this_model,
                           ".rds", sep = ""))

# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
colnames(X)

source("R/extract_model_results.R")
