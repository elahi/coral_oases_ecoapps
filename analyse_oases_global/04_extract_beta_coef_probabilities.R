################################################################################
##' @title Extract beta coef probabilities
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-12-11
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

my_threshold <- "z2_"
this_model <- "occu_hier_binom"

##### PACKAGES #####
library(tidyverse)
library(tidybayes)
library(forcats)
library(rjags)
library(broom)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

source("R/inverse_logit.R")

##### ZED 0, 1 #####

# my_scale <- "zed0_"

get_covariate_effects <- function(scale = "zed0_", dat = dat, X = X){
  
  ## Get JAGS model output
  zc <- readRDS(file = paste(path_to_model_rds, my_threshold, scale, this_model,
                             ".rds", sep = ""))
  
  ## Get covariate names
  df_jurisdiction_region <- dat %>% distinct(JURISDICTION, REGION)
  
  # Create sample area groups
  dat <- dat %>% mutate(group_j = as.integer(as.factor(REGION)))
  dat_groups <- dat %>% select(REGION, group_j) %>% distinct()
  
  # Remove intercept column
  X <- X[ , -1]
  
  ## Gather beta coeficients
  beta_df <- zc %>% gather_draws(beta[i])
  
  # Format names
  beta_names <- colnames(X)
  beta_names_df <- tibble(beta_name = beta_names, 
                          i = seq(1:length(beta_names)))
  
  # Join with beta names
  beta_df <- beta_df %>% 
    right_join(., beta_names_df, by = "i")
  
  beta_coefs_df <- beta_df %>%
    group_by(beta_name, i, .variable) %>% 
    median_qi(.width = c(0.95, 0.8)) %>% 
    ungroup() 

  beta_coefs <- beta_coefs_df %>% 
    distinct(beta_name, i, .value) %>% 
    arrange(i)
  
  # Reorder beta_df
  beta_df2 <- beta_df %>% 
    arrange(.chain, .iteration, .draw, i)
  
  effect_summary <- beta_df2 %>% 
    mutate(above_0 = ifelse(.value > 0, 1, 0), 
           below_0 = ifelse(.value < 0, 1, 0)) %>% 
    group_by(beta_name) %>% 
    summarise(above_0 = sum(above_0), 
              below_0 = sum(below_0)) %>% 
    mutate(n_iter = above_0 + below_0, 
           above_prob = above_0 / n_iter, 
           below_prob = below_0 / n_iter)
  
  return(effect_summary)

}

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w0.csv")
my_scale <- "zed0_"
# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
effects0 <- get_covariate_effects(scale = my_scale, dat = dat, X = X) %>% 
  mutate(scale = "Cross-basin")

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w1.csv")
my_scale <- "zed1_"
# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
effects1 <- get_covariate_effects(scale = my_scale, dat = dat, X = X) %>% 
  mutate(scale = "Basin")

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w2.csv")
my_scale <- "zed2_"
# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
effects2 <- get_covariate_effects(scale = my_scale, dat = dat, X = X) %>% 
  mutate(scale = "Region")

## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w3.csv")
my_scale <- "zed3_"
# Get model matrix 
source(paste(my_path, "models/model_matrices.R", sep = ""))
effects3 <- get_covariate_effects(scale = my_scale, dat = dat, X = X) %>% 
  mutate(scale = "Sub-region")

##### COMPILE #####

effects_df <- rbind(effects0, effects1, effects2, effects3) %>% 
  mutate(Direction = ifelse(above_0 > below_0, "Positive", "Negative"), 
         Probability = ifelse(Direction == "Positive", above_prob, below_prob))
effects_df

effects_df_clean <- effects_df %>% 
  select(beta_name, scale, Direction, Probability) %>% 
  rename(covariate = beta_name) %>% 
  mutate(Probability = round(Probability, digits = 3))

effects_df_clean <- effects_df_clean %>% 
  mutate(variable = factor(covariate))

## Rename variables
effects_df_clean$variable <- recode(effects_df_clean$variable, 
                       human_pop50_log_z = "Human population density", 
                       nAll_30yr_z = "Storms", 
                       wave_cv_z = "Wave energy (cv)", 
                       wave_mean_z = "Wave energy (mean)", 
                       npp_cv_z = "Primary productivity (cv)", 
                       npp_mean_z = "Primary productivity (mean)", 
                       kd490_cv_z = "Light attenuation (cv)", 
                       kd490_mean_z = "Light attenuation (mean)", 
                       sst_cv_z = "SST (cv)", 
                       sst_mean_z = "SST (mean)")

effects_df_clean

effects_df_clean_wide <- effects_df_clean %>% 
  spread(., key = scale, value = Probability) 

effects_df_clean_wide <- effects_df_clean_wide %>% 
  select(variable, Direction, `Cross-basin`, Basin, Region, `Sub-region`)

##### WRITE FILES #####
write.csv(effects_df_clean, 
          file = paste(path_to_output, 
                       my_threshold, this_model, "_", "beta_effect_probabilities_df", ".csv", sep = ""))

write.csv(effects_df_clean_wide, 
          file = paste(path_to_output, 
                       my_threshold, this_model, "_", "beta_effect_probabilities_wide_df", ".csv", sep = ""))

