################################################################################
##' @title Null vs model comparison of grid cell selection; zed0
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-09-10
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
my_threshold <- "z2_"
this_model <- "occu_hier_binom"

my_scale <- "zed0_"

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

source("R/inverse_logit.R")

##### DATA #####
## Get raw data, and model fit
dat <- read.csv("workspace/grid_df_w0_psi.csv")

## Get JAGS model output
zc <- readRDS(file = paste(path_to_ignored_model_rds, 
                           my_threshold, my_scale, this_model,
                           ".rds", sep = ""))

str(zc)

#### PSI PREDICTIONS ####
psi_df <- zc %>% 
  gather_draws(psi[i])

psi_df
summary(psi_df$.draw) # 24000 total draws from the posterior
mc_draws <- unique(psi_df$.draw)

# Select random draws
set.seed(108)
n_draws <- 5000
mc_draws_sub <- sample(mc_draws, size = n_draws, replace = FALSE, prob = NULL)
mc_draws_sub <- sort(mc_draws_sub, decreasing = FALSE)
tail(mc_draws_sub)
length(unique(mc_draws_sub))

# Subset dataframe
psi_df_sub <- psi_df[psi_df$.draw %in% mc_draws_sub, ]
psi_df_sub

##### GET MODEL EXPECTATIONS #####
oasis_present_vector <- dat$oasis_present
n_oases <- sum(dat$oasis_present)
n_oases

## Get vector of model predictions, matching the length of random draws
n_cells_vector <- rep(NA, length = n_draws)
n_cells_vector

start_time <- Sys.time()
start_time

for(i in 1:n_draws){
  # Select an mcmc draw from the random subset
  draw_i <- mc_draws_sub[i]
  # Get the posterior probabilities for grid cell psi for one chain
  psi_df_sub_i <- psi_df_sub %>% 
    filter(.draw == draw_i)
  # Make df with observations and probabilities for each cell
  oasis_prob_df <- tibble(oasis_present = dat$oasis_present, 
                          prob_vector = psi_df_sub_i$.value)
  
  n_cells_model <- oasis_prob_df %>%  
    arrange(desc(prob_vector)) %>%  # arrange the df from highest to lowest psi
    slice(1:n_oases) %>%            # remove the top n rows with highest psi 
    summarise(sum(oasis_present))   # sum to get total oases in the highest predicted cells
  
  # Save the model predicted number of oasis cells
  n_cells_vector[i] <- as.numeric(n_cells_model) 
}
end_time <- Sys.time()
end_time - start_time

# Rename
n_cells_model <- n_cells_vector
head(n_cells_model)

##### COMPARE MODEL WITH NULL EXPECTATIONS #####
## Get vector of null predictions
n_cells_vector <- rep(NA, length = n_draws)

for(i in 1:n_draws){
  n_cells_null <- oasis_prob_df %>% 
    sample_n(size = n_oases, replace = FALSE) %>% 
    summarise(sum(oasis_present))

  n_cells_vector[i] <- as.numeric(n_cells_null)
}

n_cells_null <- n_cells_vector

##### MAKE DATAFRAME OF MODEL AND NULL EXPECTATIONS #####

cells_df <- tibble(n_cells_model, n_cells_null,
                   n_cells_oasis = n_oases) %>% 
  mutate(model_null_ratio = n_cells_model / n_cells_null, 
         lrr = log(model_null_ratio), 
         null_percentage = n_cells_null / n_cells_oasis * 100, 
         model_percentage = n_cells_model / n_cells_oasis * 100)

summary(cells_df)
cells_df %>% 
  ggplot() + 
  geom_density(aes(null_percentage)) + 
  geom_density(aes(model_percentage), col = "red")

##### WRITE FILES #####
write.csv(cells_df, 
          file = paste(path_to_output, 
                       my_threshold, my_scale, this_model, "_", "null_model_comparison_df", ".csv", sep = ""))
