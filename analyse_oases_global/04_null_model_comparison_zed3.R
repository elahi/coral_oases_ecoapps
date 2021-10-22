################################################################################
##' @title Null vs model comparison of grid cell selection; zed3
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-09-10
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
my_threshold <- "z2_"
this_model <- "occu_hier_binom"

my_scale <- "zed3_"

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
dat <- read.csv("workspace/grid_df_w3_psi.csv")

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

## Function
get_n_cells_model <- function(oasis_prob_df){
  n_oases_i <- sum(oasis_prob_df$oasis_present)
  
  n_cells_df <- oasis_prob_df %>% 
    arrange(desc(prob_vector)) %>% 
    slice(1:n_oases_i) %>% 
    summarise(n_cells = sum(oasis_present))
  
  return(n_cells_df)
}

start_time <- Sys.time()
start_time

for(i in 1:n_draws){
  # Select an mcmc draw from the random subset
  draw_i <- mc_draws_sub[i]
  # Get the posterior probabilities for grid cell psi for one chain
  psi_df_sub_i <- psi_df_sub %>% 
    filter(.draw == draw_i)
  # Make df with observations and probabilities for each cell
  oasis_prob_df <- tibble(OCEAN = dat$OCEAN, 
                          JURISDICTION = dat$JURISDICTION, 
                          SUBJURISDICTION = dat$REGION, 
                          oasis_present = dat$oasis_present, 
                          prob_vector = psi_df_sub_i$.value)
  
  n_cells_model <- oasis_prob_df %>%  
    group_by(SUBJURISDICTION) %>% 
    do(get_n_cells_model(.)) %>% 
    ungroup() %>% 
    summarise(n_cells = sum(n_cells)) %>% 
    as.numeric()
  
  # Save the model predicted number of oasis cells
  n_cells_vector[i] <- as.numeric(n_cells_model) 
}

end_time <- Sys.time()
end_time - start_time

# Rename
n_cells_model <- n_cells_vector
head(n_cells_model)

##### GET NULL EXPECTATIONS #####

## In this null model, I will select oases in each subjurisdiction separately
# Determine # of oases in each subjurisdiction (REGION)
df_oases <- dat %>% count(OCEAN, JURISDICTION, REGION, oasis_present) %>% 
  spread(key = oasis_present, value = n, fill = 0) %>% 
  mutate(oasis_cells = `1`, 
         total_cells =  `1` + `0`,  
         prop_oasis = oasis_cells / total_cells) %>% 
  select(-c(`0`, `1`))
df_oases

# Add these values to the dataframe
dat2 <- df_oases %>%
  left_join(dat, ., by = c("OCEAN", "JURISDICTION", "REGION"))

get_n_cells_null <- function(df_i){
  n_oases_i <- unique(df_i$oasis_cells)
  df_null <- df_i %>% sample_n(size = n_oases_i) 
  n_cells_null <- df_i %>% 
    sample_n(size = n_oases_i) %>% 
    summarise(n_cells_null = sum(oasis_present))
  return(n_cells_null)
}

get_n_cells_subjurisdiction <- function(dat){
  dat %>% 
    group_by(REGION) %>% 
    do(get_n_cells_null(.)) %>% 
    ungroup() %>%
    summarise(n_cells_null = sum(n_cells_null)) %>% 
    as.numeric()
}

start_time <- Sys.time()
start_time
n_cells_null <- replicate(n = n_draws, get_n_cells_subjurisdiction(dat2))
end_time <- Sys.time()
end_time - start_time

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
