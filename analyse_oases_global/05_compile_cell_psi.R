################################################################################
##' @title Extract psi from occupancy models
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
source("R/renaming_functions.R")

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

source("R/inverse_logit.R")

##### COMPILE PSI MEDIANS #####

my_files <- list.files(path = path_to_model_output, pattern = "psi_coefs_prob_df")
my_files

## Get files
fit <- read.csv(file = paste(path_to_model_output, my_files[1], sep = ""))
fit0 <- fit %>% mutate(scale = "Cross-basin") %>% 
  filter(.width == 0.95) %>% 
  select(i, cell_2.5, .value, scale) %>%
  rename(psi_median = .value) %>% 
  tibble()
dat <- read.csv("workspace/grid_df_w0.csv")
dat2 <- dat %>% mutate(y_pred_prob = fit0$psi_median)
write.csv(dat2, paste(path_to_workspace,"grid_df_w0_psi.csv", sep = ""))
dat2 %>% 
  ggplot(aes(y_pred_prob, REGION, fill = JURISDICTION)) + 
  geom_boxplot() + 
  facet_wrap(~ JURISDICTION, scales = "free_y")

fit <- read.csv(file = paste(path_to_model_output, my_files[2], sep = ""))
fit1 <- fit %>% mutate(scale = "Basin") %>% 
  filter(.width == 0.95) %>% 
  select(i, cell_2.5, .value, scale) %>%
  rename(psi_median = .value) %>% 
  tibble()
dat <- read.csv("workspace/grid_df_w1.csv")
dat2 <- dat %>% mutate(y_pred_prob = fit1$psi_median)
write.csv(dat2, paste(path_to_workspace,"grid_df_w1_psi.csv", sep = ""))
dat2 %>% 
  ggplot(aes(y_pred_prob, REGION, fill = JURISDICTION)) + 
  geom_boxplot() + 
  facet_wrap(~ JURISDICTION, scales = "free_y")

fit <- read.csv(file = paste(path_to_model_output, my_files[3], sep = ""))
fit2 <- fit %>% mutate(scale = "Region") %>% 
  filter(.width == 0.95) %>% 
  select(i, cell_2.5, .value, scale) %>%
  rename(psi_median = .value) %>% 
  tibble()
dat <- read.csv("workspace/grid_df_w2.csv")
dat2 <- dat %>% mutate(y_pred_prob = fit2$psi_median)
write.csv(dat2, paste(path_to_workspace,"grid_df_w2_psi.csv", sep = ""))
dat2 %>% 
  ggplot(aes(y_pred_prob, REGION, fill = JURISDICTION)) + 
  geom_boxplot() + 
  facet_wrap(~ JURISDICTION, scales = "free_y")

fit <- read.csv(file = paste(path_to_model_output, my_files[4], sep = ""))
fit3 <- fit %>% mutate(scale = "Sub-region") %>% 
  filter(.width == 0.95) %>% 
  select(i, cell_2.5, .value, scale) %>%
  rename(psi_median = .value) %>% 
  tibble()
dat <- read.csv("workspace/grid_df_w3.csv")
dat2 <- dat %>% mutate(y_pred_prob = fit3$psi_median)
write.csv(dat2, paste(path_to_workspace,"grid_df_w3_psi.csv", sep = ""))
dat2 %>% 
  ggplot(aes(y_pred_prob, REGION, fill = JURISDICTION)) + 
  geom_boxplot() + 
  facet_wrap(~ JURISDICTION, scales = "free_y")

fit_df <- rbind(fit0, fit1, fit2, fit3) 

## Link to lat-longs 
dat <- read.csv("workspace/grid_df_w0.csv")
dat2 <- rename_oasis_data(dat)
names(dat2)
dat3 <- dat2 %>% 
  select(Ocean, Region, Subregion, long_bin_2.5, lat_bin_2.5, cell_2.5) %>% 
  mutate(i = seq(1:nrow(dat2)))
fit_df2 <- left_join(fit_df, dat3, by = c("i", "cell_2.5"))

## Save predictions
write.csv(fit_df2, paste(path_to_workspace, my_threshold, this_model, "_cell_psi.csv", sep = ""))
