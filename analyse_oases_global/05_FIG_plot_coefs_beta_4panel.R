################################################################################
##' @title Plot beta coefs
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-08-24
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
this_model <- "occu_hier_binom"

##### PACKAGES, DATA #####
library(tidyverse)
library(tidybayes)
library(viridis)
library(forcats)
library(rjags)
source("R/inverse_logit.R")

theme_set(theme_bw(base_size = 14) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

##### GET COVARIATE NAMES #####

# Prep data
dat <- read.csv("workspace/grid_df_w1.csv")

df_jurisdiction_region <- dat %>% 
  distinct(JURISDICTION, REGION)

# Create sample area groups
dat <- dat %>% mutate(group_j = as.integer(as.factor(REGION)))
dat_groups <- dat %>% 
  select(REGION, group_j) %>% distinct()

##### COMPILE BETA COEFS #####

my_files <- list.files(path = path_to_model_output, pattern = "beta_coefs")

## Get model fits
fit <- read.csv(file = paste(path_to_model_output, my_files[1], sep = ""))
fit0_coefs <- fit %>% mutate(scale = "Cross-basin")

fit <- read.csv(file = paste(path_to_model_output, my_files[2], sep = ""))
fit1_coefs <- fit %>% mutate(scale = "Basin")

fit <- read.csv(file = paste(path_to_model_output, my_files[3], sep = ""))
fit2_coefs <- fit %>% mutate(scale = "Region")

fit <- read.csv(file = paste(path_to_model_output, my_files[4], sep = ""))
fit3_coefs <- fit %>% mutate(scale = "Sub-region")

fit_coefs <- rbind(fit0_coefs, fit1_coefs, fit2_coefs, fit3_coefs)

## Reorder jurisdiction
unique(fit_coefs$scale)
fit_coefs <- fit_coefs %>%
  mutate(scale = factor(scale, 
                        levels = c("Cross-basin", "Basin", "Region", "Sub-region")))

## Relevel variables
fit_coefs <- fit_coefs %>% 
  mutate(variable = factor(beta_name))
levels(fit_coefs$variable)

## Reorder variables
fit_coefs$variable <- fct_relevel(fit_coefs$variable, "human_pop50_log_z", "nAll_30yr_z", 
                                  "wave_cv_z", "wave_mean_z", "npp_cv_z", "npp_mean_z", 
                                  "kd490_cv_z", "kd490_mean_z", "sst_cv_z", "sst_mean_z")

levels(fit_coefs$variable)

## Rename variables
fit_coefs$variable <- recode(fit_coefs$variable, 
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

## Color points according to probability
fit_coefs <- fit_coefs %>% 
  mutate(point_color = ifelse(.lower < 0 & .upper > 0 & .width == 0.95, "prob_a",
                              ifelse(.lower < 0 & .upper > 0 & .width == 0.80, "prob_b", "prob_c")))


## Colors
viridis::viridis(n = 2)
my_pal <- c("#FDE725FF", "#FDE725FF", "#440154FF")

fit_coefs %>%
  ggplot(aes(y = variable, x = .value)) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.2) + 
  geom_pointintervalh(aes(color = point_color), fatten_point = 1.2) + 
  facet_wrap(~ scale, ncol = 4) + 
  theme(legend.position = "none") + 
  labs(x = "Standardized coefficient", y = "") + 
  scale_color_manual(values = c("darkgray", "darkgray", "black")) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste(path_to_figs, "FIG_", this_model, "plot_coefs_beta_4panel.pdf", 
             sep = ""), height = 3.5, width = 7)

