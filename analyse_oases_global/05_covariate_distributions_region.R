################################################################################
##' @title Plot covariate distributions by region
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-27
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
library(tidyverse)
library(forcats)
library(rjags)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

##### PLOTTING FUNCTION #####


plot_covariates <- function(d, my_scale = "zed0_"){
  
  d <- d %>% 
    select(OCEAN:long_bin_2.5, scale, 
           human_pop50_log_z, nAll_30yr_z, 
           wave_cv_z, wave_mean_z, 
           npp_cv_z, npp_mean_z, 
           kd490_cv_z, kd490_mean_z, 
           sst_cv_z, sst_mean_z) %>% 
    as_tibble()
  
  sdl <- d %>% 
    gather(key = covariate, value = value,
           human_pop50_log_z:sst_mean_z)
  
  sdl <- sdl %>% 
    mutate(variable = factor(covariate))
  
  ## Reorder variables
  sdl$variable <- fct_relevel(sdl$variable, "human_pop50_log_z", "nAll_30yr_z", 
                              "wave_cv_z", "wave_mean_z", "npp_cv_z", "npp_mean_z", 
                              "kd490_cv_z", "kd490_mean_z", "sst_cv_z", "sst_mean_z")
  
  ## Rename variables
  sdl$variable <- recode(sdl$variable, 
                         human_pop50_log_z = "Human population density", 
                         nAll_30yr_z = "Storms", 
                         wave_cv_z = "Wave exposure (cv)", 
                         wave_mean_z = "Wave exposure (mean)", 
                         npp_cv_z = "Primary productivity (cv)", 
                         npp_mean_z = "Primary productivity (mean)", 
                         kd490_cv_z = "Light attenuation (cv)", 
                         kd490_mean_z = "Light attenuation (mean)", 
                         sst_cv_z = "SST (cv)", 
                         sst_mean_z = "SST (mean)")
  
  ## Reverse variables
  sdl$variable <- fct_rev(sdl$variable)
  
  ## Relevel jurisdiction
  sdl <- sdl %>% mutate(Jurisdiction = factor(JURISDICTION))
  levels(sdl$Jurisdiction)
  
  ## Reorder jurisdictions
  sdl$Jurisdiction <- fct_relevel(sdl$Jurisdiction, 
                                  "USVI", "PRICO", "FLORIDA",
                                  "MARIAN", "NWHI", "MHI", "SAMOA")
  
  ## Rename variables
  levels(sdl$Jurisdiction) <- c("US Virgin Islands", "Puerto Rico", 
                                "Florida", "Mariana Islands", 
                                "Northwest Hawaiian Islands", 
                                "Main Hawaiian Islands", 
                                "American Samoa")
  
  ## Reverse
  sdl$Jurisdiction <- fct_rev(sdl$Jurisdiction)
  
  sdl %>% 
    ggplot(aes(Jurisdiction, value, fill = Jurisdiction)) + 
    geom_hline(yintercept = 0, linetype = "solid", color = "gray") + 
    geom_boxplot(outlier.shape = 1, outlier.size = 0.5) + 
    facet_wrap(~ variable, ncol = 2, scales = "free_y") + 
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(x = "Region", y = "Standardized value") +  
    theme(axis.text.x = element_text(angle = 35, vjust = 0.99, hjust = 0.99)) +
    scale_fill_brewer(type = "div", palette = "RdYlBu", direction = 1)
  
  ggsave(paste(path_to_figs, my_scale, "covariate_distributions_region.pdf", sep = ""), 
         height = 8, width = 6)
  
}

##### SAVE PLOTS #####

## Zed 0
d <- read.csv("workspace/grid_df_w0.csv") %>% 
  mutate(scale = "Cross-basin")
plot_covariates(d, my_scale = "zed0_")

## Zed 1
d <- read.csv("workspace/grid_df_w1.csv") %>% 
  mutate(scale = "Basin")
plot_covariates(d, my_scale = "zed1_")

## Zed 2
d <- read.csv("workspace/grid_df_w2.csv") %>% 
  mutate(scale = "Region")
plot_covariates(d, my_scale = "zed2_")

## Zed 3
d <- read.csv("workspace/grid_df_w3.csv") %>% 
  mutate(scale = "Sub-region")
plot_covariates(d, my_scale = "zed3_")


