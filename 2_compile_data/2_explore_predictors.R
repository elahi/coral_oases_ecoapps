################################################################################
##' @title Exploratory data analysis, predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-26
################################################################################

##### PACKAGES, DATA #####
source("R/HighstatLibV6.R")
source("R/scale_gelman.R")
source("R/get_base_map.R")

library(here)
library(tidyverse)
library(viridis)
library(ggcorrplot)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

## Predictors
dat <- read_csv("workspace/predictor_df.csv") 
names(dat)
path_to_figs <- "2_compile_data/predictor_figs/"

##### SUBSET DATA #####

statDat <- dat

# Remove irrelevant columns
names(statDat)
statDat <- statDat %>% 
  select(-c(X1, ll_id, REGION:REGION_NEW, 
            long_bin_2.5, lat_bin_2.5, 
            wave_wind_fetch, wave_ww3_res, 
            wave_mean_nc, dist_market_nc,
            npp_flag))
names(statDat)

# Get complete cases
summary(statDat)
statDat <- statDat[complete.cases(statDat), ]

##### VISUALISE DISTRIBUTIONS OF PREDICTORS #####

names(statDat)
sdl <- statDat %>% 
  gather(key = covariate, value = value, wave_mean:kd490_cv)

sdl %>% 
  ggplot(aes(value)) + 
  geom_histogram() + 
  facet_wrap(~ covariate, scales = "free")

# Remove covariates that are minimally informative (storms in the last 5 years)
statDat <- statDat %>% 
  select(-c(n3_5yr, n4_5yr, n5_5yr)) 
names(statDat)

sdl <- statDat %>% 
  gather(key = covariate, value = value, wave_mean:kd490_cv)

sdl %>% 
  ggplot(aes(value, fill = OCEAN)) + 
  geom_histogram(color = 'black') + 
  facet_wrap(~ covariate, scales = "free") + 
  scale_fill_viridis_d() + 
  theme(legend.position = "bottom")

ggsave(paste(path_to_figs, "histogram_variables.pdf", sep = ""), 
       height = 10, width = 14)

##### CHECK FOR COVARYING PREDICTORS #####
summary(statDat)
names(statDat)

# Separate by ocean
statDat2 <- statDat %>% 
  filter(OCEAN == "CARIB") %>% 
  select(-(OCEAN))

statDat2 <- statDat %>% 
  filter(OCEAN == "PACIFIC") %>% 
  select(-(OCEAN))

statDat2 <- statDat %>% 
  select(-(OCEAN))

# Variance inflation factors
statDat2 <- statDat2 %>% select(order(colnames(statDat2)))
names(statDat2)

statDat2 %>% 
  select(human_pop50_log, 
         kd490_mean, 
         kd490_cv, 
         nAll_30yr, 
         npp_mean, 
         npp_cv, 
         sst_mean, 
         sst_var, 
         wave_mean, 
         wave_cv) %>% corvif()

statDat %>% 
  ggplot(aes(sst_mean, sst_var)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ OCEAN) + 
  geom_smooth(method = "lm", se = TRUE)

statDat %>% 
  ggplot(aes(sst_mean, sst_cv)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ OCEAN) + 
  geom_smooth(method = "lm", se = TRUE)

# Plot correlation matrix

cor_mat <- statDat2 %>% 
  select(human_pop50_log, 
         kd490_mean, 
         kd490_cv, 
         nAll_30yr, 
         npp_mean,
         npp_cv,
         sst_mean, 
         sst_cv, 
         wave_mean, 
         wave_cv) %>% cor()

ggcorrplot(cor_mat, hc.order = FALSE, type = "lower",
           lab = TRUE, show.legend = T, 
           lab_size = 3, legend.title = "", 
           tl.cex = 8) 
