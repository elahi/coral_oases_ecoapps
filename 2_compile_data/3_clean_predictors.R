################################################################################
##' @title Choose predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-26
################################################################################

##### PACKAGES, DATA #####
library(tidyverse)

## Predictors
dat <- read_csv("workspace/predictor_df.csv") 
names(dat)

##### SUBSET DATA #####

dat_meta <- dat %>% select(ll_id:REGION_NEW, long_bin_2.5, lat_bin_2.5)

# Remove irrelevant columns
statDat <- dat

statDat <- statDat %>% 
  select(-c(X1:REGION_NEW, 
            long_bin_2.5, lat_bin_2.5, 
            wave_wind_fetch, wave_ww3_res, 
            wave_mean_nc, dist_market_nc,
            npp_flag, 
            sst_var, kd490_var))
names(statDat)

# Remove covariates that are minimally informative (storms in the last 5 years)
statDat <- statDat %>% 
  select(-c(n3_5yr, n4_5yr, n5_5yr, 
            n3, n4, n5, 
            n3_30yr, n4_30yr, n5_30yr, 
            nAll, nAll_5yr)) 

# Remove covariates deemed less relevant by pwg
statDat <- statDat %>% 
  select(-c(reef_area200, dist_market)) 
names(statDat)

# Remove covariates that have strong variance inflation factors 
statDat <- statDat %>% 
  select(-c(wave_interann_sd, 
            npp_interann_sd, npp_max, npp_min))

# Reorder column names
statDat <- statDat %>% select(order(colnames(statDat)))
names(statDat)

# Combine with metadata
dat_new <- cbind(dat_meta, statDat)
names(dat_new)

write.csv(dat_new, "workspace/predictor_df_clean.csv")

