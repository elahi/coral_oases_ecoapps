################################################################################
##' @title Compile MSEC predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-11-15
##' @log Add a log here
################################################################################

##' These geospatial data are from:
##' Yeager et al. 2017
##' https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/ecy.1884
##' Download the zip file, and in terminal, use the command:
##' jar xvf ecy1884-sup-0002-datas1.zip
##' (unzipping by double-clicking throws an error - 'directory not found')
##' 
##' The following data were extracted from the SHINY app:
##' wave_mean, wave_sd, wave_interann_sd, 
##' npp_mean, npp_min, npp_max, npp_sd, npp_interann_sd, dist_market
##' 
##' The following data were extracted using the nc files
##' humanpop50
##' landarea50
##' reefarea200
##' 

#### LOAD PACKAGES, DATA ####

# Tidyverse
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

source("R/longitude_conversion_functions.R")

# NOAA lat-longs
noaa_ll <- read_csv("data_output/noaa_ll_date.csv") %>% 
  select(-c(X1, long_bin_0.5:lat_bin_2.5))

# Convert noaa longitude to nc longitude
noaa_ll <- noaa_ll %>% 
  mutate(SI_LONG_360 = convert_180_to_360(SI_LONG))

# Create new region that splits up PRIAs
noaa_ll <- noaa_ll %>% 
  mutate(REGION_NEW = ifelse(REGION == "PRIAs", REGION_SUB, REGION))
noaa_ll

# Shiny data
shiny_dat <- read_csv("data_output/noaa_msec_out_shiny_181115.csv")
shiny_dat

##### LOAD NC DATA #####

##' These are the data I processed from the NC files

# Human population
human_dat <- read_csv("data_output/noaa_msec_humanpop_50km.csv")

# Land area
land_dat <- read_csv("data_output/noaa_msec_landarea_50km.csv")

# Reef area
reef_dat <- read_csv("data_output/noaa_msec_reefarea_200km.csv")

## Below, these are redundant to shiny (useful as sanity check)

# Distance to market
market_dat <- read_csv("data_output/noaa_msec_distmarket.csv")

# Wave mean
wave_dat <- read_csv("data_output/noaa_msec_wave_mean.csv")

##### COMPILE DATA #####

noaa_ll
shiny_dat

# Combine noaa metadata with shiny output
noaa_ll_msec <- cbind(noaa_ll, shiny_dat) %>% 
  select(-c(long, lat)) %>% tbl_df()
noaa_ll_msec

# Now add nc data
noaa_ll_msec <- noaa_ll_msec %>% 
  mutate(human_pop50 = human_dat$z,
         land_area50 = land_dat$z, 
         reef_area200 = reef_dat$z, 
         dist_market_nc = market_dat$z, 
         wave_mean_nc = wave_dat$z)

noaa_ll_msec
write.csv(noaa_ll_msec, "workspace/noaa_ll_msec.csv")

