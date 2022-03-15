################################################################################
##' @title Process MSEC predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-11-12
##' @log Add a log here
################################################################################

##' These geospatial data are from:
##' Yeager et al. 2017
##' https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/ecy.1884
##' Download the zip file, and in terminal, use the command:
##' jar xvf ecy1884-sup-0002-datas1.zip
##' (unzipping by double-clicking throws an error - 'directory not found')

#### LOAD PACKAGES, DATA ####

# Tidyverse
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Spatial
library(ncdf4)
library(fuzzyjoin)

source("R/get_base_map.R")
source("R/longitude_conversion_functions.R")
source("2_process_msec/get_msec_z.R")

# NOAA lat-longs
noaa_ll <- read_csv("data_output/noaa_ll_date.csv") %>% 
  select(-c(X1, long_bin_0.5:lat_bin_2.5))

# Convert noaa longitude to nc longitude
noaa_ll <- noaa_ll %>% 
  mutate(SI_LONG_360 = convert_180_to_360(SI_LONG))

# Create new region that splits up PRIAs
noaa_ll <- noaa_ll %>% 
  mutate(REGION_NEW = ifelse(REGION == "PRIAs", REGION_SUB, REGION))

# Enter your path to the netcdf data
path_to_data <- "/Volumes/archive/pwg_data/yeager_msec_nc/"

# Retrieve a list of nc files in my data folder:
flist <- list.files(path = path_to_data, pattern = "^.*\\.(nc|NC|Nc|Nc)$")

#### MSEC DATA - DISTANCE TO MARKET ####

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
flist
j = 1
msec_var <- gsub("\\..*", "", flist[j])
msec_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2, nc_dim_logical = FALSE, nc_dim = 1)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

#### MSEC DATA - HUMAN POPULATION WITHIN 50 KM ####

## Multiple years available: 1990, 1995, 2000, 2005, 2010, 2015, 2020
## We chose 2015 because this reflected the latest period of coral cover data in our dataset

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
flist
j = 3
msec_var <- gsub("\\..*", "", flist[j])
msec_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
my_nc_dim = 6 # This represents 2015
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = TRUE, nc_dim = my_nc_dim)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = TRUE, nc_dim = my_nc_dim)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

#### MSEC DATA - LAND AREA WITHIN 50 KM ####

## Land area was available within 15 or 50km
## We chose 50 km to maximize the potential impact

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
flist
j = 5
msec_var <- gsub("\\..*", "", flist[j])
msec_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
my_nc_dim = NA
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

#### MSEC DATA - REEF AREA WITHIN 200 KM ####

## Reef area was available within 15 or 200km
## We chose 200 km to maximize larval pool

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
flist
j = 13
msec_var <- gsub("\\..*", "", flist[j])
msec_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
my_nc_dim = NA
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

#### MSEC DATA - WAVE INTRA-ANNUAL MEAN ####

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
flist
j = 14
msec_var <- gsub("\\..*", "", flist[j])
msec_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
my_nc_dim = NA
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

#### MSEC DATA - WAVE INTRA-ANNUAL SD ####

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
flist
j = 16
msec_var <- gsub("\\..*", "", flist[j])
msec_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
my_nc_dim = NA
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2, 
                          nc_dim_logical = FALSE, nc_dim = my_nc_dim)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

##' Note that the wave_sd is corrupt and the wave_sd_inter files does not exist when I unzip the file (see)


##### TESTING #######

flist
j = 16
df = noaa_ll_i
p = 2
nc_dim_logical = FALSE
nc_dim = 1

j = 3 # Human population
df = noaa_ll_i
nc_dim_logical = TRUE
nc_dim = 6

lat_range <- range(df$SI_LATI)
long_range <- range(df$SI_LONG_360)

ncFile <- nc_open(paste0(path_to_data, flist[j]))

# Retrieve the latitude and longitude values.
nc_lon <- ncvar_get(ncFile, attributes(ncFile$dim)$names[1])
nc_lat <- ncvar_get(ncFile, attributes(ncFile$dim)$names[2])

nc_lon_length <- length(nc_lon)
nc_lat_length <- length(nc_lat)

nc_lon_bin_size <- 360 / nc_lon_length
nc_lat_bin_size <- 180 / nc_lat_length

# Now extract the relevant lat-longs
# Add a buffer p x the bin size
LonIdx <- which(ncFile$dim$lon$vals > long_range[1] - p*nc_lon_bin_size & ncFile$dim$lon$vals < long_range[2] + p*nc_lon_bin_size)

LatIdx <- which(ncFile$dim$lat$vals > lat_range[1] - p*nc_lat_bin_size & ncFile$dim$lat$vals < lat_range[2] + p*nc_lat_bin_size)

## If the ncFile has one dimension (e.g., year) only:
if(nc_dim_logical == FALSE){
  MyVariable <- ncvar_get(ncFile, attributes(ncFile$var)$names[1])[LonIdx, LatIdx]
}

## If the ncFile has more than one year, I will need to subset the 3rd dimension of the array
if(nc_dim_logical == TRUE){
  MyVariable <- ncvar_get(ncFile, attributes(ncFile$var)$names[1])[LonIdx, LatIdx, nc_dim]
}

lon <- ncFile$dim$lon$val[LonIdx] 
lat <- ncFile$dim$lat$val[LatIdx]
nc_close(ncFile)

nc_df <- cbind(rep(lat, each = length(lon)), rep(lon, length(lat)), 
               c(MyVariable)) %>% tbl_df() %>% 
  rename(SI_LATI = V1, SI_LONG_360 = V2, z = V3) 

# Change to -180 to 180
nc_df$SI_LONG <- convert_360_to_180(nc_df$SI_LONG_360)

# Remove NAs (for safety, in case the join occurs on a cell without satellite data)
nc_df <- nc_df %>% filter(!is.na(z))

# Fuzzy join
df <- df %>% 
  select(-SI_LONG_360) %>% # remove third col
  geo_left_join(., nc_df, unit = 'km', distance_col = "dist_km", max_dist = 4)

# Note that there are now multiple rows (dist_km) for each ll_id
# Group by ll_id, arrange by dist_km (descending), then slice the first row
# Should retrieve the original nrows

df <- df %>% 
  group_by(ll_id) %>% 
  arrange(ll_id, dist_km) %>% 
  slice(1) %>% ungroup()

return(df)

mean_1 <- mean(df$z)
mean_6 <- mean(df$z)
mean_7 <- mean(df$z)
