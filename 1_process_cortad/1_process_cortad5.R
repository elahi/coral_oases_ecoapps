################################################################################
##' @title Process raw cortad5 data
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-01-11
##' @log Add a log here
##' 2020-04-03: Adding stdev download
################################################################################

##' These geospatial data are from:

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
source("1_process_msec/get_msec_z.R")
source("1_process_cortad/get_cortad_z.R")

# NOAA lat-longs
noaa_ll <- read_csv("data_output/noaa_ll_date.csv") %>% 
  select(-c(X1, long_bin_0.5:lat_bin_2.5))
noaa_ll

# Convert noaa longitude to nc longitude
noaa_ll <- noaa_ll %>% 
  mutate(SI_LONG_360 = convert_180_to_360(SI_LONG))

# Create new region that splits up PRIAs
noaa_ll <- noaa_ll %>% 
  mutate(REGION_NEW = ifelse(REGION == "PRIAs", REGION_SUB, REGION))

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Enter your path to the netcdf data
path_to_data <- "ignore_folder/cortadv5/"

# Retrieve a list of nc files in my data folder:
flist <- list.files(path = path_to_data, pattern = "^.*\\.(nc|NC|Nc|Nc)$")

j = 1

##### TESTING #####

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

## variables
df = noaa_ll_i

p = 2
nc_dim_logical <- FALSE
nc_dim = 1

lat_range <- range(df$SI_LATI)
long_range <- range(df$SI_LONG_360)
long_range <- range(df$SI_LONG)

ncFile <- nc_open("ignore_folder/cortadv5/cortadv5_FilledSST.nc")

# Save the print(nc) dump to a text file 
{
  sink(paste0("ignore_folder/cortadv5/cortad_print.txt"))
  print(ncFile)
  sink()
}

# Get a list of the nc variable names.
nc_var_names <- attributes(ncFile$var)$names
nc_var_names

# FilledSST is hard to use, b/c too big probably
# Test using filled SSTmean
ncatt_get(ncFile, attributes(ncFile$var)$names[12])

# Retrieve the attributes
nc_atts <- ncatt_get(ncFile, 0)

# List all the attributes 
names(nc_atts)

# Retrieve the latitude and longitude values.
attributes(ncFile$dim)$names

nc_lat <- ncvar_get(ncFile, attributes(ncFile$dim)$names[2])
nc_lon <- ncvar_get(ncFile, attributes(ncFile$dim)$names[3])

# This is equivalent
nc_lon <- ncvar_get(ncFile, "lon")
nc_lat <- ncvar_get(ncFile, "lat")

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
  MyVariable <- ncvar_get(ncFile, attributes(ncFile$var)$names[12])[LonIdx, LatIdx]
}

## If the ncFile has more than one year, I will need to subset the 3rd dimension of the array
if(nc_dim_logical == TRUE){
  MyVariable <- ncvar_get(ncFile, attributes(ncFile$var)$names[12])[LonIdx, LatIdx, nc_dim]
}

lon <- ncFile$dim$lon$val[LonIdx] 
lat <- ncFile$dim$lat$val[LatIdx]
nc_close(ncFile)

nc_df <- cbind(rep(lat, each = length(lon)), rep(lon, length(lat)), 
               c(MyVariable)) %>% as_tibble() %>% 
  rename(SI_LATI = V1, SI_LONG = V2, z = V3) 
nc_df
summary(nc_df)

# Remove NAs (for safety, in case the join occurs on a cell without satellite data)
nc_df <- nc_df %>% filter(!is.na(z))
summary(nc_df)

ggplot(nc_df, aes(SI_LONG, SI_LATI)) + 
  geom_raster(aes(fill = z)) + 
  scale_fill_gradient(low = "white", high = "steelblue")

# Fuzzy join
df <- df %>% 
  geo_left_join(., nc_df, unit = 'km', distance_col = "dist_km", max_dist = 4)

# Note that there are now multiple rows (dist_km) for each ll_id
# Group by ll_id, arrange by dist_km (descending), then slice the first row
# Should retrieve the original nrows

df <- df %>% 
  group_by(ll_id) %>% 
  arrange(ll_id, dist_km) %>% 
  slice(1) %>% ungroup()

df

##### SSTmean #####

## CHANGE var_number FOR EACH CORTAD VARIABLE ##
# File #
flist
j = 1 # there is only one nc file
nc_var_names
var_number = 12 # choosing FilledSSTmean
cortad_var <- nc_var_names[var_number]
cortad_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
noaa_ll_new <- get_cortad_z(df = noaa_ll_i, j = j, p = 2, 
                            nc_dim_logical = FALSE, nc_dim = 1, var_number = var_number)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_cortad_z(df = noaa_ll_i, j = j, p = 2, 
                            nc_dim_logical = FALSE, nc_dim = 1, var_number = var_number)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/cortad_", cortad_var, ".csv", sep = ""))


##### SSTstdev #####

## CHANGE var_number FOR EACH CORTAD VARIABLE ##
# File #
flist
j = 1 # there is only one nc file
nc_var_names
var_number <- 11 # choosing FilledSSTmean
cortad_var <- nc_var_names[var_number]
cortad_var

# Get unique groups
i_unique <- unique(noaa_ll$REGION_NEW)
i = 1 # PRIAs (8) causes the get_msec_z function to crash R when using REGION, so I created REGION_NEW

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
noaa_ll_new <- get_cortad_z(df = noaa_ll_i, j = j, p = 2, 
                            nc_dim_logical = FALSE, nc_dim = 1, var_number = var_number)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_cortad_z(df = noaa_ll_i, j = j, p = 2, 
                            nc_dim_logical = FALSE, nc_dim = 1, var_number = var_number)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

summary(noaa_ll_new)
write.csv(noaa_ll_new, file = paste("data_output/cortad_", cortad_var, ".csv", sep = ""))
