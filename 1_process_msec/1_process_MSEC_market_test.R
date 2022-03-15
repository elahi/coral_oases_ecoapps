################################################################################
##' @title Process MSEC predictors - distance to market
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
path_to_data <- "/Volumes/archive/pwg_data/yeager_msec_nc/"
#path_to_data <- "/Volumes/sdxc1/"

# Retrieve a list of nc files in my data folder:
flist <- list.files(path = path_to_data, pattern = "^.*\\.(nc|NC|Nc|Nc)$")
flist
gsub("\\..*","",flist)

#### MSEC DATA - DISTANCE TO MARKET ####

## CHANGE j FOR EACH MSEC VARIABLE ##
# File #
j = 1
msec_var <- gsub("\\..*", "", flist[j])

# Subset the data
noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])

# Initiate a loop that will get z for the first REGION_NEW
noaa_ll_new <- get_msec_z(df = noaa_ll_i, j = j, p = 2)

# Run loop for remaining REGION_NEW
for(i in 2:length(i_unique)) {
  noaa_ll_i <- noaa_ll %>% filter(REGION_NEW == i_unique[i])
  noaa_ll_i <- get_msec_z(df = noaa_ll_i, j = j, p = 2)
  noaa_ll_new <- rbind(noaa_ll_new, noaa_ll_i)
}

noaa_ll_new
write.csv(noaa_ll_new, file = paste("data_output/noaa_", msec_var, ".csv", sep = ""))

# Validation - upload SHINY data
msec_out <- read_csv("data_output/msec_out_distmarket.csv") %>% 
  mutate(ll_id = seq_along(long))

msec_out <- noaa_ll_new %>% select(ll_id, z) %>% 
  left_join(msec_out, ., by = "ll_id")

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank()))

msec_out %>% 
  ggplot(aes(dist_market, z)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "gray") + 
  geom_point(alpha = 1, shape = 1) + 
  labs(x = "Shiny", y = "NetCDF", title = "Distance to market (km)")

ggsave("figs/msec_shiny_netcdf_scatter_market.pdf", height = 3.5, width = 3.5)

### TEST

flist
j = 3 # human pop has 6 dimensions
j = 6 # npp mean has 2 dims
j = 8 # npp mean has 2 dims

ncFile <- nc_open(paste0(path_to_data, flist[j]))
ncFile

# Close the connection
nc_close(ncFile)

# # Select a time layer
# nc_hp1990 <- humanpop_20km[ , , 1] # Assumes 1 is 1990
# nc_hp2015 <- humanpop_20km[ , , 6] # Assumes 6 is 2015
# 
# # Check this assumption - is 6 > 1?
# test1990 <- colSums(nc_hp1990, na.rm = TRUE)
# test2015 <- colSums(nc_hp2015, na.rm = TRUE)
# test_lat <- as.numeric(names(test1990))
# 
# plot(test2015, test_lat, type = 'l')
# points(test1990, test_lat, type = 'l', col = 'red')
# plot(test2015 - test1990, test_lat, type = 'l')

# Select 2015
nc_hp20 <- humanpop_20km[ , , 6] # Assumes 6 is 2015
nc_hp20 <- t(nc_hp20)
nc_hp20[1:20, 1:20]
dimnames(nc_hp20)

### Global attributes

# Retrieve the attributes
nc_atts <- ncatt_get(nc, 0)
names(nc_atts)

# Close the connection
nc_close(nc)

######
# Subset by latitude & longitude (dimension 1)
lat_vector <- as.numeric(dimnames(nc_hp20)[[1]])
long_vector <- as.numeric(dimnames(nc_hp20)[[2]])
range(lat_vector); range(long_vector)

lat_range <- noaa_ll %>% filter(OCEAN == "CARIB") %>% get_ll_range(., axis = "lat")
long_range <- noaa_ll %>% filter(OCEAN == "CARIB") %>% get_ll_range(., axis = "long")
long_range
long_range <- long_range + 360 # if carib

lat_vector_logical <- lat_vector >= lat_range[1] & lat_vector <= lat_range[2]
long_vector_logical <- long_vector >= long_range[1] & long_vector <= long_range[2]

crap <- nc_hp20[lat_vector_logical, long_vector_logical]
crap
str(crap)

crap_vector <- as.vector(crap)
lat = as.numeric(dimnames(crap)[[1]])
lon = as.numeric(dimnames(crap)[[2]])

my_df <- cbind( rep(lat, each=length(lon)), rep(lon,length(lat)), c(crap_vector) ) %>% tbl_df()


crap_df <- data.frame(hp = crap_vector, 
                      lat = as.numeric(dimnames(crap)[[1]]), 
                      long = as.numeric(dimnames(crap)[[2]]))

head(crap_df)

ggplot(my_df, aes(V2, V1)) + 
  geom_raster(aes(fill = V3)) + 
  scale_fill_gradient(low = "white", high = "steelblue")

