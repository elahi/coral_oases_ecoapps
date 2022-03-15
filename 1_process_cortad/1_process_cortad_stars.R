################################################################################
##' @title Process raw cortad5 data using the STARS package
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-04-03
##' @log Add a log here
################################################################################

##' These geospatial data are from:

#### LOAD PACKAGES, DATA ####

# Tidyverse
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# Spatial
#library(ncdf4)
library(fuzzyjoin)
library(stars)

source("R/get_base_map.R")
source("R/longitude_conversion_functions.R")
source("1_process_msec/get_msec_z.R")

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

##### STARS TUTORIAL #####

## This fails, because vector memory exhausted (limit reached?)
w <- read_stars("ignore_folder/cortadv5/cortadv5_FilledSST.nc")

##### RASTER ######

## https://pjbartlein.github.io/REarthSysSci/raster_intro.html

# load packages  
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)

# set path and filename
ncpath <- "ignore_folder/cortadv5/"
ncname <- "cortadv5_FilledSST"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

tmp_raster <- brick(ncfname, varname = "FilledSST")
tmp_raster; class(tmp_raster)

plot(tmp_raster)

# rasterVis plot
mapTheme <- rasterTheme(region = rev(brewer.pal(10, "RdBu")))
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)

levelplot(subset(tmp_raster, 100))

plt <- levelplot(subset(tmp_raster, 100), margin = F, 
                 at=cutpts, cuts=11, pretty=TRUE, par.settings = mapTheme)
plt

#plt + layer(sp.lines(world_outline, col="black", lwd=1.0))
