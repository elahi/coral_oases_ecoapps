################################################################################
##' @title Function to extract MSEC predictors for a 
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-09-10
##' @log Add a log here
################################################################################

## df = dataframe of sites with noaa lat-longs labeled as SI_LATI and SI_LONG
## j is the number in the file path (e.g., to retrieve distance to market)
## p = the multiplier of bin size to expand the desired coordinates

library(dplyr)
library(ncdf4)
library(fuzzyjoin)

get_msec_z <- function(df, j = j, p = 2, nc_dim_logical = FALSE, nc_dim){
  
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
  
}
