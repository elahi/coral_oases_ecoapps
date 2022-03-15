################################################################################
##' @title Function to assign lat-long bins to a dataframe
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com 
##' @date 2017-10-12
##' 
##' @log Add a log here
################################################################################

##' Get binned lat or long (numeric)
##' x = lat or long (vectorized function)
get_lat_bin <- function(x, bin_lat_vector = bin_lat){
  lat_bin <- findInterval(x, bin_lat_vector)
  return(bin_lat_vector[lat_bin])
}

get_long_bin <- function(x, bin_long_vector = bin_long){
  long_bin <- findInterval(x, bin_long_vector)
  return(bin_long_vector[long_bin])
}

#bin_size = 1

##' The above functions are used here, to assign a GPS position to a gridded bin
get_lat_long_bin <- function(x, bin_size = 0.25, msec = FALSE){
  
  ## The original way
  if(msec == FALSE){
    # Get vectors of lat and long
    bin_lat <- seq(-90, 90, by = bin_size)
    bin_long <- seq(-180, 180, by = bin_size)
    
    x <- x %>% 
      mutate(long_bin = get_long_bin(SI_LONG, bin_long_vector = bin_long), 
             lat_bin = get_lat_bin(SI_LATI, bin_lat_vector = bin_lat))
    
    # Ensure that the lat and long are centered in the grid cell
    x <- x %>% 
      mutate(long_bin = long_bin + bin_size/2, 
             lat_bin = lat_bin + bin_size/2) 
    
    return(x)
  }
  
  ## Modified way to match MSEC coords
  if(msec == TRUE){
    # Hard-coded lengths from human pop 20km msec nc file
    nc_lat_length <- 4320
    nc_lon_length <- 8639
    # Bin size
    nc_lon_bin_size <- 360 / nc_lon_length
    nc_lat_bin_size <- 180 / nc_lat_length
    # Get vectors of lat and long
    bin_lat <- seq(-90, 90, length.out = nc_lat_length)
    bin_long <- seq(-180, 180 - nc_lon_bin_size, length.out = nc_lon_length)
    
    x <- x %>% 
      mutate(long_bin = get_long_bin(SI_LONG, bin_long_vector = bin_long), 
             lat_bin = get_lat_bin(SI_LATI, bin_lat_vector = bin_lat))

    return(x)
  }

}

