##' Robin Elahi
##' 20 August 2018
##' Functions to convert longitude between -180 & 180 and 0 & 360

convert_180_to_360 <- function(x){
  # Add 360 to negative values
  x[x < 0] <- x[x < 0] + 360
  return(x)
}

convert_360_to_180 <- function(x){
  # Subtract 360 to values >= 180
  x[x >= 180] <- x[x >= 180] - 360
  return(x)
}


# nc_to_180 <- function(x, nc_lon_bin_size = 360/8639){
#   
#   x[x > 180] <- x[x > 180] - 360 - nc_lon_bin_size
#   x[x < 180] <- x[x < 180] + nc_lon_bin_size / 2 
#   return(x)
#   
# }
