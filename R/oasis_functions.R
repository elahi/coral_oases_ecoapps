################################################################################
##' @title Helper functions to standardize variables and designate oases
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-18
##' @log Add a log here
################################################################################

##' Transorm coral cover (from Darling et al. 2019)
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

##' Rescale continuous predictors (divide mean by 2 sds)
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = TRUE)) / (2 * sd(x, na.rm = TRUE))

##' Calculate zed score by a grouping and response (summary) variable
##' Use tidy_eval
get_zed_group <- function(x, group_var, summary_var){
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  
  x <- x %>%
    group_by(!!group_var) %>% 
    mutate(mu = mean(!!summary_var, na.rm = TRUE), 
           stdev = sd(!!summary_var, na.rm = TRUE), 
           zed = (!!summary_var - mu)/stdev) %>% 
    ungroup()
  
  x <- x %>% 
    select(-c(mu, stdev))
  
  return(x)
} 

##' Assign oases based on a grouping variable and response variable to summarize
assign_oases <- function(d, group_var, summary_var, z_threshold){
  
  library(tidyverse)

  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  
  d <- d %>% 
    group_by(YEAR) %>% 
    do(., get_zed_group(., group_var = !!group_var, summary_var = !!summary_var)) %>% 
    ungroup()
  
  z_threshold_lower <- -z_threshold
  
  d <- d %>% 
    mutate(oasis = ifelse(zed > z_threshold, "oasis", "not_oasis"), 
           oasis_logic = ifelse(zed > z_threshold, TRUE, FALSE), 
           desert_logic = ifelse(zed < z_threshold_lower, TRUE, FALSE), 
           oasis_bd = ifelse(oasis_logic == TRUE, "bright", 
                             ifelse(desert_logic == TRUE, "dark", "average")))

  
  return(d)
}

##' Grid oasis data
##' Without year

# d <- dat %>% rename(oasis = oasis0)
# d %>% count(oasis) %>% summarise(sum(n))


grid_oasis_data <- function(d, grid_summary){
  
  grid_reef_count <- d %>% 
    count(REGION, 
          oasis, cell_2.5, lat_bin_2.5, long_bin_2.5)
  
  #grid_reef_count %>% summarise(sum(n))
  
  grid_df <- inner_join(grid_summary, grid_reef_count, 
                        by = c("REGION",  
                               "cell_2.5", "lat_bin_2.5", "long_bin_2.5"))
  
  #grid_df %>% summarise(sum(n))
  
  grid_df_w <- grid_df %>% spread(key = oasis, value = n) %>% 
    mutate(oasis_present = ifelse(!is.na(oasis), 1, 0))
  
  # Get Ji observations, replace NAs with zeros and then sum
  grid_df_w <- grid_df_w %>% 
    replace_na(list(not_oasis = 0, oasis = 0)) %>% 
    mutate(Ji = not_oasis + oasis)
  
}
