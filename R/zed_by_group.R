



##' Use group_by ...
get_zed_coral <- function(x, ...){
  x %>%
    group_by(...) %>% 
    mutate(mu = mean(HARD_CORAL, na.rm = TRUE), 
           stdev = sd(HARD_CORAL, na.rm = TRUE), 
           zed = (HARD_CORAL - mu)/stdev) %>% 
    ungroup()
}  

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
