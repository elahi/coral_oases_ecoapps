################################################################################
##' @title Useful functions for Stan Bayes
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-12
##' @log 
################################################################################


y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}


## Function to extract fitted and predicted draws using tidybayes

fit_n_predict <- function(dat, model){
  
  library(tidybayes)
  library(dplyr)
  
  ## Model fits
  y_fitted <- add_fitted_draws(newdata = dat, model = model, re_formula = NA)
  y_fitted <- y_fitted %>% 
    mutate(.residual = cover_prop2 - .value)
  
  ## Predict data for each iteration
  # y_new <- add_predicted_draws(newdata = dat, model = model, re_formula = NA)
  # 
  # y_fitted$.prediction <- y_new$.prediction
  
  return(y_fitted)

}

## Function to calculate Bayesian p-values, using the output from fit_n_predict
calculate_bpv <- function(dat){
  
  dat <- dat %>% 
    mutate(sq_error_data = (size_log - fitted_median)^2, 
           sq_error_new = (.prediction - fitted_median)^2, 
           I_k = ifelse(sq_error_new > sq_error_data, 1, 0))
  
  bpv_discrepancy <- sum(dat$I_k) / length(dat$I_k)
  
  return(bpv_discrepancy)
}


