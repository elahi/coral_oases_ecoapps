
tidy_predictions <- function(mat_pred, df_data, obs_name = "observation",
                             prob_lwr = .05, prob_upr = .95) {
  # Get data-frame with one row per fitted value per posterior sample
  df_pred <- mat_pred %>% 
    as_tibble() %>% 
    setNames(seq_len(ncol(.))) %>% 
    tibble::rownames_to_column("posterior_sample") %>% 
    tidyr::gather_(obs_name, "fitted", setdiff(names(.), "posterior_sample"))
  
  # Helps with joining later
  class(df_pred[[obs_name]]) <- class(df_data[[obs_name]])
  
  # Summarise prediction interval for each observation
  df_pred %>% 
    group_by_(obs_name) %>% 
    summarise(median = median(fitted),
              lower = quantile(fitted, prob_lwr), 
              upper = quantile(fitted, prob_upr)) %>% 
    left_join(df_data, by = obs_name)
}
