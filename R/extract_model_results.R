################################################################################
##' @title Extract model summaries
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-08-26
##' @log 
################################################################################

##' Need to load:
##' gridded oasis data
##' zc: jags output
##' X: model matrix

##### SAVE MODEL SUMMARY #####
MCMCsummary(zc, params = "p.chi2b", round = 2)

MCMCdiag(zc, file_name = paste(my_path, "model_output/", "model_summaries/", 
                               my_threshold, my_scale, this_model, 
                               "_model_summary.txt", sep = ""), 
         save_object = FALSE, round = 3)

##### GET COVARIATE NAMES #####
df_jurisdiction_region <- dat %>% 
  distinct(JURISDICTION, REGION)

# Create sample area groups
dat <- dat %>% mutate(group_j = as.integer(as.factor(REGION)))
dat_groups <- dat %>% 
  select(REGION, group_j) %>% distinct()

##### BETA COEFFICIENTS #####
beta_df <- zc %>% 
  gather_draws(beta[i])

# Format names
beta_names <- colnames(X[,-1])
beta_names_df <- tibble(beta_name = beta_names, 
                        i = seq(1:length(beta_names)))

# Join with beta names
beta_df <- beta_df %>% 
  right_join(., beta_names_df, by = "i")
beta_df %>% distinct(i, beta_name, .variable, .chain)

beta_coefs_df <- beta_df %>%
  group_by(beta_name, i, .variable) %>% 
  median_qi(.width = c(0.95, 0.8)) %>% 
  ungroup() 

## Plot chains
beta_df %>% 
  filter(.iteration %% 10 == 0) %>% 
  ggplot(aes(x = .iteration, y = .value, color = as.factor(.chain))) +
  facet_wrap(~ i, scale = "free_y", ncol = 3) + 
  geom_line(alpha = 0.5) + 
  labs(color = ".chain") + 
  theme(legend.position = "none")

ggsave(filename = paste(my_path, "model_output/", "plot_trace/", 
                        my_threshold, my_scale, this_model, "_b_trace.pdf", sep = ""), 
       height = 5, width = 7)

##### ALPHA COEFFICIENTS #####
alpha_df <- zc %>% 
  gather_draws(alpha[i])

# Join with group names
alpha_df <- dat_groups %>% 
  rename(i = group_j) %>% 
  right_join(., alpha_df, by = "i")

names(alpha_df)[1] <- "group"

alpha_coefs_df <- alpha_df %>%
  group_by(group, i, .variable) %>% 
  median_qi(.width = c(0.95, 0.8)) %>% 
  ungroup() %>% 
  mutate(group = as.factor(group))

# Link to get regions
alpha_coefs_df <- df_jurisdiction_region %>% 
  rename(group = REGION) %>% 
  left_join(alpha_coefs_df, .)

alpha_coefs_prob_df <- alpha_coefs_df %>% 
  mutate_at(c(".value", ".lower", ".upper"), inverse_logit)

## Plot chains
alpha_df %>% 
  filter(.iteration %% 10 == 0) %>% 
  ggplot(aes(x = .iteration, y = .value, color = as.factor(.chain))) +
  facet_wrap(~ i, scale = "free_y", ncol = 4) + 
  geom_line(alpha = 0.5) + 
  labs(color = ".chain") + 
  theme(legend.position = "none")

ggsave(filename = paste(my_path, "model_output/", "plot_trace/", 
                        my_threshold, my_scale, this_model, "_a_trace.pdf", sep = ""), 
       height = 10, width = 7)

##### ETA COEFFICIENTS #####
eta_df <- zc %>% 
  gather_draws(eta[i])

# Join with group names
eta_df <- dat_groups %>% 
  rename(i = group_j) %>% 
  right_join(., eta_df, by = "i")

names(eta_df)[1] <- "group"

eta_coefs_df <- eta_df %>%
  group_by(group, i, .variable) %>% 
  median_qi(.width = c(0.95, 0.8)) %>% 
  ungroup() %>% 
  mutate(group = as.factor(group))

# Link to get regions
eta_coefs_df <- df_jurisdiction_region %>% 
  rename(group = REGION) %>% 
  left_join(eta_coefs_df, .)

eta_coefs_prob_df <- eta_coefs_df %>% 
  mutate_at(c(".value", ".lower", ".upper"), inverse_logit)

## Plot chains
eta_df %>% 
  filter(.iteration %% 10 == 0) %>% 
  ggplot(aes(x = .iteration, y = .value, color = as.factor(.chain))) +
  facet_wrap(~ i, scale = "free_y", ncol = 4) + 
  geom_line(alpha = 0.5) + 
  labs(color = ".chain") + 
  theme(legend.position = "none")

ggsave(filename = paste(my_path, "model_output/", "plot_trace/", 
                        my_threshold, my_scale, this_model, "_eta_trace.pdf", sep = ""), 
       height = 10, width = 7)

##### PSI #####
psi_df <- zc %>% 
  gather_draws(psi[i])

psi_coefs_prob_df <- psi_df %>%
  median_qi(.width = c(0.95, 0.8)) %>% 
  ungroup() 

psi_coefs_prob_df <- cbind(psi_coefs_prob_df, 
                      cell_2.5 = rep(dat$cell_2.5, 2))

##### ALPHA HYPERPARAMETERS #####
hyper_df <- zc %>% gather_draws(c(mu.alpha, sigma.alpha))
hyper_coefs_df <- hyper_df %>%
  median_qi(.width = c(0.95, 0.8)) 

## Plot chains
hyper_df %>% 
  filter(.iteration %% 10 == 0) %>% 
  ggplot(aes(x = .iteration, y = .value, color = as.factor(.chain))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ .variable, scales = "free") +
  labs(color = ".chain") + 
  theme(legend.position = "none")

ggsave(filename = paste(my_path, "model_output/", "plot_trace/", 
                        my_threshold, my_scale, this_model, "_hyper_alpha_trace.pdf", sep = ""), 
       height = 3.5, width = 4)

## Plot posterior distributions
hyper_df %>%
  ggplot(aes(x = .value)) +
  geom_density(fill = "gray") + 
  facet_wrap(~ .variable, scales = "free") 
ggsave(filename = paste(my_path, "model_output/", "plot_diagnostics/", 
                        my_threshold, my_scale, this_model, "_hyper_alpha_posterior.pdf", sep = ""), 
       height = 3.5, width = 7)

##### ETA HYPERPARAMETERS #####
hyper_df <- zc %>% gather_draws(c(mu.det, sigma.det))
hyper_coefs_df <- hyper_df %>%
  median_qi(.width = c(0.95, 0.8)) 

## Plot chains
hyper_df %>% 
  filter(.iteration %% 10 == 0) %>% 
  ggplot(aes(x = .iteration, y = .value, color = as.factor(.chain))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ .variable, scales = "free") +
  labs(color = ".chain") + 
  theme(legend.position = "none")

ggsave(filename = paste(my_path, "model_output/", "plot_trace/", 
                        my_threshold, my_scale, this_model, "_hyper_eta_trace.pdf", sep = ""), 
       height = 3.5, width = 4)

## Plot posterior distributions
hyper_df %>%
  ggplot(aes(x = .value)) +
  geom_density(fill = "gray") + 
  facet_wrap(~ .variable, scales = "free") 

ggsave(filename = paste(my_path, "model_output/", "plot_diagnostics/", 
                        my_threshold, my_scale, this_model, "_hyper_eta_posterior.pdf", sep = ""), 
       height = 3.5, width = 7)

##### YNEW #####
ynew_df <- zc %>% gather_draws(c(y.new[i]))
ynew_coefs_df <- ynew_df %>%
  median_qi(.width = c(0.95, 0.8)) 

ynew_coefs_df <- cbind(ynew_coefs_df, 
                       yobs = rep(dat$oasis, 2))

max_ynew <- max(ynew_coefs_df$.value)
max_obs <- max(ynew_coefs_df$yobs)
axis_limit <- max(max_ynew, max_obs) + 1

ynew_coefs_df %>% 
  filter(.width == 0.95) %>% 
  ggplot(aes(yobs, .value)) + 
  geom_abline(aes(slope = 1, intercept = 0), color = "gray", linetype = "dashed") + 
  geom_jitter(size = 1.5, alpha = 1, width = 0.1, height = 0.1, pch = 21) + 
  scale_x_continuous(limits = c(-0.5, axis_limit)) + 
  scale_y_continuous(limits = c(-0.5, axis_limit)) + 
  labs(x = "Oases per grid cell (observed)", y = "Oases per grid cell (predicted)") 

ggsave(filename = paste(my_path, "model_output/", "plot_diagnostics/", 
                        my_threshold, my_scale, this_model, "_yobs_ynew.pdf", sep = ""), 
       height = 3.5, width = 4)

##### GELMAN DIAGNOSTICS #####

## Extract Gelman diagnostic.
gd <- gelman.diag(zc, multivariate = F)
gd_df <- as.data.frame(gd[[1]])
gd_df <- gd_df %>% 
  mutate(.variable = rownames(gd_df))
names(gd_df) <- c(".value", ".upper", ".variable")

## Effective size
gd_df <- gd_df %>% 
  mutate(effective_size_rjags = round(effectiveSize(zc)), 
         .value = round(.value, 3), 
         .upper = round(.upper, 3))

gd_df_short <- gd_df %>%   
  filter(str_detect(tolower(.variable), pattern = "y.new", negate = TRUE)) %>% 
  filter(str_detect(tolower(.variable), pattern = "psi", negate = TRUE)) %>% 
  arrange(desc(.upper))

head(gd_df_short)

##### BAYESIAN P-VALUES #####
bpv_df <- zc %>% gather_draws(p.chi2b, p.ftd)
bpv_summary <- bpv_df %>% 
  group_by(.variable) %>% 
  summarise(mean = mean(.value))
bpv_summary

##### SAVE FILES #####
write.csv(alpha_coefs_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "alpha_coefs_df", ".csv", sep = ""))

write.csv(alpha_coefs_prob_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "alpha_coefs_prob_df", ".csv", sep = ""))

write.csv(beta_coefs_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "beta_coefs_df", ".csv", sep = ""))

write.csv(eta_coefs_prob_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "eta_coefs_prob_df", ".csv", sep = ""))

write.csv(psi_coefs_prob_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "psi_coefs_prob_df", ".csv", sep = ""))

write.csv(gd_df_short, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "gd_df", ".csv", sep = ""))

write.csv(bpv_summary, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "bpv_df", ".csv", sep = ""))  

beta_names_df <- beta_coefs_df %>% distinct(i, beta_name)
write.csv(beta_names_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "beta_names_df", ".csv", sep = ""))

alpha_group_name_df <- alpha_coefs_df %>% distinct(group, i, JURISDICTION)
write.csv(alpha_group_name_df, 
          file = paste(my_path,"model_output/model_summaries/", 
                       my_threshold, my_scale, this_model, "_", "alpha_group_name_df", ".csv", sep = ""))
