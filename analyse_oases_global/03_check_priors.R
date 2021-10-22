################################################################################
##' @title Check priors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-30
##' @log 
################################################################################

library(rethinking)
inverse_logit <-function(x) return(exp(x)/(1 + exp(x)))

N <- 10000

set.seed(180)

##### OCCUPANCY PRIORS #####

# Alpha_j
mu_alpha <- rnorm(n = N, -1, 1)
mu_alpha_inv <- inverse_logit(mu_alpha)
par(mfrow = c(1,2))
dens(mu_alpha, xlab = "Value", col = rangi2)
dens(mu_alpha_inv, xlab = "Value", col = rangi2)

# Sigma_j
sigma_alpha <- rexp(n = N, rate = 1)
dens(sigma_alpha, xlab = "Value", col = rangi2)

# 
alpha_j_prior <- rnorm(n = N, mu_alpha, sigma_alpha)
alpha_j_prior_inv <- inverse_logit(alpha_j_prior)
dens(alpha_j_prior, xlab = "Value", col = rangi2)
dens(alpha_j_prior_inv, xlab = "Value", col = rangi2)


##### DETECTION PRIORS #####

# Alpha_j
mu_alpha <- rnorm(n = N, 0, 1.25)
mu_alpha_inv <- inverse_logit(mu_alpha)
par(mfrow = c(1,2))
dens(mu_alpha, xlab = "Value", col = rangi2)
dens(mu_alpha_inv, xlab = "Value", col = rangi2)

# Sigma_j
sigma_alpha <- rexp(n = N, rate = 2)
dens(sigma_alpha, xlab = "Value", col = rangi2)

# 
alpha_j_prior <- rnorm(n = N, mu_alpha, sigma_alpha)
alpha_j_prior_inv <- inverse_logit(alpha_j_prior)
dens(alpha_j_prior, xlab = "Value", col = rangi2)
dens(alpha_j_prior_inv, xlab = "Value", col = rangi2)

