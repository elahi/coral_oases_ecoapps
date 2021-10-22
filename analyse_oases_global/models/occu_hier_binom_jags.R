
model{
  
  # priors for occupancy model
  for(i in 1:nX){
    beta[i] ~ dnorm(0, 1)
  }
  
  # hyper-priors for occupancy model
  mu.alpha ~ dnorm(-1, 1)
  sigma.alpha ~ dexp(1)
  tau.alpha <- 1/sigma.alpha^2
  for(j in 1:y.n.sites){
    alpha[j] ~ dnorm(mu.alpha, tau.alpha)
  }
  
  # hyper-priors for detection model
  mu.det ~ dnorm(-1, 1)
  sigma.det ~ dexp(1)
  tau.det <- 1 / sigma.det^2
  for(j in 1:y.n.sites){
    eta[j] ~ dnorm(mu.det, sigma.det)
  } 
  
  # likelihood
  for(i in 1:N){
    
    # occupancy model
    logit(psi[i]) <- alpha[y.group[i]] + inprod(beta[], X[i, ] )
    z[i] ~ dbern(psi[i])                                          
    
    # detection model
    logit(p[i]) <- eta[y.group[i]]                                
    mu.p[i] <- z[i] * p[i]                                        
    y[i] ~ dbin(mu.p[i], n[i])
    
    # simulate new data, conditional on model parameters
    y.new[i] ~ dbin(mu.p[i], n[i])                                
    
    # chi-square discrepancy for a binomial; e is small value to avoid division by zero
    chi2b.data[i] <-  ((y[i] - mu.p[i] * n[i]) / sqrt((mu.p[i] + e) * n[i] * (1 - mu.p[i] - e)))^2
    chi2b.sim[i] <-  ((y.new[i] - mu.p[i] * n[i]) / sqrt((mu.p[i] + e) * n[i] * (1 - mu.p[i] - e)))^2
    
    # freeman-tukey discrepancy for a binomial
    ftd.data[i] <- (sqrt(y[i]) - sqrt(p[i] * z[i] * n[i]))^2 
    ftd.sim[i] <-  (sqrt(y.new[i]) - sqrt(p[i] * z[i] * n[i]))^2 

  }
  
  # bayesian p-value for chi-square discrepancy
  d.chi2b.data <- sum(chi2b.data)
  d.chi2b.sim <- sum(chi2b.sim)
  p.chi2b <- step(d.chi2b.sim - d.chi2b.data)
  
  # bayesian p-value for freeman-tukey discrepancy
  d.ftd.data <- sum(ftd.data)
  d.ftd.sim <- sum(ftd.sim)
  p.ftd <- step(d.ftd.sim - d.ftd.data)
  
}

