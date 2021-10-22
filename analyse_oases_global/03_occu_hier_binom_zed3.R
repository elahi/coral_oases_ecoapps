################################################################################
##' @title Hierarchical occupancy model for coral oases, zed3
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-24
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

my_threshold <- "z2_"
my_scale <- "zed3_"
this_model <- "occu_hier_binom"

##### PACKAGES, DATA #####
source("R/inverse_logit.R")

library(tidyverse)
library(rjags)
library(tidybayes)

dat <- read.csv("workspace/grid_df_w3.csv")
sum(dat$oasis_present)/nrow(dat) * 100 # percent of cells with oasis
sum(dat$Ji)
df_jurisdiction_region <- dat %>% 
  distinct(JURISDICTION, REGION)

##### PREPARE DATA FOR JAGS #####
names(dat)

# Create sample area groups
dat <- dat %>% mutate(group_j = as.integer(as.factor(REGION)))
dat_groups <- dat %>% 
  select(REGION, group_j) %>% distinct()
y.n.sites <- length(unique(dat$group_j))

source(paste(my_path, "models/model_matrices.R", sep = ""))
colnames(X)

nX <- ncol(X) - 1

# Get data
dat_list = list(
  N = nrow(dat), 
  y = as.double(dat$oasis), # number of oasis detections
  n = as.double(dat$Ji), # number of visits
  X = X[ , -1], 
  nX = nX, 
  y.group = dat$group_j, 
  y.n.sites = y.n.sites, 
  e = 0.001
)

##### MODEL 3: HIERARCHICAL, WITH COVARIATES; IN PARALLEL ####

# Number of iterations
n.adapt <- 10000
n.update <- 40000
n.iter <- 80000
n.thin <- 10

n_clusters <- 3
library(parallel)
cl <- makeCluster(n_clusters)
pidList <- NA

for(i in 1:n_clusters){
  pidNum <- capture.output(cl[[i]])
  start <- regexpr("pid", pidNum)[[1]]
  end <- nchar(pidNum)
  pidList[i] <- substr(pidNum, (start + 4), end)
}

initsP = list(
  list(z = rep(1, nrow(dat)), eta = rep(runif(1, -2, 2), y.n.sites),
       alpha = rep(runif(1, -2, 2), y.n.sites), beta = runif(nX, -2, 2), 
       .RNG.name = "base::Mersenne-Twister", .RNG.seed = 1), 
  list(z = rep(1, nrow(dat)), eta = rep(runif(1, -2, 2), y.n.sites),
       alpha = rep(runif(1, -2, 2), y.n.sites), beta = runif(nX, -2, 2), 
       .RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = 22), 
  list(z = rep(1, nrow(dat)), eta = rep(runif(1, -2, 2), y.n.sites),
       alpha = rep(runif(1, -2, 2), y.n.sites), beta = runif(nX, -2, 2), 
       .RNG.name = "base::Wichmann-Hill", .RNG.seed = 373)
)

parallel::clusterExport(cl, c("my_path", "dat_list", "X", "nX", "y.n.sites",
                              "pidList", "initsP", "n.adapt", "n.update", "n.iter", "n.thin"))

start_time <- Sys.time()
start_time
out <- clusterEvalQ(cl, {
  library(rjags)
  processNum <- which(pidList==Sys.getpid())
  worker_inits <- initsP[[processNum]]
  
  jm <- jags.model(paste(my_path, "models/occu_hier_binom_jags.R", sep = ""),
                   data = dat_list, inits = worker_inits, 
                   n.chains = 1, 
                   n.adapt = n.adapt)
  
  update(jm, n.iter = n.update)
  
  zmCore <- coda.samples(jm, variable.names = c("p.chi2b", "d.chi2b.data", "d.chi2b.sim", 
                                                "p.ftd", "d.ftd.data", "d.ftd.sim", 
                                                "y.new", "psi",
                                                "eta", "beta", 
                                                "alpha", "mu.alpha", "sigma.alpha", 
                                                "mu.det", "sigma.det"), 
                         n.iter = n.iter, thin = n.thin)
  
  return(as.mcmc(zmCore))
})
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time
zc <- mcmc.list(out)
str(zc)

saveRDS(zc, file = paste(path_to_model_rds, my_threshold, my_scale, this_model, 
                          ".rds", sep = ""))