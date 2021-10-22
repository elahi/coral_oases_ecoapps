################################################################################
##' @title Clean data: remove duplicates
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-12
##' @log 
################################################################################

##### PATHS #####
#my_path <- ""

##### PACKAGES, DATA #####
library(tidyverse)
source("R/oasis_functions.R")
dat <- read_csv("workspace/coral_total_cover_and_predictors_2021.csv")
names(dat)

##### REMOVE ROWS WITH NAS FOR COVARIATES #####
summary(dat)
dat <- dat %>% drop_na(., c(DEPTH, kd490_cv, kd490_mean, npp_cv, npp_mean, 
                            sst_cv, sst_mean, wave_cv, wave_mean))

##### CHECK RAW DATA FOR DUPLICATES #####

# Note that some sites are duplicated, and it looks due to DEPTH, and duplications only in CARIB
dat %>% 
  select(REGION, REGION_SUB, SITE, DATE, DEPTH, HABITAT, REEF_ZONE, coral_cover) %>% 
  count(REGION, SITE) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  count(REGION)

# Look more closely at Florida
# dat %>% 
#   arrange(REGION, SITE, DATE) %>% 
#   filter(JURISDICTION == "FLORIDA") %>% 
#   select(REGION_SUB, SITE, DATE, DEPTH, HABITAT, REEF_ZONE, coral_cover) %>% 
#   View()

dat %>% 
  count(SITE) %>% 
  arrange(desc(n))

# Removed duplicated rows by selecting only one replicate per SITE
set.seed(23)
dat2 <- dat %>% 
  group_by(SITE) %>% 
  sample_n(., size = 1)

# Check reproducibility
dat2 %>% filter(SITE == "FLK-100") %>% select(DEPTH, coral_cover) # repeat this many times with and without the seed to demonstrate that the seed ensures reproducibility

# Remove duplicates
set.seed(23)
dat <- dat %>% 
  group_by(SITE) %>% 
  sample_n(., size = 1) %>% 
  ungroup()

# Remove Swains because only 2 grid cells
dat %>% 
  filter(REGION == "Swains") %>% 
  count(cell_2.5, REGION)

dat <- dat %>% 
  filter(REGION != "Swains") 
