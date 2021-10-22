################################################################################
##' @title Load coral cover data
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-12-04
##' @log 2021-08-06: removed duplicate sites
################################################################################

##### PACKAGES, DATA #####
library(tidyverse)

##### CORAL COVER DATA #####
dat <- read.csv("workspace/coral_total_cover_and_predictors_zed.csv")

## Make factors
dat2 <- dat %>%
  mutate(Jurisdiction = fct_reorder(JURISDICTION, cover_prop), 
         Ocean = as.factor(OCEAN)) %>% 
  ungroup()
levels(dat2$Jurisdiction)

## Rename variables
levels(dat2$Jurisdiction) <- c("Florida",
                               "US Virgin Islands", 
                               "Northwest\nHawaiian Islands",
                               "Puerto Rico", 
                               "Main\nHawaiian Islands", 
                               "Mariana Islands", 
                               "American Samoa")
levels(dat2$Ocean) <- c("Atlantic", "Pacific")
levels(dat2$Jurisdiction)

dat2 %>% count(JURISDICTION, REGION)

dat2 <- dat2 %>% 
  mutate(Ocean = OCEAN, `Sub-jurisdiction` = REGION)

