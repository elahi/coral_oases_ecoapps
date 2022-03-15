################################################################################
##' @title Prepare dataframe for analysis, with total coral cover and predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-09-23
##' @log 2020-12-06: updated with all calcifying corals
################################################################################

#### LOAD PACKAGES, DATA ####

library(tidyverse)
source("R/get_gridded_bins.R")

## Coral cover
coral <- read_csv("workspace/coral_total_cover.csv") %>% 
  select(-X1)
coral
names(coral)
coral %>% count(REGION, long_bin_2.5, lat_bin_2.5)
str(coral$REEF_ZONE)

## Predictors
pred <- read_csv("workspace/predictor_df_clean.csv") 
names(pred)
pred %>% count(REGION, REGION_SUB, long_bin_2.5, lat_bin_2.5)
pred <- pred %>% 
  select(-c(X1, OCEAN:REGION_SUB, YEAR, DATE, long_bin_2.5, lat_bin_2.5))
pred

## Combine with inner_join
names(coral)
names(pred)

dat <- coral %>% 
  left_join(., pred, by = c("SI_LATI", "SI_LONG"))

summary(dat)

dat <- dat %>% 
  rename(long_bin_2.5_orig = long_bin_2.5, 
         lat_bin_2.5_orig = lat_bin_2.5)

## Get gridded bins
res <- 0.5 / 60 # 0.5 arc-minutes (i.e., 30 arc-seconds; i.e., ~1km)
dat <- get_lat_long_bin(dat, bin_size = res, msec = F) %>% 
  rename(long_bin_0.5 = long_bin, lat_bin_0.5 = lat_bin)

res <- 2.5 / 60 # 2.5 arc-minutes (~4.6km)
dat <- get_lat_long_bin(dat, bin_size = res, msec = F) %>% 
  rename(long_bin_2.5 = long_bin, lat_bin_2.5 = lat_bin)

names(dat)

dat %>% 
  ggplot(aes(x = lat_bin_2.5_orig, y = lat_bin_2.5)) + 
  geom_point()

## Clean up
dat <- dat %>%
  select(-c(ll_id.x, ll_id.y, long_bin_2.5_orig, lat_bin_2.5_orig))
names(dat)
summary(dat)

## Check REGIONS
dat %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

dat %>% 
  count(REGION, YEAR) %>% print(n=100)

dat <- dat %>% 
  mutate(REGION_SUB = ifelse(REGION == "FGBNMS", "FGBNMS", REGION_SUB)) %>% 
  mutate(REGION_SUB = ifelse(REGION == "PRICO", "PRICO", REGION_SUB)) %>% 
  mutate(REGION_NEW = ifelse(REGION_SUB == "Wake", "Wake", REGION_NEW))

dat %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

names(dat)

## Write to file
write.csv(dat, "workspace/coral_total_cover_predictors.csv")


