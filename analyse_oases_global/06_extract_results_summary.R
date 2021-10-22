################################################################################
##' @title Extract relevant summary statistics for Results section
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-07-23
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
library(tidyverse)
z <- 2 # my z-score threshold

##### RAW DATA #####
## Get raw data
source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))
nrow(dat2)

## Make factors
dat2 <- dat %>%
  mutate(Jurisdiction = fct_reorder(JURISDICTION, cover_prop), 
         Ocean = as.factor(OCEAN)) %>% 
  ungroup()

dat2 %>% distinct(cell_2.5, lat_bin_2.5, long_bin_2.5) %>% dim()

dat3 <- dat2 %>% select(human_pop50_log:wave_sd)
dat3 <- dat3[complete.cases(dat3), ]

## Get summary stats
summary0 <- dat2 %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

summary1 <- dat2 %>% 
  group_by(Ocean) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

summary2 <- dat2 %>% 
  group_by(Ocean, Jurisdiction) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

summary3 <- dat2 %>% 
  group_by(Ocean, Jurisdiction, REGION) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)


## In the Results:
summary0

dat2 %>% count(OCEAN)

summary1

summary2

summary3

summary(summary3)

##### OASIS DATA #####

## Function to summarise number of oases by jurisdiction
## dat: input dataframe is gridded
summarise_oases <- function(dat, scale_text){
  oasis_n_df <- dat %>%  
    group_by(JURISDICTION) %>% 
    summarise(oasis_n = sum(oasis), 
              reef_n = sum(Ji), 
              oasis_prop = oasis_n / reef_n)
  
  oasis_n_df <- oasis_n_df %>% 
    mutate(Scale = scale_text)
}

summarise_oases_subjurisdiction <- function(dat, scale_text){
  oasis_n_df <- dat %>%  
    group_by(REGION) %>% 
    summarise(oasis_n = sum(oasis), 
              reef_n = sum(Ji), 
              oasis_prop = oasis_n / reef_n)
  
  oasis_n_df <- oasis_n_df %>% 
    mutate(Scale = scale_text)
}

get_oasis_percent <- function(dat){
  oasis_per_reefs <- sum(dat$oasis) / dim(dat)[1]
  n_grid_cells <- length(unique(dat$cell_2.5))
  
  names(dat)
  dat %>% distinct(YEAR, cell_2.5, Ji) %>% dim()

  
}

##### APPLY FUNCTION TO ALL SCALES #####

dat <- read.csv("workspace/grid_df_w0.csv") 
oasis_0 <- summarise_oases(dat = dat, scale_text = "Cross-basin")

dat <- read.csv("workspace/grid_df_w1.csv") 
oasis_1 <- summarise_oases(dat = dat, scale_text = "Basin")

dat <- read.csv("workspace/grid_df_w2.csv") 
oasis_2 <- summarise_oases(dat = dat, scale_text = "Region")

dat <- read.csv("workspace/grid_df_w3.csv") 
oasis_3 <- summarise_oases_subjurisdiction(dat = dat, scale_text = "Sub-region")

oasis_0
oasis_1
oasis_2
oasis_3
summary(oasis_3)

##### COMPILE #####

oasis_df <- rbind(oasis_0, oasis_1, oasis_2)

## Reorder jurisdiction
oasis_df <- oasis_df %>%
  mutate(scale = factor(Scale,  levels = c("Cross-basin", "Basin", "Region")))

## Reorder variables
oasis_df <- oasis_df %>%
  group_by(scale) %>% 
  mutate(Jurisdiction = fct_reorder(JURISDICTION, oasis_prop)) %>% 
  ungroup()

oasis_df

oasis_df %>% 
  group_by(Scale) %>% 
  summarise(n_reefs = sum(reef_n))
