################################################################################
##' @title Make df of samples
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2022-02-12
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
library(tidyverse)
library(forcats)

my_scale <- "zed0_"
statDat <- read.csv("workspace/grid_df_w0.csv")
summary(statDat)

## Relevel jurisdiction
statDat$JURISDICTION <- fct_relevel(statDat$JURISDICTION,
                                    "MARIAN", "NWHI", "MHI", "SAMOA", "FLORIDA", "PRICO", "USVI")

df_samples <- statDat %>% 
  count(OCEAN, JURISDICTION, REGION) %>% 
  mutate(Ocean = OCEAN, Jurisdiction = JURISDICTION, `Sub-jurisdiction` = REGION) %>%
  mutate(Region = case_when(Jurisdiction == "FLORIDA" ~ "Florida", 
                            Jurisdiction == "PRICO" ~ "Puerto Rico", 
                            Jurisdiction == "USVI" ~ "US Virgin Islands", 
                            Jurisdiction == "MARIAN" ~ "Mariana Islands", 
                            Jurisdiction == "PRICO" ~ "Puerto Rico", 
                            Jurisdiction == "NWHI" ~ "Northwest Hawaiian Islands", 
                            Jurisdiction == "MHI" ~ "Main Hawaiian Islands", 
                            Jurisdiction == "SAMOA" ~ "American Samoa"
                            )) %>% 
  mutate(Subregion = case_when(REGION == "SEFCRI" ~ "Southeast_Florida", 
                               REGION == "EPRICO" ~ "Puerto_Rico_east", 
                               REGION == "NPRICO" ~ "Puerto_Rico_north", 
                               REGION == "SWPRICO" ~ "Puerto_Rico_southwest", 
                               REGION == "STJ" ~ "StJohn", 
                               REGION == "STT" ~ "StThomas", 
                               REGION == "STT_STJ_offshore" ~ "StJohn_StThomas_offshore", 
                               REGION == "STX" ~ "StCroix", 
                               REGION == "French Frigate" ~ "French_Frigate", 
                               REGION == "Pearl & Hermes" ~ "Pearl_Hermes", 
                               TRUE ~ as.character(REGION)))
df_samples

write.csv(df_samples, "data_output/df_samples.csv")
