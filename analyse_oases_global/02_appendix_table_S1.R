################################################################################
##' @title Appendix: Table S1
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-21
##' @log 
################################################################################

##' Number of grid cells (i) sampled within each sub-region, region, and ocean. 

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
source("R/HighstatLibV6.R")
library(tidyverse)
library(viridis)
library(ggcorrplot)
library(forcats)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

my_scale <- "zed0_"
statDat <- read.csv("workspace/grid_df_w0.csv")
summary(statDat)

## Relevel jurisdiction
statDat$JURISDICTION <- fct_relevel(statDat$JURISDICTION,
                                    "MARIAN", "NWHI", "MHI", "SAMOA", "FLORIDA", "PRICO", "USVI")

df_samples <- statDat %>% 
  count(OCEAN, JURISDICTION, REGION) %>% 
  mutate(Ocean = OCEAN, Region = JURISDICTION, Subregion = REGION) %>% 
  select(Ocean, Region, Subregion, n) %>% 
  rename(i = n)

df_samples$Ocean <- fct_recode(df_samples$Ocean, 
                               `Western Atlantic` = "CARIB", 
                               Pacific = "PACIFIC")

df_samples$Region <- fct_recode(df_samples$Region, 
                                `Mariana Islands` = "MARIAN", 
                                `Main Hawaiian Islands` = "MHI", 
                                `Northwest Hawaiian Islands` = "NWHI", 
                                `American Samoa` = "SAMOA", 
                                Florida = "FLORIDA", 
                                `Puerto Rico` = "PRICO", 
                                `US Virgin Islands` = "USVI")

df_samples$Subregion <- fct_recode(df_samples$Subregion, 
                                   `Manua Islands` = "Manua_islands", 
                                   `Marianas-middle` = "Marianas_middle", 
                                   `Marianas-lower` = "Marianas_lower", 
                                   `Marianas-upper` = "Marianas_upper", 
                                   `Keys-upper` = "Keys_upper", 
                                   `Keys-middle` = "Keys_middle", 
                                   `Keys-lower` = "Keys_lower", 
                                   `Southeast Florida` = "SEFCRI", 
                                   `Puerto Rico-north` = "NPRICO", 
                                   `Puerto Rico-east` = "EPRICO", 
                                   `Puerto Rico-southwest` = "SWPRICO", 
                                   `St. Croix` = "STX", 
                                   `St. John` = "STJ", 
                                   `St. Thomas` = "STT", 
                                   `St. J & St. T offshore` = "STT_STJ_offshore")



write.csv(df_samples, "data_output/df_samples.csv")
