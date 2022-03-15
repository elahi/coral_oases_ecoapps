################################################################################
##' @title Adjust FGBNMS region_subs
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-14
##' @log Add a log here
################################################################################

#### PATHS #####
my_path <- "3_adjust_regions_2021/"
path_to_figs <- paste(my_path, "analysis_figs/", sep = "")
path_to_output <- paste(my_path, "analysis_output/", sep = "")

#### LOAD PACKAGES, DATA ####
source("R/get_base_map.R")
source("R/zed_by_group.R")
library(tidyverse)
library(viridis)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

dat <- read_csv("workspace/coral_total_cover_predictors.csv")

dat %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

dat %>% 
  count(REGION_NEW, YEAR) %>% print(n=100)

##### FGBNMS #####
my_regions <- c("FGBNMS")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = YEAR)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 1, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_c() + 
  ggtitle(my_regions)

dat %>% count(REGION, REGION_SUB)

dat <- dat %>% 
  mutate(REGION_SUB = ifelse(REGION == "FGBNMS" & SI_LONG > -93.7, "FGB_E", 
                             ifelse(REGION == "FGBNMS" & SI_LONG < -93.7, "FGB_W", REGION_SUB)))

statDat <- statDat %>% 
  mutate(REGION_SUB = ifelse(SI_LONG > -93.7, "FGB_E", "FGB_W"), 
         admin = "NMS")

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 1, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

# Rename columns
statDat <- statDat %>% 
  rename(JURISDICTION = REGION_NEW) %>% 
  mutate(REGION = REGION_SUB)
names(statDat)
statDat %>% count(JURISDICTION, REGION, REGION_SUB)

# Rename object
names(statDat)
d_fgbnms <- statDat
d_fgbnms %>% count(JURISDICTION, REGION, REGION_SUB)



