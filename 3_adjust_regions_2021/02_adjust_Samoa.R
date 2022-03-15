################################################################################
##' @title Adjust Samoa region
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

##### Marianas #####
statDat <- dat %>% filter(REGION == "SAMOA") %>% droplevels()
statDat %>% 
  count(REGION, REGION_NEW, REGION_SUB) %>% print(n=100)

statDat <- statDat %>% 
  rename(JURISDICTION = REGION) %>% 
  mutate(REGION = REGION_SUB, 
         admin = NA) %>% 
  select(-REGION_NEW)
  
statDat %>% 
  count(JURISDICTION, REGION, REGION_SUB)

# Create new REGION for Samoa
statDat <- statDat %>% 
  mutate(REGION = ifelse(REGION_SUB == "Ofu & Olosega" | REGION_SUB == "Tau", 
                         "Manua_islands", REGION))
statDat %>% 
  count(JURISDICTION, REGION)

# Plot
my_jurisdiction <- "Samoa"
unique(statDat$REGION)
statDat$REGION <- factor(statDat$REGION, 
                         levels = c("Swains", "Tutuila", "Manua_islands", "Rose"))

my_basemap <- get_basemap(x = statDat, high_res = T)
ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = REGION)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1,
                            title = "Sub-region",
                            override.aes = list(alpha = 1),
                            title.position = "top"),
         size = guide_legend(nrow = 1,
                             title = "Coral (%)",
                             title.position = "top")) +
  theme(legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_jurisdiction)

statDat %>% count(JURISDICTION, REGION)
ggsave(paste(path_to_figs, "map2021_", my_jurisdiction, ".pdf", sep = ""), height = 7, width = 10)

# Rename object
d_samoa <- statDat
names(d_samoa)
