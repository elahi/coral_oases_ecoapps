################################################################################
##' @title Adjust NWHI region
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

##### NWHI #####
statDat <- dat %>% filter(REGION == "NWHI") %>% droplevels()
statDat %>% 
  count(REGION, REGION_NEW, REGION_SUB) %>% print(n=100)

statDat <- statDat %>% 
  rename(JURISDICTION = REGION) %>% 
  mutate(REGION = REGION_SUB, 
         admin = NA) %>% 
  select(-REGION_NEW)
  
statDat %>% 
  count(JURISDICTION, REGION, REGION_SUB)

# Plot
my_jurisdiction <- "Northwest Hawaiian Islands"
unique(statDat$REGION)
statDat$REGION <- factor(statDat$REGION, 
                         levels = c("Kure", "Pearl & Hermes", "Lisianski", "French Frigate"))

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
d_nwhi <- statDat
names(d_nwhi)
