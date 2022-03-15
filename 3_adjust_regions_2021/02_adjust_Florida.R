################################################################################
##' @title Adjust Florida region_subs
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

##### FLORIDA #####
my_regions <- c("FLK", "SEFCRI", "Tortugas")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

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
  ggtitle(my_regions) + 
  facet_wrap(~ REGION_SUB)

# Rename columns
statDat %>% count(REGION_NEW, REGION, REGION_SUB)

statDat <- statDat %>% 
  rename(JURISDICTION = REGION_NEW) %>% 
  mutate(JURISDICTION = "Florida", 
         admin = NA)
names(statDat)
statDat %>% count(JURISDICTION, REGION, REGION_SUB)

names(statDat)
statDat %>% count(JURISDICTION, REGION, REGION_SUB)

# Create new REGION for Upper, Middle and Lower Keys
statDat <- statDat %>% 
  mutate(REGION = ifelse(REGION_SUB == "Biscayne" | REGION_SUB == "Upper Keys", 
                             "Keys_upper", REGION))
statDat <- statDat %>% 
  mutate(REGION = ifelse(REGION_SUB == "Middle Keys" | REGION_SUB == "Mid-Upper Keys Transition", 
                         "Keys_middle", REGION))
statDat <- statDat %>% 
  mutate(REGION = ifelse(REGION_SUB == "Lower Keys", 
                         "Keys_lower", REGION))

# Plot
my_jurisdiction <- "Florida"
unique(statDat$REGION)
statDat$REGION <- factor(statDat$REGION, 
                         levels = rev(c("SEFCRI", "Keys_upper", "Keys_middle", "Keys_lower", 
                                    "Tortugas")))

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
d_florida <- statDat
d_florida %>% count(JURISDICTION, REGION)
d_florida %>% count(JURISDICTION, REGION, REGION_SUB)
