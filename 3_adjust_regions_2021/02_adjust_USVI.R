################################################################################
##' @title Adjust USVI region_subs
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
library(tidyverse)
library(viridis)
library(fuzzyjoin)
library(lubridate)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

dat <- read_csv("workspace/coral_total_cover_predictors.csv")

carib <- dat %>% filter(OCEAN == "CARIB") %>% droplevels()

##### USVI #####
usvi <- carib %>% filter(REGION == "USVI") %>% droplevels()
usvi %>% count(REGION, REGION_NEW, REGION_SUB)
usvi <- usvi %>% mutate(REGION = REGION_SUB)

path_to_data <- "data/noaa_station_attributes/usvi/"
file_names <- dir(path = path_to_data, recursive = FALSE, pattern = ".csv")
file_names

i <- 1
dat <- read.csv(paste(path_to_data, file_names[i], sep = ""))

for(i in 2:length(file_names)){
  dat_i <- read.csv(paste(path_to_data, file_names[i], sep = ""))
  dat <- rbind(dat, dat_i)
}

dat2 <- dat %>% 
  select(region:stratum) %>% as_tibble()  %>% 
  rename(SI_LONG = longitude, SI_LATI = latitude, REGION = region, 
         YEAR = survey_year) %>% 
  mutate(DATE = mdy(as.character(survey_date)), 
         SITE = paste("USVI-", station_code, sep = ""))

dat2 <- dat2 %>% 
  select(REGION, YEAR, DATE, SI_LATI, SI_LONG, SITE, biotope:stratum)

my_basemap <- get_basemap(x = dat2, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = dat2, alpha = 0.25, color = "red", 
             aes(x = SI_LONG, y = SI_LATI, color = biotope)) +
  coord_equal() +
  theme(legend.position = "none") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 6, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 6, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() +
  facet_wrap(~ biotope)

# Combine datatsets  
usvi2 <- dat2 %>% 
  select(SI_LATI, SI_LONG, biotope:admin) %>% 
  difference_left_join(usvi, ., by = c("SI_LATI", "SI_LONG"), max_dist = 0.000001)

# For some reason, 4 rows were duplicated
usvi2 <- usvi2 %>% distinct()

# Clean up the column names
names(usvi2)
usvi3 <- usvi2 %>% 
  select(-c(SI_LATI.y, SI_LONG.y)) %>% 
  rename(SI_LATI = SI_LATI.x, SI_LONG = SI_LONG.x)

usvi3 %>% count(biotope)
usvi3 %>% count(REGION_NEW, REGION, REGION_SUB)

# Change REGION_SUB to BIOTOPE
usvi3 <- usvi3 %>% 
  mutate(REGION_SUB = biotope) %>% 
  rename(JURISDICTION = REGION_NEW) %>% 
  select(-biotope)
usvi3 %>% count(JURISDICTION, REGION, REGION_SUB)

# Create new REGIONs
# EDGE = 
# MSR = mid-shelf reef
# SLRK = Sail Rock

statDat <- usvi3
statDat <- statDat %>%
  mutate(REGION = ifelse(REGION_SUB == "MSR" | REGION_SUB == "SLRK" |
                           REGION_SUB == "EDGE",
                         "STT_STJ_offshore", REGION))
statDat <- statDat %>%
  mutate(REGION = ifelse(REGION_SUB == "STJ",
                         "STJ", REGION))

statDat <- statDat %>%
  mutate(REGION = ifelse(REGION_SUB == "STT",
                         "STT", REGION))

statDat %>%
  count(JURISDICTION, REGION, REGION_SUB)
statDat %>%
  count(JURISDICTION, REGION)

# Plot
my_jurisdiction <- "US Virgin Islands"
unique(statDat$REGION)
statDat$REGION <- factor(statDat$REGION,
                         levels = c("STT", "STJ", "STT_STJ_offshore", "STX"))

#statDat <- statDat %>% filter(REGION == "STTSTJ")
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
d_usvi <- statDat

