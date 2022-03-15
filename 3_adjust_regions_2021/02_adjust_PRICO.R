################################################################################
##' @title Adjust PRICO region_subs
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

##### PRICO #####
prico <- carib %>% filter(REGION == "PRICO") %>% droplevels()
prico %>% count(YEAR)

path_to_data <- "data/noaa_station_attributes/prico/"

file_names <- dir(path = path_to_data, recursive = FALSE, pattern = ".csv")
file_names

i <- 1
d1 <- read_csv(paste(path_to_data, file_names[i], sep = ""))
names(d1)

d1 <- d1 %>% 
  select(region:admin) %>% as_tibble()  %>% 
  rename(SI_LONG = longitude, SI_LATI = latitude, REGION = region, 
         YEAR = survey_year) %>% 
  mutate(DATE = mdy(as.character(survey_date)), 
         biotope = paste(biotope, "PRICO", sep = "")) %>% 
  select(-c(survey_date:station_code))

d2 <- read_csv(paste(path_to_data, file_names[2], sep = ""))

d2 <- d2 %>% 
  select(REGION, YEAR, MONTH, DAY, 
         LAT_DEGREES, LON_DEGREES, SUB_REGION_NAME, ADMIN)

d2 <- d2 %>% 
  mutate(DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>% 
  rename(biotope = SUB_REGION_NAME, admin = ADMIN, 
         SI_LATI = LAT_DEGREES, SI_LONG = LON_DEGREES) %>% 
  select(-c(MONTH, DAY))

df <- rbind(d1, d2)
df %>% count(YEAR)

my_basemap <- get_basemap(x = df, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = df, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, color = biotope)) +
  coord_equal() +
  theme(legend.position = "right") + 
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
  scale_color_viridis_d() 

# Get distinct lat-longs
df2 <- df %>% distinct(SI_LATI, SI_LONG, biotope, admin)

# Combine datatsets  
prico2 <- df2 %>% 
  select(SI_LATI, SI_LONG, biotope, admin) %>% 
  difference_left_join(prico, ., by = c("SI_LATI", "SI_LONG"), max_dist = 0.000001)

# Clean up the column names
names(prico2)
prico3 <- prico2 %>% 
  select(-c(SI_LATI.y, SI_LONG.y)) %>% 
  rename(SI_LATI = SI_LATI.x, SI_LONG = SI_LONG.x)

prico3 %>% count(REGION_SUB)
prico3 %>% count(biotope)
prico3 %>% count(REGION_NEW, REGION, REGION_SUB)

# Change column names
prico3 <- prico3 %>% 
  mutate(REGION = biotope, 
         REGION_SUB = biotope) %>% 
  select(-biotope) %>% 
  rename(JURISDICTION = REGION_NEW)
str(prico3)
prico3 %>% count(JURISDICTION, REGION, REGION_SUB)

# Plot
statDat <- prico3
my_jurisdiction <- "Puerto Rico"
unique(statDat$REGION)
statDat$REGION <- factor(statDat$REGION, 
                         levels = c("SWPRICO", "NPRICO", "EPRICO"))

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
d_prico <- statDat

