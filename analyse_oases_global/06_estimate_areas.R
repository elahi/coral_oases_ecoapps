################################################################################
##' @title Estimate areas at each spatial scale
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-07-16
##' @log Add a log here
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
library(tidyverse)
library(sf)
library(patchwork)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

## Get raw data
source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))
d <- dat2 %>% 
  mutate(lon = SI_LONG, lat = SI_LATI)
d %>% count(REGION_SUB) %>% arrange(n)

## Get oasis data with predictions, and model fit
dat <- read.csv("workspace/grid_df_w2_psi.csv") %>% 
  mutate(lon = long_bin_2.5, lat = lat_bin_2.5)

dat %>% count(OCEAN) %>% arrange(n) 

# For Caribbean
my_region <- c("CARIB")

## Subset data
statDat <- dat[dat$OCEAN %in% my_region, ] %>% droplevels() 
d_sub <- d[d$OCEAN %in% my_region, ] %>% droplevels() 

##### WRITE FUNCTION #####

## Make polygon based on lat-longs
dat_polygon <- statDat %>%
  st_as_sf(coords = c("long_bin_2.5", "lat_bin_2.5"), crs = 4326) %>% 
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_convex_hull(.) 
plot(dat_polygon)
area_km2 <- as.numeric(st_area(dat_polygon)) / 1e6

get_area_km2 <- function(d){
  dat_polygon <- d %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>% 
    st_convex_hull(.) 
  area_km2 <- tibble(area_km2 = as.numeric(st_area(dat_polygon)) / 1e6)
  return(area_km2)
}

test_dat <- d_sub %>% 
  mutate(lon = SI_LONG, lat = SI_LATI)
get_area_km2(d = test_dat)

test_dat <- statDat %>% 
  mutate(lon = long_bin_2.5, lat = lat_bin_2.5)
get_area_km2(d = test_dat)

##### APPLY FUNCTION TO GRIDDED DATA #####

area_ocean <- dat %>% 
  group_by(OCEAN) %>%
  do(get_area_km2(.))

area_jurisdiction <- dat %>% 
  group_by(OCEAN, JURISDICTION) %>%
  do(get_area_km2(.))

area_jurisdiction <- dat %>% 
  group_by(OCEAN, JURISDICTION, REGION) %>%
  do(get_area_km2(.))

dat %>% count(OCEAN, JURISDICTION, REGION)


##### APPLY FUNCTION TO RAW DATA #####

area_ocean <- d %>% 
  group_by(OCEAN) %>%
  do(get_area_km2(.)) %>% 
  ungroup() %>% 
  mutate(JURISDICTION = NA, 
         REGION = NA)

area_jurisdiction <- d %>% 
  group_by(OCEAN, JURISDICTION) %>%
  do(get_area_km2(.)) %>% 
  ungroup() %>% 
  mutate(REGION = NA)

area_subjurisdiction <- d %>% 
  group_by(OCEAN, JURISDICTION, REGION) %>%
  do(get_area_km2(.)) %>% 
  ungroup()

area_ocean
area_jurisdiction %>% summary()
area_subjurisdiction %>% summary()

area_df <- rbind(area_ocean, area_jurisdiction, area_subjurisdiction) %>% 
  arrange(OCEAN, JURISDICTION, desc(area_km2))
write.csv(area_df, paste(path_to_output, "estimated_areas.csv", sep = ""))  


p1 <- area_ocean %>% 
  ggplot(aes("", area_km2 / 1000, color = OCEAN)) + 
  geom_jitter(width = 0.02, alpha = 0.5) + 
  theme(legend.position = "none") +
  ggtitle("A. Ocean") + 
  labs(x = "", y = expression(paste("Area (", km^2, " x 1000)", sep = "")))
p1

p2 <- area_jurisdiction %>% 
  ggplot(aes("", area_km2 / 1000, color = OCEAN)) + 
  geom_jitter(width = 0.02, alpha = 0.5) + 
  theme(legend.position = "none") +
  ggtitle("B. Jurisdiction") +  
  labs(x = "", y = "")

p3 <- area_subjurisdiction %>% 
  ggplot(aes("", area_km2 / 1000, color = OCEAN)) + 
  geom_jitter(width = 0.02, alpha = 0.5) + 
  labs(x = "", y = "") +
  ggtitle("C. Sub-jurisdiction") +  
  theme(legend.position = "none") 
  

p1 + p2 + p3 +
  plot_layout(guides = 'collect') 

ggsave(paste(path_to_figs, "estimate_areas.pdf", sep = ""), height = 3, width = 7)

