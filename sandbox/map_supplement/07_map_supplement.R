################################################################################
##' @title Supplemental maps of all regions
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2022-02-12
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
path_to_figs <- paste(my_path, "map_cell_probs/", sep = "")

my_threshold <- "z2_"
this_model <- "occu_hier_binom"

##### PACKAGES, DATA #####
library(tidyverse)
library(viridis) 
library(patchwork)
library(sf)
library(ggspatial)
source("R/get_base_map.R")
source("R/renaming_functions.R")
source("R/ggplot_settings.R")

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

# Psi, with error; zed2
my_scale <- "zed2_"
d <- read_csv(file = paste(my_path,"model_output/model_summaries/", 
                           my_threshold, my_scale, this_model, "_", "psi_coefs_prob_0.5_df", ".csv", sep = ""))

###### FUNCTIONS ######

##' Function to select a gridded dataset according to:
##' scale
##' and then subset according to:
##' my_subregion
get_grid_subregion <- function(my_subregion = "Keys_middle", scale = "zed2", d = d){
  
  #### Select a gridded dataset
  if(scale == "zed0"){
    dat <- read.csv("workspace/grid_df_w0_psi.csv")}
  if(scale == "zed1"){
    dat <- read.csv("workspace/grid_df_w1_psi.csv")}
  if(scale == "zed2"){
    dat <- read.csv("workspace/grid_df_w2_psi.csv")}
  if(scale == "zed3"){
    dat <- read.csv("workspace/grid_df_w3_psi.csv")}
  
  #### Add the uncertainty
  d2 <- d %>% 
    #filter(.width == 0.80) %>% 
    select(.value, .lower, .upper)
  dat <- cbind(dat, d2)
  
  #### Subset oasis data
  statDat <- dat[dat$REGION %in% my_subregion, ] %>% droplevels() 
  
  return(statDat)
}

##### FLORIDA (EXCEPT SEFCRI) #####

my_subregions <- c("Keys_upper", "Keys_middle", "Keys_lower", "Tortugas")

# Get gridded subregions
statDat <- get_grid_subregion(my_subregion = my_subregions, scale = "zed2", d = d)

# Get basemap
my_basemap <- get_basemap(x = statDat, high_res = T, range_extension = 0.5, 
                          latitude = "lat_bin_2.5", longitude = "long_bin_2.5")

## Get subregion borders
statDat <- statDat %>% 
  rename_oasis_data()

long_summary <- statDat %>% 
  group_by(Subregion) %>%
  summarise(min_long = min(long_bin_2.5), 
            max_long = max(long_bin_2.5)) %>% 
  ungroup() %>% 
  gather(key = min_max, value = long, min_long:max_long)

# Remove left and right boundaries
long_summary
long_summary2 <- long_summary %>% 
  filter(Subregion != "Tortugas" & Subregion != "Keys-upper")

long_summary3 <- long_summary2[2:3, ] 
dist_difference <- (long_summary3$long[1] - long_summary3$long[2]) / 2

long_summary2
border_left <- long_summary2$long[1] - dist_difference
border_right <- long_summary2$long[4] + dist_difference
border_middle <- long_summary2$long[3] + dist_difference

# Get colors
# https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=4
col_psi <- "#008837"

# Base plot
p <- ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region), 
           fill = "black", alpha = 1, color = "gray", size = 0.2) + 
  geom_tile(data = statDat, alpha = 0.9, 
            aes(long_bin_2.5, lat_bin_2.5, fill = y_pred_prob), 
            inherit.aes = F, color = "black", lwd = 0.3) +
  scale_fill_gradient(low = "white", high = col_psi, limits = c(0, 1), 
                      breaks = seq(0, 1, by = 0.2)) + 
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(x = "", y = "", fill = expression(psi)) +
  scale_y_continuous(limits = c(24.4, 25.65)) + 
  guides(fill = guide_colorbar(ticks.colour = "black", 
                               ticks.linewidth = 1, 
                               frame.colour = "black", 
                               frame.linewidth = 1)) + 
  annotation_scale(location = "tl",style = "ticks", pad_y = unit(0.1, "in")) 

p

# Fine tune the map
p2 <- p + 
  theme(legend.box.margin = margin(0,0,0,0),
        legend.text = element_text(size = 8),
        legend.text.align = 1, 
        plot.margin = margin(0, 0, 0, 0, "cm"))

p2
