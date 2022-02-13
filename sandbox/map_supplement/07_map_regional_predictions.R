################################################################################
##' @title Make gridded map of probabilities, Florida only
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-07-29
##' @log 2021-07-08: updated subjurisdictions
################################################################################

##### PATHS #####
my_path <- "00_analyse_oases_global/"
path_to_figs <- paste(my_path, "map_cell_probs/", sep = "")

my_threshold <- "z2_"
this_model <- "model3"

##### PACKAGES, DATA #####
library(tidyverse)
library(viridis) 
library(patchwork)
source("R/get_base_map.R")

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

## Get raw data
d <- read_csv("workspace/coral_total_cover_and_predictors_zed.csv")

###### FUNCTIONS ######

##' Function to take the raw site data and subset according to:
##' my_jurisdction
##' scale
get_oasis_df <- function(d = d, my_jurisdiction = "FLORIDA", scale = "zed2"){
  
  #### Subset oases
  d_sub <- d[d$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  
  #### Select scale
  if(scale == "zed0"){
    d_sub_oases <- d_sub %>% filter(oasis0 == "oasis")}
  if(scale == "zed1"){
    d_sub_oases <- d_sub %>% filter(oasis1 == "oasis")}
  if(scale == "zed2"){
    d_sub_oases <- d_sub %>% filter(oasis2 == "oasis")}
  if(scale == "zed3"){
    d_sub_oases <- d_sub %>% filter(oasis3 == "oasis")}
  
  return(d_sub_oases)
}

##' Function to select a gridded dataset according to:
##' scale
##' and then subset according to:
##' my_jurisdiction
get_grid_jurisdiction <- function(my_jurisdiction = "FLORIDA", scale = "zed2"){
  
  #### Select a gridded dataset
  if(scale == "zed0"){
    dat <- read.csv("workspace/grid_df_w0_ypred_medians.csv")}
  if(scale == "zed1"){
    dat <- read.csv("workspace/grid_df_w1_ypred_medians.csv")}
  if(scale == "zed2"){
    dat <- read.csv("workspace/grid_df_w2_ypred_medians.csv")}
  if(scale == "zed3"){
    dat <- read.csv("workspace/grid_df_w3_ypred_medians.csv")}
  
  #### Subset oasis data
  statDat <- dat[dat$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  
  return(statDat)
}

##' Function to take the subsetted oasis and gridded data and plot
map_panel <- function(statDat, d_sub_oases, my_basemap = my_basemap){
  
  p <- ggplot() +
    geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region), 
             fill = "gray", alpha = 0.2, color = "black", size = 0.2) + 
    geom_tile(data = statDat, 
              aes(long_bin_2.5, lat_bin_2.5, fill = y_pred_prob), 
              inherit.aes = F, color = "black", alpha = 0.8) +
    geom_point(data = d_sub_oases,
               aes(SI_LONG, SI_LATI), size = 0.75, 
               inherit.aes = F, color = "black", fill = "white", alpha = 1, pch = 21) +
    scale_fill_viridis_c(begin = 0.3, end = 1, limits = c(0, 1), 
                         breaks = seq(0, 1, by = 0.2)) + 
    guides(fill = guide_colorbar(reverse = TRUE, ticks.colour = "black")) +
    coord_equal() + 
    labs(x = "", y = "") 
  
  return(p)
}

##' Function to subset the data to get subregional maps
get_map_subregion <- function(oasis_df = oasis_df, statDat = statDat, my_region = "Tortugas"){
  
  # Subset datasets
  oasis_df_sub <- oasis_df[oasis_df$REGION %in% my_region, ] %>% droplevels() 
  statDat_sub <- statDat[statDat$REGION %in% my_region, ] %>% droplevels() 
  
  # Get basemap
  my_basemap <- get_basemap(x = statDat_sub, high_res = T, range_extension = 0.1, 
                            latitude = "lat_bin_2.5", longitude = "long_bin_2.5")
  
  # Get basic map
  p <- map_panel(statDat_sub, oasis_df_sub, my_basemap = my_basemap) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
    theme(legend.position = "none")
  
  return(p)
}

##### FLORIDA - gridded #####

my_jurisdiction <- "FLORIDA"
d_sub <- d[d$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 

# Get oasis sites
oasis_df <- get_oasis_df(d = d, my_jurisdiction = my_jurisdiction, scale = "zed2")

# Get gridded region
statDat <- get_grid_jurisdiction(my_jurisdiction = my_jurisdiction, scale = "zed2")

# Get basemap
my_basemap <- get_basemap(x = statDat, high_res = T, range_extension = 0.1, 
                          latitude = "lat_bin_2.5", longitude = "long_bin_2.5")

# Get basic map
p <- map_panel(statDat, oasis_df, my_basemap = my_basemap) +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

# Fine tune the map
p_region <- p + 
  theme(legend.position = c(0.15, 0.97),
        legend.box.margin = margin(0,0,0,0),
        legend.justification = c(0.9, 0.94), 
        legend.title = element_blank(),  
        legend.text = element_text(size = 8),
        legend.text.align = 1, 
        plot.margin = margin(0, 0, 0, 0, "cm"))

p_region

# Save it
ggsave(paste(path_to_figs, "map_regional_predictions.pdf", 
             "_", my_jurisdiction, ".pdf", sep = ""), 
       plot = p_region, height = 5, width = 6)

##### FLORIDA - sites #####

unique(statDat$REGION)
statDat$REGION <- factor(statDat$REGION, 
                         levels = rev(c("SEFCRI", "Keys_upper", "Keys_middle", "Keys_lower", 
                                        "Tortugas")))
d_sub$REGION <- factor(d_sub$REGION, 
                         levels = rev(c("SEFCRI", "Keys_upper", "Keys_middle", "Keys_lower", 
                                        "Tortugas")), 
                       labels = rev(c("Southeast Florida", "Upper Keys", "Middle Keys", "Lower Keys", 
                                  "Tortugas")))

# Create site map, on its own
ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region), 
           fill = "gray", alpha = 0.2, color = "black", size = 0.2) +
  geom_point(data = d_sub, alpha = 0.5, inherit.aes = F,
             aes(x = SI_LONG, y = SI_LATI, color = REGION)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(color = guide_legend(nrow = 1,
                            title = "Sub-region",
                            override.aes = list(alpha = 1),
                            title.position = "top")) +
  theme(legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(x = "Longitude", y = "Latitude") + 
  scale_color_viridis_d(option = "C") 

ggsave(paste(path_to_figs, "site_map", "_", my_jurisdiction, ".pdf", sep = ""), 
       height = 5, width = 6)



region_map <- ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region), 
           fill = "gray", alpha = 0.2, color = "black", size = 0.2) +
  geom_point(data = d_sub, alpha = 0.5, inherit.aes = F, 
             aes(x = SI_LONG, y = SI_LATI, color = REGION)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1,
                            title = "Sub-region",
                            override.aes = list(alpha = 1),
                            title.position = "top")) +
  theme(legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(x = "Longitude", y = "Latitude") + 
  scale_color_viridis_d() + 
  theme(legend.position = c(0.15, 0.97),
        legend.box.margin = margin(0,0,0,0),
        legend.justification = c(0.9, 0.94), 
        legend.title = element_blank(),  
        legend.text = element_text(size = 8),
        legend.text.align = 1, 
        plot.margin = margin(0, 0, 0, 0, "cm"))

region_map

##### SUBREGIONS WITHIN FLORIDA #####

statDat %>% count(REGION)

## Subregion
p <- get_map_subregion(oasis_df = oasis_df, statDat = statDat, my_region = "SEFCRI")
# Fine tune the map
p1 <- p + 
  ggtitle("Southeast Florida")
p1

## Subregion
p <- get_map_subregion(oasis_df = oasis_df, statDat = statDat, my_region = "Keys_upper")
# Fine tune the map
p2 <- p + 
  ggtitle("Upper Keys")
p2

## Subregion
p <- get_map_subregion(oasis_df = oasis_df, statDat = statDat, my_region = "Keys_middle")
# Fine tune the map
p3 <- p + 
  ggtitle("Middle Keys")
p3

## Subregion
p <- get_map_subregion(oasis_df = oasis_df, statDat = statDat, my_region = "Keys_lower")
# Fine tune the map
p4 <- p + 
  ggtitle("Lower Keys")
p4

## Subregion
p <- get_map_subregion(oasis_df = oasis_df, statDat = statDat, my_region = "Tortugas")
# Fine tune the map
p5 <- p + 
  ggtitle("Tortugas")
p5


###### COMBINE ######

region_map + plot_spacer() + p1 + p2 + p3 + p5 + p4 + 
  plot_layout(widths = c(1))

region_map + p1 + p2 + p5 + p4 + p3 + 
  plot_layout(ncol = 3)

p_all <- region_map + p1 + p2 + p3 + p4 + p5 + 
  plot_layout(guides = 'collect') 
p_all

ggsave(paste(path_to_figs, "map_subregional_predictions.pdf", 
             "_", my_jurisdiction, ".pdf", sep = ""), 
       plot = p_all, height = 5, width = 6)
