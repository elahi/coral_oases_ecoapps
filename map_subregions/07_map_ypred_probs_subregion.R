################################################################################
##' @title Make gridded map of probabilities, subregion
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2022-02-12
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
path_to_figs <- paste(my_path, "map_subregions/", sep = "")

my_threshold <- "z2_"
this_model <- "model3"

##### PACKAGES, DATA #####
library(tidyverse)
library(viridis) 
library(patchwork)
library(ggspatial)

source("R/get_base_map.R")

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

# Create sample area groups
df_samples <- read.csv("data_output/df_samples.csv")
dat_groups <- df_samples %>% 
  mutate(group_j = as.integer(as.factor(REGION))) %>% 
  arrange(group_j) %>% 
  select(JURISDICTION, REGION, Region, Subregion, group_j)

###### FUNCTIONS ######

col_psi <- "#008837"

map_panel <- function(statDat, my_basemap = my_basemap){
  
  p <- ggplot() +
    geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region), 
             fill = "black", alpha = 1, color = "gray", size = 0.2) + 
    geom_tile(data = statDat, alpha = 0.9,
              aes(long_bin_2.5, lat_bin_2.5, fill = y_pred_prob),
              inherit.aes = F, color = "black", lwd = 0.3) +
    # geom_tile(data = statDat, alpha = 0.9, 
    #           aes(round(long_bin_2.5, 4), round(lat_bin_2.5, 4), fill = y_pred_prob), 
    #           inherit.aes = F, color = "black", lwd = 0.3) + # FOR LANAI ONLY
    scale_fill_gradient(low = "white", high = col_psi, limits = c(0, 1), 
                        breaks = seq(0, 1, by = 0.2)) + 
    theme(axis.title = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5), 
          axis.text.x = element_text(hjust = 0.5)) +
    labs(x = "", y = "", fill = expression(psi)) + 
    coord_equal() + 
    guides(fill = guide_colorbar(ticks.colour = "black", 
                                 ticks.linewidth = 1, 
                                 frame.colour = "black", 
                                 frame.linewidth = 1))
  
  return(p)
}

map_predictions <- function(my_region = "STT", my_extension = 0.1, 
                            df_groups = dat_groups){
  
  #### Prepare a base map
  ## Get oasis data with predictions, and model fit
  dat <- read.csv("workspace/grid_df_w0_psi.csv")
  
  # Subset oasis data
  statDat <- dat[dat$REGION %in% my_region, ] %>% droplevels() 
  
  # Get basemap
  my_basemap <- get_basemap(x = statDat, high_res = T, range_extension = my_extension, 
                            latitude = "lat_bin_2.5", longitude = "long_bin_2.5")

  ##### ZED 0 #####
  dat <- read.csv("workspace/grid_df_w0_psi.csv")
  # Subset oasis data
  statDat <- dat[dat$REGION %in% my_region, ] %>% droplevels() 
  
  p0 <- map_panel(statDat, my_basemap = my_basemap) +
    ggtitle("A. Cross-basin")
  
  ##### ZED 1 #####
  dat <- read.csv("workspace/grid_df_w1_psi.csv")
  # Subset oasis data
  statDat <- dat[dat$REGION %in% my_region, ] %>% droplevels() 
  
  p1 <- map_panel(statDat, my_basemap = my_basemap) +
    ggtitle("B. Basin")
  
  ##### ZED 2 #####
  
  ## Get oasis data with predictions, and model fit
  dat <- read.csv("workspace/grid_df_w2_psi.csv")
  # Subset oasis data
  statDat <- dat[dat$REGION %in% my_region, ] %>% droplevels() 
  
  p2 <- map_panel(statDat, my_basemap = my_basemap) +
    ggtitle("C. Region")
  
  ##### ZED 3 #####
  
  ## Get oasis data with predictions, and model fit
  dat <- read.csv("workspace/grid_df_w3_psi.csv")
  # Subset oasis data
  statDat <- dat[dat$REGION %in% my_region, ] %>% droplevels() 
  
  if(dim(statDat)[1] > 0){
    p3 <- map_panel(statDat, my_basemap = my_basemap) +
      ggtitle("D. Sub-region")
  }
  
  if(dim(statDat)[1] == 0){
    p3 <- plot_spacer()
  }
  
  ##### COMPILE #####
  
  dat_groups_i <- dat_groups[dat_groups$REGION %in% my_region, ]
  
  # Get region
  my_region_title <- unique(dat_groups_i$Region)
  my_region_file <- unique(dat_groups_i$JURISDICTION)
  
  # Get subregion
  my_subregion_title <- unique(dat_groups_i$Subregion)
  
  # Collect plots
  p <- p0 + p1 + p2 + p3 +
    plot_layout(guides = 'collect') +
    plot_annotation(title = paste("Region: ", my_region_title, "; ", 
                                  "Subregion: ", my_subregion_title, sep = ""))

  ggsave(paste(path_to_figs, my_region_file, "_",
               my_region, "_", "grid_ypred_oases.pdf", sep = ""), p, 
         width = 8.5*0.8, height = 11*0.8)

}

# test
i = 1
map_predictions(my_region = dat_groups$REGION[i])


##### LOOP THROUGH PLOTS #####

# Remove problems
problems <- c("French Frigate", "Pearl & Hermes", "Rose")
dat_problems <- dat_groups[(dat_groups$REGION %in% problems), ]
dat_groups2 <- dat_groups[!(dat_groups$REGION %in% problems), ]

# # Loop all
# for(i in 1:length(dat_groups$group_j)){ 
#   print(i); print(dat_groups$REGION[i])
#   map_predictions(my_region = dat_groups$REGION[i])
# }

# Loop all but problems
for(i in 1:length(dat_groups2$group_j)){ 
  print(i); print(dat_groups2$REGION[i])
  map_predictions(my_region = dat_groups2$REGION[i])
}

# Loop problems
for(i in 1:length(dat_problems$group_j)){ 
  print(i); print(dat_problems$REGION[i])
  map_predictions(my_region = dat_problems$REGION[i], my_extension = 1.5)
}


##### TROUBLESHOOTING #####

# Map Lanai
map_predictions(my_region = "Lanai", my_extension = 3)

dat <- read.csv("workspace/grid_df_w0_psi.csv")
# Subset oasis data
dat2 <- dat[dat$REGION %in% "Lanai", ]
#dat2 <- dat[dat$REGION %in% "Kauai", ]

dat2 %>% count(JURISDICTION, REGION)
summary(dat2)
my_basemap <- get_basemap(x = dat2, high_res = T, range_extension = 0.1,
                          latitude = "lat_bin_2.5", longitude = "long_bin_2.5")
my_basemap
head(my_basemap)
my_basemap %>% count(region)

# This rounding hack fixed the problem for Lanai
ggplot(mapping = aes(x = long, y = lat)) +
  geom_map(data = my_basemap, map = my_basemap,
           mapping = aes(map_id = region),
           fill = "gray", alpha = 0.2, color = "black", size = 0.2) +
  coord_equal() +
  geom_tile(data = dat2, alpha = 0.9, 
            aes(round(long_bin_2.5, 4), round(lat_bin_2.5, 4), fill = y_pred_prob), 
            inherit.aes = F, color = "black", lwd = 0.3) +
  geom_point(data = dat2, alpha = 0.9, 
            aes(long_bin_2.5, lat_bin_2.5), 
            inherit.aes = F, color = "black", lwd = 0.3) +
  scale_fill_gradient(low = "white", high = col_psi, limits = c(0, 1), 
                      breaks = seq(0, 1, by = 0.2)) 

##### LANAI #####

# Comment out in map_panel
map_predictions(my_region = "Lanai")
