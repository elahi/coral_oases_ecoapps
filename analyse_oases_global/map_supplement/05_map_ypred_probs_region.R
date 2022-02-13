################################################################################
##' @title Make gridded map of probabilities, region
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
    theme(legend.position = c(0.97, 0.97),
          legend.box.margin = margin(0,0,0,0),
          legend.justification = c(0.9, 0.94), 
          legend.title = element_blank(),  
          legend.text = element_text(size = 8),
          legend.text.align = 1, 
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    labs(x = "", y = "") 
  
  return(p)
}

map_regional_predictions <- function(d = d, my_jurisdiction = "USVI", my_extension = 0.1){
  
  #### Subset oases
  d_sub <- d[d$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  d_sub_oases0 <- d_sub %>% filter(oasis0 == "oasis")
  d_sub_oases1 <- d_sub %>% filter(oasis1 == "oasis")
  d_sub_oases2 <- d_sub %>%  filter(oasis2 == "oasis")
  d_sub_oases3 <- d_sub %>%  filter(oasis3 == "oasis")
  
  #### Prepara base map
  ## Get oasis data with predictions, and model fit
  dat <- read.csv("workspace/grid_df_w0_ypred_medians.csv")
  # Subset oasis data
  statDat <- dat[dat$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  # Get basemap
  my_basemap <- get_basemap(x = statDat, high_res = T, range_extension = my_extension, 
                            latitude = "lat_bin_2.5", longitude = "long_bin_2.5")


  ##### ZED 0 #####
  dat <- read.csv("workspace/grid_df_w0_ypred_medians.csv")
  # Subset oasis data
  statDat <- dat[dat$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  
  p0 <- map_panel(statDat, d_sub_oases0, my_basemap = my_basemap) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    ggtitle("A. Cross-basin")
  
  ##### ZED 1 #####
  dat <- read.csv("workspace/grid_df_w1_ypred_medians.csv")
  # Subset oasis data
  statDat <- dat[dat$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  
  p1 <- map_panel(statDat, d_sub_oases1, my_basemap = my_basemap) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    ggtitle("B. Basin")
  
  ##### ZED 2 #####
  
  ## Get oasis data with predictions, and model fit
  dat <- read.csv("workspace/grid_df_w2_ypred_medians.csv")
  # Subset oasis data
  statDat <- dat[dat$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  
  p2 <- map_panel(statDat, d_sub_oases2, my_basemap = my_basemap) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    ggtitle("C. Region")
  
  # ##### ZED 3 #####
  # 
  # ## Get oasis data with predictions, and model fit
  # dat <- read.csv("workspace/grid_df_w3_ypred_medians.csv")
  # # Subset oasis data
  # statDat <- dat[dat$JURISDICTION %in% my_jurisdiction, ] %>% droplevels() 
  # 
  # if(dim(statDat)[1] > 0){
  #   p3 <- map_panel(statDat, d_sub_oases3, my_basemap = my_basemap) +
  #     theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  #     ggtitle("D. Sub-region")
  # }
  # 
  # if(dim(statDat)[1] == 0){
  #   p3 <- plot_spacer()
  # }
  
  ##### COMPILE #####
  
  p <- p0 + p1 + p2  +
    plot_layout(guides = 'collect') 

  ggsave(paste(path_to_figs, my_jurisdiction, "_",
               "_", "grid_ypred_oases.pdf", sep = ""), p)

}

# Create sample area groups
d <- d %>% mutate(group_region = as.integer(as.factor(JURISDICTION)))
dat_groups <- d %>% 
  select(JURISDICTION, group_region) %>% distinct() %>% arrange(group_region)
dat_groups

# # Remove problems
# problems <- c("French Frigate", "Pearl & Hermes", "Rose", "Swains")
# dat_groups <- dat_groups[!(dat_groups$REGION %in% problems), ]

# Loop
for(i in 1:length(dat_groups$group_region)){ 
  print(i); print(dat_groups$JURISDICTION[i])
  map_regional_predictions(d = d, my_jurisdiction = dat_groups$JURISDICTION[i])
}


# 
# # troubleshooting
# dat <- read.csv("workspace/grid_df_w0_ypred_medians.csv")
# dat2 <- dat[dat$REGION %in% problems, ] 
# dat2 <- dat[dat$REGION %in% "French Frigate", ]
# 
# dat2 %>% count(JURISDICTION, REGION)
# summary(dat2)
# my_basemap <- get_basemap(x = dat2, high_res = T, range_extension = 1.4,
#                           latitude = "lat_bin_2.5", longitude = "long_bin_2.5")
# my_basemap
# head(my_basemap)
# my_basemap %>% count(region)
# 
# ggplot(mapping = aes(x = long, y = lat)) +
#   geom_map(data = my_basemap, map = my_basemap, 
#            mapping = aes(map_id = region), 
#            fill = "gray", alpha = 0.2, color = "black", size = 0.2) + 
#   coord_equal() 
