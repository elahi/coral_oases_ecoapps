################################################################################
##' @title Supplemental maps of all regions
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2022-02-12
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
my_path <- "supplemental_maps/"
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

##### FLORIDA LAYERS #####

st_layers("kml/florida/National_Parks/National_Parks.kml")
st_layers("kml/florida/State_Parks/State_Parks.kml")
st_layers("kml/florida/FKNMS_Boundary/FKNMS_Boundary.kml")

fnp_polygon <- st_read("kml/florida/National_Parks/National_Parks.kml", layer = "Features")
fsp_polygon <- st_read("kml/florida/State_Parks/State_Parks.kml", layer = "Features")
fnms_polygon <- st_read("kml/florida/FKNMS_Boundary/FKNMS_Boundary.kml", layer = "Features")

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
col_fnms <- "#9e9ac8"
col_fsp <- "#6a51a3"
col_fnp <- "#cbc9e2"

# Base plot
p <- ggplot() +
  geom_sf(data = fnp_polygon, color = col_fnp, fill = col_fnp, 
          size = 0.1, alpha = 1) +
  geom_sf(data = fnms_polygon, color = col_fnms, fill = col_fnms, 
          size = 0.1, alpha = 1) +
  geom_sf(data = fsp_polygon, color = col_fsp, fill = col_fsp, 
          size = 0.1, alpha = 1) +
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
  # annotation_north_arrow(width = unit(.3,"in"), 
  #                        pad_y = unit(.3, "in"),
  #                        location = "tl", 
  #                        which_north = "true") + 
  annotation_scale(location = "tl",style = "ticks", pad_y = unit(0.1, "in")) 

p

p2 <- p + 
  #geom_vline(aes(xintercept = long), data = long_summary2) + 
  geom_vline(aes(xintercept = border_left), linetype = "dashed") + 
  geom_vline(aes(xintercept = border_right), linetype = "dashed") + 
  geom_vline(aes(xintercept = border_middle), linetype = "dashed") + 
  geom_text(aes(x = -82.7, y = 25, label = "Tortugas"), size = 4) + 
  geom_text(aes(x = -81.65, y = 25, label = "Lower Keys"), size = 4) + 
  geom_text(aes(x = -80.9, y = 24.45, label = "Middle Keys"), size = 4) + 
  geom_text(aes(x = -80.2, y = 24.5, label = "Upper\nKeys"), size = 4) + 
  geom_text(aes(x = -80.8, y = 25.6, label = "Florida"), size = 5, color = "white")

# Fine tune the map
p3 <- p2 + 
  theme(legend.box.margin = margin(0,0,0,0),
        legend.text = element_text(size = 8),
        legend.text.align = 1, 
        plot.margin = margin(0, 0, 0, 0, "cm"))

p3

p_map <- p3

#### COVARIATES ####

names(statDat)

# Reorder to match map
statDat2 <- statDat
levels(statDat2$Subregion)
statDat2$Subregion <- fct_relevel(statDat2$Subregion, "Tortugas", "Keys-lower")
statDat2$Subregion <- fct_recode(statDat2$Subregion,
                                 "Lower Keys" = "Keys-lower", 
                                 "Middle Keys" = "Keys-middle", 
                                 "Upper Keys" = "Keys-upper")

pB <- statDat2 %>% 
  ggplot(aes(kd490_mean, y_pred_prob, fill = Subregion, color = Subregion)) + 
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, color = NULL), alpha = 0.2)  + 
  geom_errorbar(aes(ymin = .lower, ymax = .upper), alpha = 0.2)  + 
  #geom_smooth(se = FALSE)  + 
  geom_point(pch = 21, color = "black", alpha = 0.7) + 
  scale_fill_viridis_d(direction = -1) + 
  scale_color_viridis_d(direction = -1) + 
  labs(x = "Light attenuation (mean)", 
       y = expression("Oasis occurrence"~(psi))) + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
  coord_cartesian(ylim = c(0, 1))
pB

pC <- statDat2 %>% 
  ggplot(aes(sst_cv, y_pred_prob, fill = Subregion, color = Subregion)) + 
  #geom_ribbon(aes(ymin = .lower, ymax = .upper, color = NULL), alpha = 0.2)  + 
  geom_errorbar(aes(ymin = .lower, ymax = .upper), alpha = 0.2)  + 
  #geom_smooth(se = FALSE)  + 
  geom_point(pch = 21, color = "black", alpha = 0.7) + 
  scale_fill_viridis_d(direction = -1) + 
  scale_color_viridis_d(direction = -1) + 
  labs(x = "Sea surface temperature (CV)", 
       y = expression("Oasis occurrence"~(psi))) + 
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
  coord_cartesian(ylim = c(0, 1))

p_bottom <- pB + pC + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", 
        legend.title = element_blank())
p_bottom

p_final <- p_map / p_bottom + 
  plot_layout(heights = c(1, 1)) + 
  plot_annotation(tag_levels = "A")

p_final

ggsave(paste(path_to_figs, "","map_predictions_covariates", 
             "_", "florida2", ".pdf", sep = ""), 
       plot = p_final, height = 6, width = 6)

