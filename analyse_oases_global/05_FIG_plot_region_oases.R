################################################################################
##' @title Plot oases by jurisdiction/region and scale
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-08-28
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

source("R/renaming_functions.R")
source("R/ggplot_settings.R")

##### PACKAGES, DATA #####
library(tidyverse)
library(viridis)
library(forcats)
library(patchwork)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

this_model <- "occu_hier_binom"
z <- 2 # my z-score threshold

##### FUNCTION TO SUMMARISE OASES #####

summarise_oases <- function(dat, scale_text){
  oasis_n_df <- dat %>%  
    group_by(JURISDICTION) %>% 
    summarise(oasis_n = sum(oasis), 
              reef_n = sum(Ji), 
              oasis_prop = oasis_n / reef_n)
  
  oasis_n_df <- oasis_n_df %>% 
    mutate(Scale = scale_text)
}

##### APPLY FUNCTION TO ALL SCALES #####

dat <- read.csv("workspace/grid_df_w0.csv") 
oasis_0 <- summarise_oases(dat = dat, scale_text = "Cross-basin")

dat <- read.csv("workspace/grid_df_w1.csv") 
oasis_1 <- summarise_oases(dat = dat, scale_text = "Basin")

dat <- read.csv("workspace/grid_df_w2.csv") 
oasis_2 <- summarise_oases(dat = dat, scale_text = "Region")

##### COMPILE #####

oasis_df <- rbind(oasis_0, oasis_1, oasis_2)
 
## Reorder jurisdiction
oasis_df <- oasis_df %>%
  mutate(scale = factor(Scale, levels = c("Cross-basin", "Basin", "Region")))

## Reorder variables
oasis_df <- oasis_df %>%
  group_by(scale) %>% 
  mutate(Jurisdiction = fct_reorder(JURISDICTION, oasis_prop)) %>% 
  ungroup()
levels(oasis_df$Jurisdiction)

## Rename variables
levels(oasis_df$Jurisdiction) <- c("US Virgin Islands", "Puerto Rico", 
                                   "Florida", "Mariana Islands", 
                                   "Northwest\nHawaiian Islands", 
                                   "Main\nHawaiian Islands", 
                                   "American Samoa")

oasis_df %>% 
  group_by(Scale) %>% 
  summarise(sum(reef_n))

##### CORAL COVER DATA #####
source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))

## Make factors
dat2 <- dat2 %>%
  mutate(Jurisdiction = fct_reorder(JURISDICTION, cover_prop), 
         Ocean = as.factor(OCEAN)) %>% 
  ungroup()
levels(dat2$Jurisdiction)

## Rename variables
levels(dat2$Jurisdiction) <- c("Florida",
                               "US Virgin Islands", 
                               "Northwest\nHawaiian Islands",
                               "Puerto Rico", 
                               "Main\nHawaiian Islands", 
                               "Mariana Islands", 
                               "American Samoa")
levels(dat2$Ocean) <- c("Atlantic", "Pacific")

levels(dat2$Jurisdiction)

## Relevel variables to match previous plot
dat2$Jurisdiction <- fct_relevel(dat2$Jurisdiction, levels(oasis_df$Jurisdiction))

## Get summary stats
summary0 <- dat2 %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

summary1 <- dat2 %>% 
  group_by(Ocean) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

ocean_thresholds <- summary1$z_threshold

summary2 <- dat2 %>% 
  group_by(Ocean, Jurisdiction) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

## Ocean colors
ocean_pal <- c("#91bfdb", "#fc8d59")

##### MAKE PLOTS #####

## Plot
p_cover <- dat2 %>%
  ggplot(aes(Jurisdiction, cover_prop*100, color = Ocean)) + 
  geom_jitter(alpha = 0.25, width = 0.1) + 
  coord_flip() +
  labs(x = "Region", 
       y = "Coral cover (%)", 
       title = "A") + 
  theme(#legend.position = c(0.95, 0.05),
        legend.position = "none", 
        legend.box.background = element_rect(fill = "white", color = "grey80"),
        legend.box.margin = margin(2,2,2,2),
        legend.justification = c(0.95, 0.05), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8)) +
  geom_hline(aes(yintercept = z_threshold), data = summary0, size = 1, 
             color = "gray50") +
  geom_segment(aes(x = 0.5, y = ocean_thresholds[1], xend = 3.5, yend = ocean_thresholds[1]), 
               color = ocean_pal[1], size = 1) +
  geom_segment(aes(x = 3.5, y = ocean_thresholds[2], xend = 7.5, yend = ocean_thresholds[2]), 
               size = 1, color = ocean_pal[2]) +
  geom_point(aes(Jurisdiction, z_threshold, fill = Ocean), data = summary2, 
             size = 1.5, color = "black", pch = 15) +  
  scale_color_manual(values = ocean_pal) +  
  scale_fill_manual(values = ocean_pal) +
  guides(fill = guide_legend(override.aes = list(size = 3)))

p_cover

p_oases <- oasis_df %>% 
  ggplot(aes(Jurisdiction, oasis_prop*100, fill = scale, size = reef_n)) + 
  geom_point(pch = 21) + 
  labs(x = "Region", 
       y = "Percent of sites classified as oases", 
       title = "B") + 
  coord_flip() + 
  theme(#legend.position = c(0.95, 0.05),
    legend.position = "right", 
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(), 
    #legend.box.background = element_rect(fill = "white", color = "grey80"),
    #legend.box.margin = margin(2,2,2,2),
    legend.justification = c(0.95, 0.05)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey80', linetype='dashed')) + 
  scale_size_area() + 
  scale_fill_viridis_d() + 
  guides(fill = guide_legend(override.aes = list(size = 4), title = "Spatial extent"), 
         size = guide_legend(title = "N (sites)"))

p_oases

##### COMPILE #####

p_cover + p_oases + plot_layout(nrow = 1)
ggsave(paste(path_to_figs, "FIG_plot_region_oases.pdf", 
             sep = ""), height = 3, width = 7)

