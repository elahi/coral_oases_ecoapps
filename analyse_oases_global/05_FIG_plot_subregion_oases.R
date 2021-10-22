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

theme_set(theme_bw(base_size = 14) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

this_model <- "occu_hier_binom"
z <- 2 # my z-score threshold

# Load this to get ordering file
source(paste(my_path, "05_FIG_plot_coefs_prob_psi.R", sep = ""))

##### CORAL COVER DATA #####
source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))

names(dat2)
dat2 <- rename_cover_data(dat2)
levels(dat2$Subregion)

dat3 <- fit_coefs_order_df %>%
  select(Subregion, variable2) %>%
  left_join(dat2, ., by = c("Subregion"))

levels(dat3$variable2)

summary3 <- dat3 %>% 
  group_by(Ocean, Region, Subregion, variable2) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd) %>% 
  ungroup()
summary3

dat3 %>%
  ggplot(aes(y = variable2, x = cover_prop*100, fill = Region)) +
  geom_jitter(alpha = 0.5, height = 0.1, pch = 21) + 
  labs(x = "Coral cover (%)", y = "") + 
  #facet_wrap(~ scale, ncol = 4) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  scale_fill_brewer(type = "div", palette = "RdYlBu", direction = 1) + 
  geom_point(aes(y = variable2, x = z_threshold), data = summary3, 
             size = 2, color = "black", pch = 15) +  
  geom_hline(yintercept = c(4.5, 7.5, 12.5, 17.5, 21.5, 29.5), color = "black", size = 0.25) 

ggsave(paste(path_to_figs, "FIG_plot_subregion_oases.pdf", 
             sep = ""), height = 8, width = 8)



## Get summary stats
summary0 <- dat3 %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

summary1 <- dat3 %>% 
  group_by(Ocean) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

ocean_thresholds <- summary1$z_threshold

summary2 <- dat3 %>% 
  group_by(Ocean, Jurisdiction) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

summary3 <- dat3 %>% 
  group_by(Ocean, Jurisdiction, Subregion) %>% 
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

