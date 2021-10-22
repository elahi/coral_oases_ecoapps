################################################################################
##' @title Inspect NWHI for oases
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-09-15
##' @log  
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES #####
library(tidyverse)
library(forcats)

theme_set(theme_bw(base_size = 11) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))
source("R/oasis_functions.R")

##### INSPECT #####

d <- dat2 %>% 
  filter(JURISDICTION == "NWHI")

d %>% 
  ggplot(aes(x = REGION, y = cover_prop, color = oasis0)) + 
  geom_jitter(width = 0.1, alpha = 0.5)

d_long <- d %>% 
  gather(key = oasis_scale, value = oasis, oasis0:oasis3)

z <- 2
region_df <- d_long %>% 
  filter(oasis_scale == "oasis3") %>% 
  group_by(REGION, oasis_scale) %>% 
  summarise(mean = mean(coral_cover), 
            sd = sd(coral_cover), 
            z_threshold = mean + z*sd)

d_long %>% 
  ggplot(aes(x = REGION, y = coral_cover, color = oasis)) + 
  # stat_summary(aes(color = NULL), fun = mean, fun.min = mean, fun.max = mean,
  #              geom = "crossbar", width = 0.5) + 
  geom_point(data = region_df, aes(REGION, y = z_threshold), inherit.aes = FALSE, pch = 3) + 
  geom_jitter(width = 0.1, alpha = 0.5) + 
  facet_wrap(~ oasis_scale)

## Save plot for zed3

d %>% 
  ggplot(aes(x = REGION, y = coral_cover, fill = oasis3)) + 
  geom_point(data = region_df, aes(REGION, mean), inherit.aes = FALSE, size = 3) + 
  geom_errorbar(data = region_df, aes(x = REGION, 
                                      ymin = mean - z*sd, 
                                      ymax = z_threshold), inherit.aes = FALSE, 
                width = 0.4, ) + 
  geom_jitter(width = 0.1, alpha = 0.7, pch = 21) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(x = "Sub-region", y = "Coral cover (%)") + 
  guides(fill = FALSE) + 
  scale_fill_brewer(palette = "Set1") 

ggsave(paste(path_to_figs, "nwhi_oases.pdf", 
             sep = ""), height = 3.5, width = 4)


