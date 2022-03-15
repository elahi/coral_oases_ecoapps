################################################################################
##' @title Explore the effects of scale on deciding oases
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-14
##' @log Add a log here
################################################################################

#### PATHS #####
my_path <- "3_adjust_regions_2021/"
spath_to_figs <- paste(my_path, "analysis_figs/", sep = "")
path_to_output <- paste(my_path, "analysis_output/", sep = "")

#### LOAD PACKAGES, DATA ####
source("R/get_base_map.R")
source("R/zed_by_group.R")
library(tidyverse)
library(viridis)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

dat <- read_csv("workspace/coral_total_cover_predictors.csv")

dat %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

dat %>% 
  count(REGION_NEW, YEAR) %>% print(n=100)

##### CARIBBEAN #####

dat %>%
  filter(OCEAN == "CARIB") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("CARIB")
statDat <- dat[dat$OCEAN %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = YEAR)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 1, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_c() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### FGBNMS #####
my_regions <- c("FGBNMS")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = YEAR)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  #facet_wrap(~ YEAR, nrow = 2) + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 1, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_c() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

statDat %>% 
  select(REGION, REGION_NEW, SI_LATI, SI_LONG) %>% 
  group_by(REGION, REGION_NEW) %>% 
  summarise_all(.funs = c("min", "max"))

##### USVI #####
my_regions <- c("USVI")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = YEAR)) +
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
  scale_color_viridis_c() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

statDat %>% 
  select(REGION, REGION_NEW, SI_LATI, SI_LONG) %>% 
  group_by(REGION, REGION_NEW) %>% 
  summarise_all(.funs = c("min", "max"))

##### FLORIDA #####
my_regions <- c("FLK", "SEFCRI", "Tortugas")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25,
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = REGION)) +
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
  scale_color_viridis_d() + 
  ggtitle(paste(my_regions, collapse = "_"))

ggsave(paste(path_to_figs, paste(my_regions, collapse = "_"), "_map.pdf", sep = ""), height = 7, width = 10)

##### FLK #####
my_regions <- c("FLK")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25,
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 2, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 2, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(paste(my_regions, collapse = "_"))

ggsave(paste(path_to_figs, paste(my_regions, collapse = "_"), "_map.pdf", sep = ""), height = 7, width = 10)

##### SEFCRI #####
my_regions <- c("SEFCRI")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25,
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "right") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 6, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 5, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(paste(my_regions, collapse = "_"))

ggsave(paste(path_to_figs, paste(my_regions, collapse = "_"), "_map.pdf", sep = ""), height = 7, width = 10)

##### Tortugas #####
my_regions <- c("Tortugas")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25,
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 2, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 2, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(paste(my_regions, collapse = "_"))

ggsave(paste(path_to_figs, paste(my_regions, collapse = "_"), "_map.pdf", sep = ""), height = 7, width = 10)

##### PRICO #####
my_regions <- c("PRICO")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25,
             aes(x = SI_LONG, y = SI_LATI, size = coral_cover, color = YEAR)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 2, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(x = "", y = "") + 
  scale_color_viridis_c() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, paste(my_regions, collapse = "_"), "_map.pdf", sep = ""), height = 7, width = 10)

##### PACIFIC #####

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("PACIFIC")
statDat <- dat[dat$OCEAN %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = F)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG_360, y = SI_LATI, color = REGION)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(shape = FALSE) + 
  guides(col = guide_legend(nrow = 1, 
                            #title = "Region-sub", 
                            override.aes = list(alpha = 1), 
                            title.position = "top"), 
         size = guide_legend(nrow = 1, 
                             title = "Coral (%)", 
                             title.position = "top")) + 
  theme(legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### MARIAN #####

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("MARIAN")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, #size = coral_cover, 
                 color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "right") + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  # guides(col = guide_legend(nrow = 1, 
  #                           #title = "Region-sub", 
  #                           override.aes = list(alpha = 1), 
  #                           title.position = "top"), 
  #        size = guide_legend(nrow = 1, 
  #                            title = "Coral (%)", 
  #                            title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### MHI #####

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("MHI")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, #size = coral_cover, 
                 color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  # guides(col = guide_legend(nrow = 1, 
  #                           #title = "Region-sub", 
  #                           override.aes = list(alpha = 1), 
  #                           title.position = "top"), 
  #        size = guide_legend(nrow = 1, 
  #                            title = "Coral (%)", 
  #                            title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### NWHI #####

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("NWHI")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, #size = coral_cover, 
                 color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  # guides(col = guide_legend(nrow = 1, 
  #                           #title = "Region-sub", 
  #                           override.aes = list(alpha = 1), 
  #                           title.position = "top"), 
  #        size = guide_legend(nrow = 1, 
  #                            title = "Coral (%)", 
  #                            title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### PRIAS #####

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("PRIAs")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = F)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG_360, y = SI_LATI, #size = coral_cover, 
                 color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "bottom") + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  # guides(col = guide_legend(nrow = 1, 
  #                           #title = "Region-sub", 
  #                           override.aes = list(alpha = 1), 
  #                           title.position = "top"), 
  #        size = guide_legend(nrow = 1, 
  #                            title = "Coral (%)", 
  #                            title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### SAMOA #####

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_NEW, REGION_SUB, YEAR) %>% print(n=100)

my_regions <- c("SAMOA")
statDat <- dat[dat$REGION %in% my_regions, ] %>% droplevels()
my_basemap <- get_basemap(x = statDat, high_res = T)

ggplot() +
  geom_map(data = my_basemap, map = my_basemap, mapping = aes(map_id = region)) +
  geom_point(data = statDat, alpha = 0.25, 
             aes(x = SI_LONG, y = SI_LATI, #size = coral_cover, 
                 color = REGION_SUB)) +
  coord_equal() +
  theme(legend.position = "right") + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  # guides(col = guide_legend(nrow = 1, 
  #                           #title = "Region-sub", 
  #                           override.aes = list(alpha = 1), 
  #                           title.position = "top"), 
  #        size = guide_legend(nrow = 1, 
  #                            title = "Coral (%)", 
  #                            title.position = "top")) + 
  # theme(legend.direction = "horizontal", 
  #       legend.box = "horizontal") + 
  labs(x = "", y = "") + 
  scale_color_viridis_d() + 
  ggtitle(my_regions)

ggsave(paste(path_to_figs, my_regions, "_map.pdf", sep = ""), height = 7, width = 10)

##### CALCULATE Z AND DESIGNATE OASES #####

dat <- get_zed_group(dat, group_var = YEAR, summary_var = cover_prop) 
hist(dat_meta$zed)

z_threshold <- 2

dat_meta <- dat_meta %>% 
  mutate(oasis = ifelse(zed > z_threshold, "oasis", "not_oasis"))

dat_meta %>% count(oasis) %>% 
  mutate(proportion = n / sum(n))

dat_meta %>% count(YEAR, oasis) %>% 
  mutate(proportion = n / sum(n))