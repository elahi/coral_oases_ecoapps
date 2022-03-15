################################################################################
##' @title Explore reef types
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-14
##' @log Add a log here
################################################################################

#### PATHS #####
my_path <- "3_adjust_regions_2021/"
path_to_figs <- paste(my_path, "analysis_figs/", sep = "")
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
names(dat)

dat %>%
  filter(OCEAN == "CARIB") %>% 
  count(REGION, REEF_ZONE)

dat %>%
  filter(OCEAN == "CARIB") %>% 
  count(REGION, HABITAT) %>% 
  ggplot(aes(HABITAT, n, fill = REGION)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_flip() + 
  scale_fill_viridis_d()

ggsave(paste(path_to_figs,"HABITAT_CARIB.pdf", sep = ""), height = 7, width = 10)

dat %>%
  filter(OCEAN == "CARIB") %>% 
  count(REGION_SUB, HABITAT) %>% 
  group_by(REGION_SUB) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(REGION_SUB, prop, fill = HABITAT)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  guides(fill = guide_legend(nrow = 4,
                             override.aes = list(alpha = 1),
                             title.position = "top")) + 
  theme(legend.position = "bottom")

ggsave(paste(path_to_figs,"HABITAT_CARIB2.pdf", sep = ""), height = 7, width = 10)

##### PACIFIC #####
names(dat)
dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REEF_ZONE)

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, HABITAT)

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, HABITAT) %>% 
  ggplot(aes(HABITAT, n, fill = REGION)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_flip() + 
  scale_fill_viridis_d()

ggsave(paste(path_to_figs,"HABITAT_PACIFIC.pdf", sep = ""), height = 7, width = 10)

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REEF_ZONE) %>% 
  ggplot(aes(REEF_ZONE, n, fill = REGION)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_flip() + 
  scale_fill_viridis_d()

ggsave(paste(path_to_figs,"REEF_ZONE_PACIFIC.pdf", sep = ""), height = 7, width = 10)

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION, REGION_SUB, HABITAT) %>% 
  group_by(REGION, REGION_SUB) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(REGION_SUB, prop, fill = HABITAT)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_flip() + 
  facet_wrap(~ REGION, scales = "free") + 
  scale_fill_viridis_d() + 
  theme(legend.position = "bottom") + 
  labs(x = "") + 
  guides(fill = guide_legend(nrow = 1,
                            override.aes = list(alpha = 1),
                            title.position = "top"))

ggsave(paste(path_to_figs,"HABITAT_PACIFIC2.pdf", sep = ""), height = 7, width = 10)

dat %>%
  filter(OCEAN == "PACIFIC") %>% 
  count(REGION_SUB, REEF_ZONE) %>% 
  group_by(REGION_SUB) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(REGION_SUB, prop, fill = REEF_ZONE)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_flip() + 
  scale_fill_viridis_d() +
  guides(fill = guide_legend(nrow = 1,
                             override.aes = list(alpha = 1),
                             title.position = "top")) + 
  theme(legend.position = "bottom")
ggsave(paste(path_to_figs,"REEF_ZONE_PACIFIC2.pdf", sep = ""), height = 7, width = 10)


