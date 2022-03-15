################################################################################
##' @title Summarize coral cover
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-12-13
##' @log 2020-12-06: included non-scleractinian species for total coral cover
################################################################################

#### LOAD PACKAGES, DATA ####
library(readxl)

## Load cover data
source("1_process_noaa/2_process_NOAA_cover.R")

## Compiled Pacific and Carib data in long format
head(cover)
names(cover)

## Load species meta-data
spp <- read_excel("data_output/cover_species_list.xlsx") %>% 
  select(species, OCEAN, fg)
spp
spp_corals <- spp %>% 
  filter(fg == "coral") %>% 
  arrange(OCEAN, species)

## Join coral species with cover
cover2 <- inner_join(cover, spp_corals, by = c("species", "OCEAN"))

unique(cover$species)
unique(cover2$species)
summary(cover2)
glimpse(cover2)

#### SUMMARISE CORAL COVER BY SITE ####

cover2 %>% count(OCEAN, HABITAT) %>% arrange(n)
names(cover2)

cover_coral <- cover2 %>% 
  select(-X1) %>% 
  group_by(SITE, REGION, REGION_SUB, YEAR, DATE, 
           SI_LATI, SI_LONG, DEPTH, HABITAT, REEF_ZONE, OCEAN, ll_id, 
           long_bin_2.5, lat_bin_2.5) %>% 
  summarise(coral_cover = sum(cover, na.rm = TRUE)) %>% 
  ungroup()

cover_coral %>% count(OCEAN, REGION, REGION_SUB, long_bin_2.5, lat_bin_2.5)

write.csv(cover_coral, "workspace/coral_total_cover.csv")

cover_coral %>% count(YEAR) %>% print(n = 100)
cover_coral %>% count(REGION, YEAR) %>% print(n = 100)
cover_coral %>% count(REGION, REGION_SUB, YEAR) %>% print(n = 100)

#### SUMMARISE CORAL COVER BY SPECIES ####

cover_species <- cover2 %>% 
  select(-X1) %>% 
  group_by(OCEAN, species) %>% 
  summarise(mean_cover = mean(cover, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(Ocean = OCEAN, Taxon = species) %>% 
  mutate(Ocean = as.factor(Ocean))

cover_species

cover_species$Ocean <- fct_recode(cover_species$Ocean, `Western Atlantic` = "CARIB", Pacific = "PACIFIC")

cover_species <- cover_species %>% 
  arrange(Ocean, desc(mean_cover)) %>% 
  mutate(mean_cover = round(mean_cover, digits = 2))

cover_species
write.csv(cover_species, "workspace/coral_species_cover.csv")
