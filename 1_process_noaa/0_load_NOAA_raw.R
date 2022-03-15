################################################################################
##' @title Load raw NOAA data
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-05-09
##' @log Add a log here
################################################################################

#### LOAD PACKAGES, DATA ####
library(tidyverse)

## NOAA - Caribbean
carib <- read.csv("ignore_folder/NOAA_carib/viehman/NCRMP_benthic_cover_All_regions_years_wide.csv", 
                  stringsAsFactors = FALSE)

## NOAA - Pacific
pacif <- read.csv("ignore_folder/NOAA_pacific/All NOAA MHI-NWHI-MARIAN-SAMOA-PRIA Benthic&Herbivore UPD.csv", 
                  skip = 7, stringsAsFactors = FALSE)

names(carib)[1:12]
names(pacif)[1:23]

carib %>% count(REGION, SUB_REGION_NAME)
pacif %>% count(REGION)

#### CARIB ####

## Get mean depth, and lat-long concatenation for each row
carib <- carib %>% 
  mutate(DEPTH = (MIN_DEPTH + MAX_DEPTH) / 2, 
         LL = paste(LAT_DEGREES, LON_DEGREES, sep = "_"), 
         ROW_ID = seq_along(REGION))

str(carib)
summary(carib)

length(carib$PRIMARY_SAMPLE_UNIT)
length(unique(carib$PRIMARY_SAMPLE_UNIT)) # why does this not equal previous?

carib %>% count(LL) %>% arrange(desc(n)) # why are there more unique lat-longs than primary sample units?
carib %>% count(PRIMARY_SAMPLE_UNIT) %>% arrange(desc(n))
carib %>% count(PRIMARY_SAMPLE_UNIT, STATION_NR)
carib %>% count(PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES, LAT_DEGREES) %>% arrange(desc(n))

## If we consider PSU, STATION_NR, DATE, and LAT-LON - we recover the length of the dataframe
carib %>% count(PRIMARY_SAMPLE_UNIT, STATION_NR, DATE, LON_DEGREES, LAT_DEGREES) %>% arrange(desc(n))
carib %>% count(PRIMARY_SAMPLE_UNIT, STATION_NR, DATE) %>% arrange(desc(n))
carib %>% count(LON_DEGREES, LAT_DEGREES, STATION_NR, DATE) %>% arrange(desc(n))
carib %>% count(LON_DEGREES, LAT_DEGREES, DATE) %>% arrange(desc(n), LON_DEGREES)

carib %>% count(STATION_NR)
carib %>% count(REGION, YEAR)
carib %>% count(REGION, SUB_REGION_NAME)
carib %>% count(HABITAT_CD) %>% arrange(desc(n))
carib %>% count(STRAT) %>% arrange(desc(n))

## Inspect rows with identical lat-longs
carib2 <- carib %>% 
  group_by(LL) %>% 
  mutate(n = n()) %>% ungroup()

carib_sub <- carib2 %>% filter(n > 1)
# View(carib_sub)
length(unique(carib_sub$PRIMARY_SAMPLE_UNIT))

# Question to Shay Viehman, 28 April 2018
# Would it make sense to take the mean percent cover for a PSU when two stations (STATION_NR; 1 & 2) were sampled on the same day?  That way I only have one set of coral cover data per site-date combination. 

#### PACIFIC ####
str(pacif)
names(pacif)[1:15]
pacif %>% count(SITE)
pacif %>% count(SITEVISITID)
pacif %>% count(OBS_YEAR, REGION)
pacif %>% count(SECTOR_NAME)
pacif %>% count(HABITAT_CODE)
pacif %>% count(REEF_ZONE) # limit to forereef only?
pacif %>% count(REEF_ZONE, HABITAT_CODE) %>% arrange(n)

pacif <- pacif %>% 
  mutate(ROW_ID = seq_along(REGION))

#### CREATE COMMON SITE METADATA WHERE POSSIBLE ####
names(carib)[1:12]
names(pacif)[1:23]

carib %>% count(REGION, SUB_REGION_NAME)
carib %>% count(REGION, SUB_REGION_NAME, STRAT) #%>% View()
## Note that PRICO does not have a SUB_REGION
## Lacks biotope data for PRICO - N, E, S (north, east, southwest of Puerto Rico)

## REGION_SUB (effectively ISLAND, with the exception of FLK, SEFCRI, and TORTUGAS in Florida)
unique(carib$SUB_REGION_NAME)
unique(pacif$ISLAND)

## HABITAT
unique(carib$HABITAT_CD)
unique(pacif$HABITAT_CODE)

## SITE - create for carib
## paste together REGION and LL (unique lat-long is defined as a site)
carib_site <- carib %>% 
  distinct(REGION, LL) %>% 
  group_by(REGION) %>%
  mutate(SITE = paste(REGION, seq_along(LL), sep = "-")) %>% 
  ungroup()

carib <- left_join(carib, carib_site, by = c("REGION", "LL"))
length(unique(carib$SITE))

## SITEVISITID - create for carib (site_visitnumber)
carib <- carib %>%
  group_by(SITE) %>% 
  mutate(SITEVISITID = paste(SITE, seq_along(SITE), sep = "_")) %>% 
  ungroup()
#carib$SITEVISITID %>% View()
length(unique(carib$SITEVISITID))
length(unique(pacif$SITEVISITID))

## NOT USED
unique(carib$STRAT)
unique(pacif$SECTOR_NAME)
unique(pacif$METHOD) 
unique(pacif$REEF_ZONE)

## Get common column names where possible
carib2 <- carib %>% 
  rename(SI_LATI = LAT_DEGREES, SI_LONG = LON_DEGREES, 
         HABITAT = HABITAT_CD) %>% 
  mutate(REGION_SUB = SUB_REGION_NAME)

pacif2 <- pacif %>% 
  rename(SI_LATI = LATITUDE, SI_LONG = LONGITUDE, 
         HABITAT = HABITAT_CODE, DATE = DATE_, 
         YEAR = OBS_YEAR, DEPTH = DEPTH.m.) %>% 
  mutate(REGION_SUB = ISLAND)

carib_meta <- carib2 %>% 
  select(ROW_ID, SITE, SITEVISITID, REGION, REGION_SUB, 
         YEAR, DATE, SI_LATI, SI_LONG, DEPTH, HABITAT, 
         PRIMARY_SAMPLE_UNIT, STATION_NR, STRAT) %>% 
  as_tibble()
carib_meta

pacif_meta <- pacif2 %>% 
  select(ROW_ID, SITE, SITEVISITID, REGION, REGION_SUB, 
         YEAR, DATE, SI_LATI, SI_LONG, DEPTH, HABITAT, 
         SECTOR_NAME, REEF_ZONE)

summary(carib_meta)
summary(pacif_meta)
names(carib_meta)
names(pacif_meta)


