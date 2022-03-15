################################################################################
##' @title Get lat-longs for NOAA data
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-09-10
##' @log Add a log here
################################################################################

#### LOAD PACKAGES, DATA ####
source("1_process_noaa/0_load_NOAA_raw.R")
source("R/get_gridded_bins.R")

## Original data
names(carib)
names(pacif)

## New metadata
names(carib_meta)
names(pacif_meta)

carib_meta %>% count(REGION_SUB)
pacif_meta %>% count(REGION_SUB)

#### LIST OF LAT-LONGS ####
carib_ll <- carib_meta %>% 
  distinct(REGION, REGION_SUB, YEAR, DATE, SI_LATI, SI_LONG) %>% 
  mutate(OCEAN = "CARIB") 
carib_ll %>% distinct(REGION)

crap <- carib_ll %>% filter(REGION == "Tortugas")

pac_ll <- pacif_meta %>% 
  distinct(REGION, REGION_SUB, YEAR, DATE, SI_LATI, SI_LONG) %>% 
  mutate(OCEAN = "PACIFIC") 
pac_ll %>% distinct(REGION_SUB)

noaa_ll <- rbind(carib_ll, pac_ll) %>% 
  mutate(ll_id = seq_along(SI_LATI)) %>% 
  mutate(REGION_SUB = ifelse(is.na(REGION_SUB), REGION, REGION_SUB)) %>% 
  as_tibble()
#noaa_ll %>% distinct(REGION, REGION_SUB) %>% View()

## Get gridded bins
res <- 0.5 / 60 # 0.5 arc-minutes (i.e., 30 arc-seconds; i.e., ~1km)
noaa_ll <- get_lat_long_bin(noaa_ll, bin_size = res, msec = F) %>% 
  rename(long_bin_0.5 = long_bin, lat_bin_0.5 = lat_bin)

head(noaa_ll)

res <- 2.5 / 60 # 2.5 arc-minutes (~4.6km)
noaa_ll <- get_lat_long_bin(noaa_ll, bin_size = res, msec = F) %>% 
  rename(long_bin_2.5 = long_bin, lat_bin_2.5 = lat_bin)

noaa_ll <- noaa_ll %>% 
  select(ll_id, OCEAN, REGION, REGION_SUB, SI_LATI, SI_LONG, YEAR, DATE, 
         long_bin_0.5:lat_bin_2.5) 
names(noaa_ll)

noaa_ll %>% count(long_bin_2.5, lat_bin_2.5)
d1 <- noaa_ll %>% count(long_bin_2.5, lat_bin_2.5, REGION)
d1 %>% distinct(long_bin_2.5, lat_bin_2.5)
d2 <- noaa_ll %>% count(long_bin_2.5, lat_bin_2.5, REGION, REGION_SUB) # why extra rows? due to FLK (3); SEFCRI (2); Tortugas (1)

# write.csv(d1, "data_output/d1.csv")
# write.csv(d2, "data_output/d2.csv")
# Based on the above csvs for Tortugas, it looks as though some lat-long pairs were associated with two different REGION_SUBs - this will duplicate sites downstream

# noaa_ll was sent to Iliana and Brian
# Iliana used the raw lat-longs
# Brian used the 2.5 degree bins
# write.csv(noaa_ll, "data_output/noaa_ll_date.csv")

