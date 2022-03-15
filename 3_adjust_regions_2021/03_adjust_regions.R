################################################################################
##' @title Adjust region_sub for analysis
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2019-10-14
##' @log 2020-12-06: updated with all calcifying corals
##' @log 2021-07-02: updated with new sub-jurisdictions (ie sub-regions)
################################################################################

#### PATHS #####
my_path <- "3_adjust_regions_2021/"
path_to_figs <- paste(my_path, "analysis_figs/", sep = "")
path_to_output <- paste(my_path, "analysis_output/", sep = "")

##### CHANGE REGION_SUB #####
source(paste(my_path, "02_adjust_FGBNMS.R", sep = ""))
source(paste(my_path, "02_adjust_USVI.R", sep = ""))
source(paste(my_path, "02_adjust_PRICO.R", sep = ""))
source(paste(my_path, "02_adjust_Florida.R", sep = ""))
source(paste(my_path, "02_adjust_Marianas.R", sep = ""))
source(paste(my_path, "02_adjust_MHI.R", sep = ""))
source(paste(my_path, "02_adjust_NWHI.R", sep = ""))
source(paste(my_path, "02_adjust_Samoa.R", sep = ""))

d_new <- rbind(d_fgbnms, d_prico, d_usvi, d_florida, 
               d_marianas, d_mhi, d_nwhi, d_samoa) %>% 
  mutate(my_cell = paste(long_bin_2.5, lat_bin_2.5, sep = "_"))

# Inspect
d_new %>% count(OCEAN, JURISDICTION, REGION) %>% print(n = 100)
d_new %>% count(OCEAN, JURISDICTION, REGION, REGION_SUB) %>% print(n = 100)
d_new %>% count(REEF_ZONE)
summary(d_new)

d_new %>% count(lat_bin_2.5, long_bin_2.5)
d_new %>% count(lat_bin_2.5, long_bin_2.5, JURISDICTION)
d_new %>% count(lat_bin_2.5, long_bin_2.5, REGION)
d_new %>% count(lat_bin_2.5, long_bin_2.5, REGION_SUB)

# Culprit
culprit <- d_new %>% 
  count(lat_bin_2.5, long_bin_2.5, my_cell, REGION) %>% 
  count(lat_bin_2.5, long_bin_2.5, my_cell) %>% 
  arrange(desc(n)) %>% slice(1)

d_new_culprit <- d_new[d_new$my_cell %in% culprit$my_cell, ]

# Change Reef Zone
d_new <- d_new %>% 
  mutate(REEF_ZONE = ifelse(is.na(REEF_ZONE), "Not available", REEF_ZONE))

#### WRITE NEW DATA #####
d_new <- d_new %>% select(-X1)
write.csv(d_new, "workspace/coral_total_cover_predictors_adjusted_regions_2021.csv")

