################################################################################
##' @title Process NOAA data - cover by taxon in PACIFIC
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-11-05
##' @log Add a log here
################################################################################

#### LOAD PACKAGES, DATA ####

source("1_process_noaa/0_load_NOAA_raw.R")

noaa_ll <- read_csv("data_output/noaa_ll_date.csv")

## Original data
names(carib)
names(pacif)

## New metadata
names(carib_meta)
names(pacif_meta)

#### COVER DATA - PACIFIC ####
## Adjust percent cover values for unclassified
# UC = unclassified
# TW = tape wand (unclassified)
# TURF = turf algae
# CCA = calcified crustose algae
# SED = sediment
# EMA = encrusting macroalgae
# MA = macroalgae
# MF = mobile fauna (unclassified)
# I = invertebrate (sessile)
# SC = soft coral
# Note - COVER = the sum of coral cover; I will remove this
# Note that I am removing all herbivore data

# Create new column, MACROALGAE, which is the sum of EMA and MA 
# pacif_cover <- pacif %>% 
#   select(TURF:Turbinaria.sp) %>% 
#   mutate(MACROALGAE = EMA + MA) %>% as_tibble() %>% 
#   select(-c(UC, TW, MF, EMA, MA, CORAL)) 

# Leave EMA and MA as is
pacif_cover <- pacif %>% 
  select(TURF:Turbinaria.sp) %>% 
  as_tibble() %>% 
  select(-c(UC, TW, MF, CORAL)) 

names(pacif_cover)

## TESTING: dummy
mat <- matrix(1, ncol=2, nrow=2,TRUE) %>% as_tibble()
dev <- c(5, 10)
mat
dev
mat / dev

# Get total cover, after removing unclassified categories
total_cover <- rowSums(pacif_cover)
pacif_cover2 <- (pacif_cover / total_cover) %>% as_tibble()

## TESTING: Check to make sure I'm dividing over the right dimension
i = 59
crap <- pacif_cover[i, ] / total_cover[i]
plot(t(pacif_cover2[i, ]), t(crap))
abline(a = 0, b = 1)
plot(pacif_cover$TURF, pacif_cover2$TURF)
plot(pacif_cover$SED, pacif_cover2$SED)

## Join with metadata
pacif_cover <- cbind(pacif_meta, pacif_cover2)

# Change date format
tail(pacif_cover$DATE)
pacif_cover$DATE <- lubridate::mdy(pacif_cover$DATE) 

# ### Get in long format
# names(pacif_cover)
# pacif_long <- pacif_cover %>% 
#   gather(key = species, value = cover, TURF:Turbinaria.sp) %>% 
#   tbl_df() %>% 
#   mutate(cover = cover * 100)

pacif_cover <- pacif_cover %>% 
  mutate(OCEAN = "PACIFIC")

## Join with lat-long ID
noaa_ll$DATE <- lubridate::mdy(noaa_ll$DATE) 

pacif_cover <- pacif_cover %>% 
  left_join(., noaa_ll, 
            by = c("REGION", "REGION_SUB", "YEAR", "DATE", 
                   "SI_LATI", "SI_LONG", "OCEAN"))
names(pacif_cover)

write.csv(pacif_cover, "workspace/pacific_cover_spp_wide.csv")

