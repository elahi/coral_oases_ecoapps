################################################################################
##' @title Process NOAA data - cover
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-05-09
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
# MF = mobile fauna (unclassified)
# Note - COVER = the sum of coral cover; I will remove this
# Create new column, MACROALGAE, which is the sum of EMA (encrusting macroalgae) and MA (macroalgae)

pacif_cover <- pacif %>% 
  select(TURF:Turbinaria.sp) %>% 
  mutate(MACROALGAE = EMA + MA) %>% as_tibble() %>% 
  select(-c(UC, TW, MF, EMA, MA, CORAL)) 

names(pacif_cover)

## TESTING: dummy
mat <- matrix(1, ncol=2, nrow=2,TRUE) %>% as_tibble()
dev <- c(5, 10)
mat
dev
mat / dev

# Get total cover, after removing unclassified categories
total_cover <- rowSums(pacif_cover)
pacif_cover2 <- (pacif_cover / total_cover) %>% tbl_df()

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

### Get in long format
pacif_long <- pacif_cover %>% 
  gather(key = species, value = cover, TURF:MACROALGAE) %>% 
  tbl_df() %>% 
  mutate(cover = cover * 100)

#### COVER DATA - CARIB ####
names(carib)

## Simplify columns to match Pacific data
carib2 <- carib %>% 
  rename(BARE = Bare.Substrate) %>% 
  mutate(I = Cliona.spp + Palythoa.spp + Porifera.spp, 
         TURF = Cyanophyta.spp + Turf.Algae.Free.of.Sediment + Turf.Algae.with.Sediment, 
         MACROALGAE = Dictyota.spp + Halimeda.spp + Lobophora.spp + MacroOtherCalcareous + 
           MacroOtherFleshy + Magnoliophyta.spp + Peysonnellia, 
         CCA = Rhodophyta.cru..spp, 
         SC = Encrusting.gorgonian + Erythropodium.caribaeorum + Gorgonians)

carib_columns_to_remove <- c("Cliona.spp", "Palythoa.spp", "Porifera.spp", 
                             "Cyanophyta.spp", "Turf.Algae.Free.of.Sediment", 
                             "Turf.Algae.with.Sediment", "Dictyota.spp", "Halimeda.spp", 
                             "Lobophora.spp", "MacroOtherCalcareous",
                             "MacroOtherFleshy", "Magnoliophyta.spp", "Peysonnellia", 
                             "Rhodophyta.cru..spp", "Encrusting.gorgonian", 
                             "Erythropodium.caribaeorum", "Gorgonians")

## Remove the rows that I used to create sums
carib2 <- carib2[, !names(carib2) %in% carib_columns_to_remove]
names(carib2)

### Quality control
# Check to make sure that cover adds up to 1
carib_cover <- carib2 %>% 
  select(Acropora.cervicornis:Tubastraea.coccinea, I:SC) %>% 
  mutate(total_cover = rowSums(.)) %>% tbl_df()

hist(carib_cover$total_cover)
unique(carib_cover$total_cover)
carib_cover %>% filter(total_cover < 1) %>% dim()

# Join with metadata
carib_cover <- cbind(carib_meta, carib_cover)
names(carib_cover)

# Some rows have 0 total_cover - remove these
carib_cover <- carib_cover %>% filter(total_cover > 0)

# Check length of SITE and SITEVISITID
hist(carib_cover$total_cover)
length(unique(carib_cover$SITE))
length(unique(carib_cover$SITEVISITID))

# Change date format
carib_cover$DATE <- lubridate::mdy(carib_cover$DATE) 

### Get in long format
carib_long <- carib_cover %>% 
  select(-total_cover) %>% 
  gather(key = species, value = cover, Acropora.cervicornis:SC) %>% 
  tbl_df()
carib_long
length(unique(carib_long$SITE))
length(unique(carib_long$SITEVISITID))
length(unique(carib_long$PRIMARY_SAMPLE_UNIT))

### Summarise to get one value per year per site
# Take the average across multiple STATION_NR on the same sampling date
names(carib_long)
carib_long2 <- carib_long %>% 
  filter(!is.na(cover)) %>% 
  group_by(SITE, REGION, REGION_SUB, YEAR, DATE, SI_LATI, SI_LONG, 
           DEPTH, HABITAT, PRIMARY_SAMPLE_UNIT, STRAT, 
           # STATION_NR,  - this is a critical piece that reverts back to original row #
           # ROW_ID,      - this is a critical piece that reverts back to original row #
           species) %>% 
  summarise(cover = mean(cover)) %>% 
  ungroup()
length(unique(carib_long2$SITE))
length(unique(carib_long2$PRIMARY_SAMPLE_UNIT))

names(carib)[1:12]
names(carib_long2)

#### COMPILE COVER DATA FOR PACIFIC AND CARIB ####
names(carib_long2) %in% names(pacif_long)

# In carib but not pacific
unique(carib_long2$PRIMARY_SAMPLE_UNIT)
length(unique(carib_long2$PRIMARY_SAMPLE_UNIT))
length(unique(carib_long2$SITE))
unique(carib_long2$STRAT) # (this is an aggregate of HABITAT and jurisdictional boundaries like MPAs)
unique(carib_long2$HABITAT)

carib_habitat <- carib_long2 %>% 
  distinct(HABITAT, REGION) %>% arrange(REGION, HABITAT)
#write.csv(carib_habitat, "data_output/carib_habitat.csv")

# In pacif but not carib
names(pacif_long)
names(pacif_long) %in% names(carib_long2)
unique(pacif_long$SITEVISITID)
unique(pacif_long$SECTOR_NAME) # similar to carib STRAT
unique(pacif_long$REEF_ZONE)
unique(pacif_long$HABITAT)

length(unique(pacif_long$SITEVISITID))

## Remove these and add new columns
carib_long3 <- carib_long2 %>% 
  select(-c(PRIMARY_SAMPLE_UNIT, STRAT)) %>% 
  mutate(REEF_ZONE = NA, 
         OCEAN = "CARIB")

pacif_long2 <- pacif_long %>% 
  select(-c(ROW_ID, SITEVISITID, SECTOR_NAME)) %>% 
  mutate(OCEAN = "PACIFIC")

## Compile oceans
cover <- rbind(carib_long3, pacif_long2)

## Join with lat-long ID
noaa_ll$DATE <- lubridate::mdy(noaa_ll$DATE) 

cover <- cover %>% 
  left_join(., noaa_ll, 
            by = c("REGION", "REGION_SUB", "YEAR", "DATE", 
                   "SI_LATI", "SI_LONG", "OCEAN"))
head(cover)

## Get species list
species_list <- cover %>% distinct(species, OCEAN) %>% arrange(species)
species_list

## Extract genera

crap <- 'Acanthastrea.sp'
strsplit(crap, split = ".", fixed = TRUE)[[1]][1]

species_list <- species_list %>% 
  mutate(genus = str_split_fixed(species, pattern = ".", n = 1))

#write.csv(species_list, "data_output/cover_species_list.csv")

