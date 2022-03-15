################################################################################
##' @title Finalize long dataset for github
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-04-24
##' @log 2020-12-06: updated with all calcifying corals
##' @log 2021-07-04: updated with new sub-jurisdictions
################################################################################

##### PATHS #####
my_path <- "3_prep_global_data_2021/"
path_to_figs <- paste(my_path, "analysis_figs/", sep = "")

##### PACKAGES, DATA #####

library(tidyverse)
source("R/oasis_functions.R")
dat <- read_csv("workspace/coral_total_cover_predictors_adjusted_regions_2021.csv")
names(dat)
dat %>% count(JURISDICTION, YEAR)

# Remove Flower Gardens because too restricted an area with exceptional coral cover; throws off covariates
dat %>% 
  ggplot(aes(JURISDICTION, coral_cover)) + 
  geom_boxplot() + 
  facet_wrap(~ OCEAN, scales = "free_x") + 
  labs(y = "Coral cover (%)")
ggsave(paste(path_to_figs, "jurisdiction_cover_boxplot.pdf", sep = ""), height = 3.5, width = 7)

dat %>% 
  distinct(OCEAN, JURISDICTION, lat_bin_0.5, long_bin_0.5) %>% 
  count(OCEAN, JURISDICTION) %>% 
  ggplot(aes(JURISDICTION, n)) + 
  geom_col() + 
  facet_wrap(~ OCEAN, scales = "free_x") + 
  labs(y = "Number of grid cells")
ggsave(paste(path_to_figs, "jurisdiction_grid_cells_barplot.pdf", sep = ""), height = 3.5, width = 7)

dat <- dat %>% filter(JURISDICTION != "FGBNMS") %>% droplevels()

# Remove PRIAs because they don't make sense as a jurisdiction
dat %>% 
  filter(JURISDICTION == "PRIAs") %>% 
  count(JURISDICTION, REGION, REGION_SUB)

dat <- dat %>% filter(JURISDICTION != "PRIAs") %>% droplevels()

# Re-level jurisdictions
dat <- dat %>% 
  mutate(JURISDICTION = factor(JURISDICTION, 
                               levels = c("Florida", "PRICO", "USVI", 
                                          "MARIAN", "NWHI", "MHI", "SAMOA")))
## Relevel jurisdiction
dat$JURISDICTION <- fct_recode(dat$JURISDICTION, FLORIDA = "Florida")

# Create cell bins
dat <- dat %>% 
  mutate(cell_0.5 = paste(round(long_bin_0.5, 3), round(lat_bin_0.5, 3), sep = "_"), 
         cell_2.5 = paste(round(long_bin_2.5, 3), round(lat_bin_2.5, 3), sep = "_"))
dat %>% count(cell_0.5)
dat %>% count(long_bin_0.5, lat_bin_0.5)
dat %>% count(cell_2.5)
dat %>% count(long_bin_2.5, lat_bin_2.5)

# Remove unnecessary covariates
names(dat)
dat <- dat %>% 
  select(-c(X1, human_pop50, land_area50))

## Examine sub-jurisdiction grid cells
dat %>% 
  distinct(OCEAN, JURISDICTION, REGION, lat_bin_0.5, long_bin_0.5) %>% 
  count(OCEAN, JURISDICTION, REGION) %>% 
  arrange(n)

dat %>% 
  distinct(OCEAN, JURISDICTION, REGION, lat_bin_0.5, long_bin_0.5) %>% 
  count(OCEAN, JURISDICTION, REGION) %>% 
  ggplot(aes(REGION, n)) + 
  geom_col() + 
  facet_wrap(~ JURISDICTION, scales = "free_y") + 
  labs(y = "Number of grid cells", x = "Sub-region") + 
  coord_flip() + 
  geom_hline(yintercept = 10, linetype = "dashed", color = "red")

ggsave(paste(path_to_figs, "subjurisdiction_grid_cells_barplot.pdf", 
             sep = ""), height = 7, width = 10)

##### CHECK CELLS WITH TWO REGIONS #####

grid_cell_n <- dat %>% 
  count(OCEAN, JURISDICTION, REGION, REGION_SUB,   
        cell_2.5, lat_bin_2.5, long_bin_2.5)

grid_cell_df <- grid_cell_n %>% 
  count(OCEAN, JURISDICTION, #REGION, #REGION_SUB,   
        cell_2.5, lat_bin_2.5, long_bin_2.5) %>% 
  arrange(desc(n))

## Connect these cell counts back to the original data
dat2 <- left_join(dat, grid_cell_df, by = c("OCEAN", "JURISDICTION", "cell_2.5", "lat_bin_2.5", "long_bin_2.5"))

## Arrange and then save for hand-editing
names(dat2)
row_id <- seq(1:length(dat2$SITE))
dat2 <- cbind(row_id, dat2)
names(dat2)
dat2 <- dat2 %>% arrange(desc(n))

##### SAVE #####

##' Due to the fact that grid cells are large enough to contain reef sites that span more than two designated REGIONS
##' (ie subjurisdictions)
##' I needed to hand-edit changes the assigned REGIONs
##' I did this by changing minority REGIONs of a grid cell to the majority region
##' I also used maps to help these re-definitions
##' The end goal was to have one grid cell, with all reef sites within assigned to a single REGION
##' There was only one grid cell where this decision may turn out to be awkward, the grid cell between 
##' St Thomas and St John - most of the sites were from St Thomas, but a few St John sites were redefined as STT
##' For the purpose of my analysis

# Save
#write.csv(dat2, "workspace/coral_total_cover_and_predictors_hand_edit.csv")


##### RELOAD EDITED FILE #####
d_edit <- read_csv("workspace/coral_total_cover_and_predictors_hand_edit_EDITED.csv")
names(d_edit)

## Check again
grid_cell_n <- d_edit %>% 
  count(OCEAN, JURISDICTION, REGION, 
        cell_2.5, lat_bin_2.5, long_bin_2.5)

grid_cell_n %>% count(cell_2.5, lat_bin_2.5, long_bin_2.5) %>% count(n)
grid_cell_n %>% count(cell_2.5, lat_bin_2.5, long_bin_2.5) %>% 
  arrange(desc(n))

## Save
d_edit <- d_edit %>% 
  select(-c(X1)) %>% 
  arrange(row_id) %>% 
  rename(region_n = n, region_edit_notes = edit_notes)
write.csv(d_edit, "workspace/coral_total_cover_and_predictors_2021.csv")
