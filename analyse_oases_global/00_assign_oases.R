################################################################################
##' @title Standardize response variable and assign oases
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-04-24
##' @log 2020-12-06: updated with all calcifying corals
##'      2021-07-04: updated with new sub-jurisdictions
##'      2021-08-06: removed duplicate sites
##'      2021-09-02: removed Swains, because only 2 grid cells
################################################################################

source("analyse_oases_global/00_set_paths.R")
source(paste(my_path, "__clean_data.R", sep = ""))
names(dat)

# Transform coral cover
dat <- dat %>% 
  mutate(cover_prop = coral_cover / 100, 
         cover_prop2 = y.transf.betareg(cover_prop))

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

# Decide on zed threshold
z_threshold <- 2

##### GET Z-SCORES BY SCALE #####

## zed0 - Global scale [cross-basin]
dat <- dat %>% 
  mutate(zed0 = as.numeric(scale(coral_cover)))

## zed1 - OCEAN scale (Caribbean v Pacific) [basin]
dat <- dat %>% 
  do(., get_zed_group(., group_var = OCEAN, summary_var = coral_cover)) %>% 
  ungroup() %>% 
  rename(zed1 = zed)

## zed2 - JURISDICTION scale [region]
dat <- dat %>% 
  do(., get_zed_group(., group_var = JURISDICTION, summary_var = coral_cover)) %>% 
  ungroup() %>% 
  rename(zed2 = zed)

## zed3 - REGION scale [sub-region]
dat <- dat %>% 
  do(., get_zed_group(., group_var = REGION, summary_var = coral_cover)) %>% 
  ungroup() %>% 
  rename(zed3 = zed)

##### ASSIGN OASES ######

dat <- dat %>% 
  mutate(oasis0 = ifelse(zed0 > z_threshold, "oasis", "not_oasis"),
         oasis1 = ifelse(zed1 > z_threshold, "oasis", "not_oasis"), 
         oasis2 = ifelse(zed2 > z_threshold, "oasis", "not_oasis"), 
         oasis3 = ifelse(zed3 > z_threshold, "oasis", "not_oasis"))

dat %>% count(oasis0) %>% summarise(sum(n))
dat %>% count(oasis1) %>% summarise(sum(n))
dat %>% count(oasis2) %>% summarise(sum(n))
dat %>% count(oasis3) %>% summarise(sum(n))

## Check n
dat_test <- dat %>% 
  select(REGION, human_pop50_log:wave_sd)
dat_test2 <- dat_test[complete.cases(dat_test), ]

##### SUMMARISE COVARIATES ######
grid_summary <- dat %>% 
  group_by(OCEAN, JURISDICTION, REGION, 
           cell_2.5, lat_bin_2.5, long_bin_2.5) %>% 
  summarise_at(vars(c_sst_mean:wave_sd), mean, na.rm = TRUE) %>% 
  ungroup()
grid_summary %>% count(cell_2.5, lat_bin_2.5, long_bin_2.5) %>% count(n)

grid_cell_n <- dat %>% 
  count(OCEAN, JURISDICTION, REGION, 
           cell_2.5, lat_bin_2.5, long_bin_2.5)
grid_cell_n %>% count(cell_2.5, lat_bin_2.5, long_bin_2.5) %>% count(n)

summary(grid_summary)

##### ADJUST HUMAN POPULATION DENSITY ######
## Add noise to covariates when = to zero
# This approach does not work when n = 1 in runif(); does work when n = n_df
# But note that this does NOT solve the problem when the covariate is the same across the subregion (but not 0)
n_df <- length(grid_summary$OCEAN)

grid_summary2 <- grid_summary %>% 
  mutate(human_pop50_log = ifelse(human_pop50_log == 0, 
                                  runif(n = n_df, min = log(1), max = log(1.25)), 
                                  human_pop50_log))

grid_summary2 <- grid_summary %>% 
  mutate(human_pop50_log = ifelse(human_pop50_log > 0, human_pop50_log, 
                                  runif(n = n_df, min = log(1), max = log(1.05))))

summary(grid_summary$human_pop50_log)
summary(grid_summary2$human_pop50_log)

par(mfrow = c(1,2))
hist(grid_summary$human_pop50_log, breaks = 30)
hist(grid_summary2$human_pop50_log, breaks = 30)

## INSTEAD:
## Add small amount of variation to ALL rows
n_df <- length(grid_summary$OCEAN)
min_value <- grid_summary %>% 
  filter(human_pop50_log > 0) %>% 
  summarise(min(human_pop50_log))
x_noise <- runif(n = n_df, min = log(1), max = log(1.05))
hist(x_noise)
(mean(x_noise) / min_value) # GET THE AVERAGE NOISE TO BE LESS THAN 5% of minimum value

# add noise to all rows
human_pop50_log_rand <- grid_summary$human_pop50_log + x_noise
par(mfrow = c(1,2))
hist(grid_summary$human_pop50_log, breaks = 30)
hist(human_pop50_log_rand, breaks = 30)
summary(grid_summary$human_pop50_log)
summary(human_pop50_log_rand)

##### ADJUST STORMS ######
## Add small amount of variation to ALL rows
n_df <- length(grid_summary$OCEAN)
min_value <- grid_summary %>% 
  filter(nAll_30yr > 0) %>% 
  summarise(min(nAll_30yr))
x_noise <- runif(n = n_df, min = 0, max = 0.005)
hist(x_noise)
(mean(x_noise) / min_value) # GET THE AVERAGE NOISE TO BE LESS THAN 5% of minimum value

# add noise to all rows
nAll_30yr_rand <- grid_summary$nAll_30yr + x_noise
par(mfrow = c(1,2))
hist(grid_summary$nAll_30yr, breaks = 30)
hist(nAll_30yr_rand, breaks = 30)
summary(grid_summary$nAll_30yr)
summary(nAll_30yr_rand)

##### ADJUST LAND AREA #####
## Add small amount of variation to ALL rows
n_df <- length(grid_summary$OCEAN)
min_value <- grid_summary %>% 
  filter(land_area50_log > 0) %>% 
  summarise(min(land_area50_log))
x_noise <- runif(n = n_df, min = log(1), max = log(1.01))
hist(x_noise)
(mean(x_noise) / min_value) # GET THE AVERAGE NOISE TO BE LESS THAN 5% of minimum value

# add noise to all rows
land_area50_log_rand <- grid_summary$land_area50_log + x_noise
par(mfrow = c(1,2))
hist(grid_summary$land_area50_log, breaks = 30)
hist(land_area50_log_rand, breaks = 30)
summary(grid_summary$land_area50_log)
summary(land_area50_log_rand)

##### INCORPORATE ADJUSTED VALUES #####

grid_summary <- grid_summary %>% 
  mutate(human_pop50_log = human_pop50_log_rand, 
         nAll_30yr = nAll_30yr_rand, 
         land_area50_log = land_area50_log_rand)

summary(grid_summary)

##### GRID THE OASIS DATA - zed0 ######
#d <- dat %>% rename(oasis = oasis0)

grid_df_w <- dat %>% 
  rename(oasis = oasis0) %>%
  grid_oasis_data(., grid_summary = grid_summary)

grid_df_w <- grid_df_w[complete.cases(grid_df_w), ]

grid_df_w <- grid_df_w %>% 
  mutate_at(.vars = vars(c_sst_mean:wave_sd), 
            .funs = list(z = scale2)) %>% 
  ungroup()

## Check
sum(grid_df_w$Ji)

write.csv(grid_df_w, "workspace/grid_df_w0.csv")

##### GRID THE OASIS DATA - zed1 ######
grid_df_w <- dat %>% 
  rename(oasis = oasis1) %>%
  grid_oasis_data(., grid_summary = grid_summary)

grid_df_w <- grid_df_w[complete.cases(grid_df_w), ]

grid_df_w <- grid_df_w %>% 
  group_by(OCEAN) %>% 
  mutate_at(.vars = vars(c_sst_mean:wave_sd), 
            .funs = list(z = scale2)) %>% 
  ungroup()

## Check
sum(grid_df_w$Ji)

write.csv(grid_df_w, "workspace/grid_df_w1.csv")

##### GRID THE OASIS DATA - zed2 ######
grid_df_w <- dat %>% 
  rename(oasis = oasis2) %>% 
  grid_oasis_data(., grid_summary = grid_summary)

grid_df_w <- grid_df_w[complete.cases(grid_df_w), ]

grid_df_w <- grid_df_w %>% 
  group_by(JURISDICTION) %>% 
  mutate_at(.vars = vars(c_sst_mean:wave_sd), 
            .funs = list(z = scale2)) %>% 
  ungroup()

## Check
sum(grid_df_w$Ji)

write.csv(grid_df_w, "workspace/grid_df_w2.csv")

##### GRID THE OASIS DATA - zed3 ######
grid_df_w <- dat %>% 
  rename(oasis = oasis3) %>% 
  grid_oasis_data(., grid_summary = grid_summary)

grid_df_w <- grid_df_w[complete.cases(grid_df_w), ]

grid_df_w <- grid_df_w %>% 
  group_by(REGION) %>% 
  mutate_at(.vars = vars(c_sst_mean:wave_sd), 
            .funs = list(z = scale2)) %>% 
  ungroup()

## Check
sum(grid_df_w$Ji)

write.csv(grid_df_w, "workspace/grid_df_w3.csv")

##### WRITE OTHER FILES ######
names(dat)
dat <- dat %>% 
  select(-c(X1, region_n, region_edit_notes, 
            long_bin_0.5, lat_bin_0.5, cell_0.5, my_cell))
write.csv(dat, "workspace/coral_total_cover_and_predictors_zed.csv")

names(grid_summary)
write.csv(grid_summary, "workspace/grid_summary.csv")

