################################################################################
##' @title Compile predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-11-15
##' @log Add a log here
################################################################################

#### LOAD PACKAGES, DATA ####

library(tidyverse)
library(skimr)
theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

path_to_data <- "ignore_folder/"

## Data from Iliana Chollett
dat_ic <- read_csv(file = paste(path_to_data, "noaa_ll_date_storms.csv", sep = "")) %>% select(-c(X1))
names(dat_ic)

# Create new hurricane categories
dat_ic <- dat_ic %>% 
  mutate(nAll = n3 + n4 + n5, 
         nAll_5yr = n3_5yr + n4_5yr + n5_5yr, 
         nAll_30yr = n3_30yr + n4_30yr + n5_30yr)

## Data from Brian Barnes
# Notes from Brian
# For each station, I've extracted 4 products (sst_mean, sst_variance, kd490_mean, and kd490_variance) which I think best define the temperature and light environment (in as few parameters as possible). These are derived from monthly mean products from NASA, using a process similar to Sbrocco et al 2013 (without their questionably supersampling). Note that I've used the standard Kd490 product, which suffers from boYom contaminaGon (long story). Thus I trust the kd490_variance product moreso than the mean product.

# Level-3 SMI monthly composites (Jan 2003 - Dec 2017) of MODIS SST (nighfme 4μm) and Kd_490 (KD2 algorithm) were acquired from NASA OBPG archives (hYps://oceancolor.gsfc.nasa.gov/cgi/l3). These were mapped using the SeaDAS (version 7.4) rouGne l3mapgen to a cylindrical equidistant projecGon with 2.5 arcminute resoluGon and boundaries of 15S - 29N, 144E - 64W (lon_0=180). Cubic interpolaGon was used to fill gaps in the data, which primarily resulted from land and persistently cloudy condiGons in equatorial Pacific regions. Pixels idenGfied as land (according to the GSHHS "high" resoluGon landmask, mapped to the same projecGon as above) were removed. Summary products were derived from the monthly mean SST and Kd_490, including mean (i.e., mean of monthly mean composites), variance, maximum, minimum, and range.

dat_bb <- read_csv("ignore_folder/noaa_ll_bbb_edit.csv") %>%  
  select(ll_id:YEAR, long_bin_2.5, lat_bin_2.5, meansst, varsst, meankd490, varkd490) 
names(dat_bb)

## Data from Yeager 
dat_msec <- read_csv("workspace/noaa_ll_msec.csv") %>% select(-X1)
names(dat_msec)

##### COMPILE #####

## Note that dat_ic has fewer rows
## because I removed some duplicates for Iliana which I had yet to do for Brian or the MSEC data
dat <- dat_bb %>% inner_join(., dat_ic, by = "ll_id")
dat <- dat %>% select(-c(OCEAN:YEAR))
dat <- dat_msec %>% inner_join(., dat, by = "ll_id")
summary(dat) # note 15 NAs in Brian's data (why?)
skim(dat)

## Rename some columns
dat <- dat %>% 
  rename(sst_mean = meansst, sst_var = varsst, 
         kd490_mean = meankd490, kd490_var = varkd490) %>% 
  mutate(YEAR = as.character(YEAR))

## Calculate log pop, CV
names(dat)
dat <- dat %>%
  mutate(human_pop50_log = log(human_pop50 + 1), 
         wave_cv = wave_sd / wave_mean, 
         npp_cv = npp_sd / npp_mean, 
         sst_cv = (sst_var^0.5) / sst_mean, 
         kd490_cv = (kd490_var^0.5) / kd490_mean)

###### ADD CORTAD DATA #####

c1 <- read_csv("data_output/cortad_FilledSSTmean.csv") %>% 
  select(ll_id, z) %>% 
  mutate(c_sst_mean = z - 273.15) %>% 
  select(-z)

c2 <- read_csv("data_output/cortad_FilledSSTstandardDeviation.csv") %>% 
  select(ll_id, z) %>% 
  mutate(c_sst_sd = z)

dat2 <- dat %>% inner_join(., c1, by = "ll_id") %>% inner_join(., c2, by = "ll_id")

summary(dat2)

mean(dat2$c_sst_sd, na.rm = TRUE)
mean(sqrt(dat2$sst_var), na.rm = TRUE)

## Remove PRIAs and FGBNMS
dat2 <- dat2 %>% 
  filter(REGION != "FGBNMS" & REGION != "PRIAs") 

dat2 %>% distinct(SI_LATI, SI_LONG)

##### PLOT #####

dat2 %>% 
  ggplot(aes(c_sst_mean, c_sst_sd, color = REGION)) +
  geom_point(alpha = 0.25) + 
  labs(title = "Cortad, 1982-2012, weekly") + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ OCEAN, scales = "free") 
  # scale_x_continuous(limits = c(23, 30)) + 
  # scale_y_continuous(limits = c(0.5, 3.5)) 
ggsave("2_compile_data/predictor_figs/cortad_sst_mean_sd_scatter.pdf", height = 3.5, width = 7)

dat2 %>% 
  ggplot(aes(sst_mean, sqrt(sst_var), color = REGION)) +
  geom_point(alpha = 0.25) + 
  labs(title = "Barnes, 2003-2017, monthly") + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ OCEAN, scales = "free")
ggsave("2_compile_data/predictor_figs/barnes_sst_mean_sd_scatter.pdf", height = 3.5, width = 7)

dat2 %>% 
  ggplot(aes(c_sst_mean, sst_mean, color = OCEAN)) +
  geom_point(alpha = 0.25) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm")
ggsave("2_compile_data/predictor_figs/sst_mean_scatter.pdf", height = 3.5, width = 7)

dat2 %>% 
  ggplot(aes(c_sst_sd, sqrt(sst_var), color = OCEAN)) +
  geom_point(alpha = 0.25) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm")
ggsave("2_compile_data/predictor_figs/sst_sd_scatter.pdf", height = 3.5, width = 7)

dat3 <- dat2 %>% 
  select(ll_id:REGION_NEW, 
         sst_mean, sst_var, c_sst_mean, c_sst_sd) %>% 
  mutate(sst_sd = sqrt(sst_var)) %>% 
  select(-sst_var)

pdf("2_compile_data/predictor_figs/barnes_cortad_temperature_pairs.pdf", width = 7, height = 7)
dat3 %>% select(sst_mean, sst_sd, c_sst_mean, c_sst_sd) %>% pairs()
dev.off()

datL <- dat3 %>% 
  gather(key = metric, value = value, sst_mean:sst_sd)
datL %>% 
  filter(metric == "c_sst_mean" | metric == "sst_mean") %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density() + 
  facet_wrap(~ metric, nrow = 2)
ggsave("2_compile_data/predictor_figs/barnes_cortad_sst_mean_histograms.pdf", height = 5, width = 5)

