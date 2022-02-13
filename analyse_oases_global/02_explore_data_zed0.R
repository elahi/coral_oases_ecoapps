################################################################################
##' @title Exploratory data analysis
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-04-24
##' @log 2020-07-23: changed gridded data
##'      2020-12-07: updated coral species
##'      2021-07-05: updated subjurisdictions
##'      2021-08-06: removed duplicate sites
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

##### PACKAGES, DATA #####
source("R/HighstatLibV6.R")
library(tidyverse)
library(viridis)
library(ggcorrplot)
library(forcats)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

my_scale <- "zed0_"
statDat <- read.csv("workspace/grid_df_w0.csv")
summary(statDat)

## Relevel jurisdiction
statDat$JURISDICTION <- fct_relevel(statDat$JURISDICTION,
                                    "MARIAN", "NWHI", "MHI", "SAMOA", "FLORIDA", "PRICO", "USVI")

df_samples <- statDat %>% 
  count(OCEAN, JURISDICTION, REGION) %>% 
  mutate(Ocean = OCEAN, Jurisdiction = JURISDICTION, `Sub-jurisdiction` = REGION)

##### VISUALISE DISTRIBUTIONS OF PREDICTORS #####
sdl <- statDat %>% 
  select(-c(c_sst_mean:Ji, 
            kd490_sd_z, npp_sd_z, sst_sd_z, wave_sd_z)) %>% 
  gather(key = covariate, value = value,
         human_pop50_log_z:wave_mean_z)
summary(sdl)

## Rename variables
sdl <- sdl %>% 
  mutate(covariate = factor(covariate))

sdl %>% 
  ggplot(aes(JURISDICTION, value, fill = JURISDICTION)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_boxplot() + 
  facet_wrap(~ covariate, scales = "free_y", ncol = 3) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Jurisdiction", y = "Standardized value")

ggsave(paste(path_to_figs, my_scale, "covariate_distributions.pdf", sep = ""),
       height = 8, width = 7)

##### CHECK FOR COVARYING PREDICTORS #####

## All covariates
covariates_all <- statDat %>% 
  select(
    human_pop50_log_z, 
    kd490_cv_z, 
    kd490_mean_z, 
    kd490_sd_z, 
    land_area50_log_z, 
    nAll_30yr_z, 
    npp_cv_z, 
    npp_mean_z, 
    npp_sd_z, 
    sst_cv_z, 
    sst_mean_z,
    sst_sd_z, 
    wave_cv_z, 
    wave_sd_z, 
    wave_mean_z)

# Variance inflation factors
covariates_all %>% corvif()
# Plot correlation matrix
cor_mat <- covariates_all %>% cor()
ggcorrplot(cor_mat, hc.order = FALSE, type = "lower",
           lab = TRUE, show.legend = T, 
           lab_size = 3, legend.title = "", 
           tl.cex = 8) 
ggsave(paste(path_to_figs, my_scale, "pairs_plot_variables.pdf", sep = ""), 
       height = 7, width = 7)

## Subset covariates
covariates_sub <- statDat %>% 
  select(
    human_pop50_log_z, 
    kd490_cv_z, 
    kd490_mean_z, 
    #kd490_sd_z, 
    land_area50_log_z, 
    nAll_30yr_z, 
    npp_cv_z, 
    npp_mean_z, 
    #npp_sd_z, 
    sst_cv_z, 
    sst_mean_z,
    #sst_sd_z, 
    wave_cv_z, 
    #wave_sd_z, 
    wave_mean_z)

# Number of complete rows
column_n <- colSums(!is.na(covariates_sub))

# Variance inflation factors
vifs <- covariates_sub %>% corvif()
vif_table <- vifs %>% 
  as_tibble() %>%
  mutate(covariate = rownames(vifs), 
         column_n = column_n)

# Plot correlation matrix
cor_mat <- covariates_sub %>% cor()
ggcorrplot(cor_mat, hc.order = FALSE, type = "lower",
           lab = TRUE, show.legend = T, 
           lab_size = 3, legend.title = "", 
           tl.cex = 8) 
ggsave(paste(path_to_figs, my_scale, "pairs_plot_variables2.pdf", sep = ""), 
       height = 7, width = 7)

## Final covariates for vif table
covariates_final <- statDat %>% 
  select(
    human_pop50_log_z, 
    kd490_cv_z, 
    kd490_mean_z, 
    #kd490_sd_z, 
    #land_area50_log_z, 
    nAll_30yr_z, 
    npp_cv_z, 
    npp_mean_z, 
    #npp_sd_z, 
    sst_cv_z, 
    #sst_mean_z,
    #sst_sd_z, 
    wave_cv_z, 
    #wave_sd_z, 
    wave_mean_z)

# Subset incomplete rows
covariates_final <- covariates_final[complete.cases(covariates_sub), ]

# Variance inflation factors
vifs <- covariates_final %>% corvif()
vif_final <- vifs %>% 
  as_tibble() %>%
  mutate(covariate = rownames(vifs)) %>% 
  rename(GVIF_final = GVIF)
vif_final <- left_join(vif_table, vif_final, by = "covariate") %>% 
  select(covariate, column_n, GVIF, GVIF_final)
vif_final
write.csv(vif_final, paste(path_to_output, my_scale, "vif_table.csv", sep = ""))
