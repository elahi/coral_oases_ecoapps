################################################################################
##' @title Plot psi (probability of oasis occurrence)
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-08-27
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
my_threshold <- "z2_"
this_model <- "occu_hier_binom_"

##### PACKAGES, DATA #####
library(tidyverse)
library(tidybayes)
library(viridis)
library(forcats)
library(rjags)
source("R/inverse_logit.R")
source("R/renaming_functions.R")
source("R/ggplot_settings.R")

theme_set(theme_bw(base_size = 14) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

##### GET COVARIATE NAMES #####

# Prep data
dat <- read.csv("workspace/grid_df_w1.csv")

df_jurisdiction_region <- dat %>% 
  distinct(JURISDICTION, REGION)

# Create sample area groups
dat <- dat %>% mutate(group_j = as.integer(as.factor(REGION)))
dat_groups <- dat %>% 
  select(REGION, group_j) %>% distinct()

##### COMPILE NAIVE OCCUPANCY AT EACH SCALE #####

dat <- read.csv("workspace/grid_df_w0.csv")
occu0 <- dat %>% 
  group_by(OCEAN, JURISDICTION, REGION) %>% 
  summarise(oasis_prop = mean(oasis_present)) %>% 
  ungroup() %>% 
  mutate(scale = "Cross-basin")

dat <- read.csv("workspace/grid_df_w1.csv")
occu1 <- dat %>% 
  group_by(OCEAN, JURISDICTION, REGION) %>% 
  summarise(oasis_prop = mean(oasis_present)) %>% 
  ungroup() %>% 
  mutate(scale = "Basin")

dat <- read.csv("workspace/grid_df_w2.csv")
occu2 <- dat %>% 
  group_by(OCEAN, JURISDICTION, REGION) %>% 
  summarise(oasis_prop = mean(oasis_present)) %>% 
  ungroup() %>% 
  mutate(scale = "Region")

dat <- read.csv("workspace/grid_df_w3.csv")
occu3 <- dat %>% 
  group_by(OCEAN, JURISDICTION, REGION) %>% 
  summarise(oasis_prop = mean(oasis_present)) %>% 
  ungroup() %>% 
  mutate(scale = "Sub-region")

occu_df <- rbind(occu0, occu1, occu2, occu3)
occu_df <- rename_oasis_data(d = occu_df)
occu_df$scale <- fct_relevel(occu_df$scale, c("Cross-basin", "Basin", "Region", "Sub-region"))

##### COMPILE COEFS #####

my_files <- list.files(path = path_to_model_output, pattern = "psi_coefs_prob_df")
my_files

## Get model fits
fit <- read.csv(file = paste(path_to_model_output, my_files[1], sep = ""))
fit0_coefs <- fit %>% mutate(scale = "Cross-basin")

fit <- read.csv(file = paste(path_to_model_output, my_files[2], sep = ""))
fit1_coefs <- fit %>% mutate(scale = "Basin")

fit <- read.csv(file = paste(path_to_model_output, my_files[3], sep = ""))
fit2_coefs <- fit %>% mutate(scale = "Region")

fit <- read.csv(file = paste(path_to_model_output, my_files[4], sep = ""))
fit3_coefs <- fit %>% mutate(scale = "Sub-region")

fit_coefs <- rbind(fit0_coefs, fit1_coefs, fit2_coefs, fit3_coefs) 

fit_coefs <- cbind(fit_coefs, 
                    cell_2.5_original = rep(dat$cell_2.5, 8),
                    JURISDICTION = rep(dat$JURISDICTION, 8), 
                    REGION = rep(dat$REGION, 8))
names(fit_coefs)

sum(fit_coefs$cell_2.5 == fit_coefs$cell_2.5_original) == nrow(fit_coefs)

## Add Ocean variable
unique(fit_coefs$JURISDICTION)
carib <- c("PRICO", "USVI", "FLORIDA")
fit_coefs <- fit_coefs %>% 
  mutate(OCEAN = ifelse(JURISDICTION %in% carib, "CARIB", "PACIFIC"), 
         Ocean = ifelse(JURISDICTION %in% carib, "western Atlantic", "Pacific"))

## Reorder scale
unique(fit_coefs$scale)
fit_coefs <- fit_coefs %>%
  mutate(scale = factor(scale, levels = c("Cross-basin", "Basin", "Region", "Sub-region")))
levels(fit_coefs$scale)

## Rename and relevel JURISDICTIONS and REGIONS
fit_coefs <- rename_oasis_data(fit_coefs)
levels(fit_coefs$Region)
levels(fit_coefs$Subregion)

## Simplify dataframe
fit_coefs <- fit_coefs %>%
  select(-c(OCEAN, JURISDICTION, REGION, Jurisdiction, cell_2.5_original))
names(fit_coefs)

#### PLOT ####

## Reorder by Jurisdiction then values, by zed2
fit_coefs_order_df <- fit_coefs %>% 
  filter(scale == "Cross-basin" & .width == 0.95) %>% 
  group_by(Region, Subregion) %>% 
  summarise(mean_value = mean(.value)) %>% 
  ungroup() %>% 
  arrange(Region, desc(mean_value)) %>% 
  mutate(variable2 = factor(Subregion, levels = unique(Subregion)))

fit_coefs_order_df$variable2 <- fct_rev(fit_coefs_order_df$variable2)
levels(fit_coefs_order_df$variable2)

fit_coefs_order_df
levels(fit_coefs_order_df$Region)
levels(fit_coefs_order_df$Subregion)
levels(fit_coefs_order_df$variable2)

levels(occu_df$Region)
levels(occu_df$Subregion)

fit_coefs2 <- fit_coefs_order_df %>%
  select(Subregion, variable2) %>%
  left_join(fit_coefs, ., by = c("Subregion"))

## Add in naive occupancy
names(occu_df)
names(fit_coefs_order_df)

occu_df2 <- fit_coefs_order_df %>%
  select(Subregion, variable2) %>%
  left_join(occu_df, ., by = c("Subregion"))

fit_coefs2 %>%
  ggplot(aes(y = variable2, x = .value, fill = Region)) +
  geom_vline(xintercept = c(0, 0.2, 0.4, 0.6, 0.8, 1), color = "gray90") + 
  geom_boxplot(outlier.shape = NA, coef = 0) + 
  geom_point(aes(x = oasis_prop), data = occu_df2, pch = 23) + 
  facet_wrap(~ scale, ncol = 4) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  labs(x = expression("Probability of oasis occurrence"~(psi)), 
       y = "")  + 
  guides(shape = FALSE) + 
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  scale_fill_brewer(type = "div", palette = "RdYlBu", direction = 1) + 
  geom_hline(yintercept = c(4.5, 7.5, 12.5, 17.5, 21.5, 29.5), color = "black", size = 0.25)

ggsave(paste(path_to_figs, "FIG_", this_model, "psi_prob_scale.pdf", 
             sep = ""), height = 8, width = 8)

## Save the order_df
write.csv(fit_coefs_order_df, paste(path_to_output, "fit_coefs_order_df.csv", sep = ""))
