################################################################################
##' @title Plot eta coefs (probability of oasis detection)
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-08-26
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
# my_threshold <- "z2_"
# this_model <- "occu_hier_binom_"
# 
# ##### PACKAGES, DATA #####
# library(tidyverse)
# library(tidybayes)
# library(viridis)
# library(forcats)
# library(rjags)
# source("R/inverse_logit.R")
# source("R/renaming_functions.R")
# source("R/ggplot_settings.R")
# 
# theme_set(theme_bw(base_size = 14) + 
#             theme(panel.grid = element_blank(), 
#                   strip.background = element_blank())
# )

# Load this to get ordering file
source(paste(my_path, "05_FIG_plot_coefs_prob_psi.R", sep = ""))

##### GET COVARIATE NAMES #####

# Prep data
dat <- read.csv("workspace/grid_df_w1.csv")

df_jurisdiction_region <- dat %>% 
  distinct(JURISDICTION, REGION)

# Create sample area groups
dat <- dat %>% mutate(group_j = as.integer(as.factor(REGION)))
dat_groups <- dat %>% 
  select(REGION, group_j) %>% distinct()

##### COMPILE ETA COEFS #####

my_files <- list.files(path = path_to_model_output, pattern = "eta_coefs_prob")
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
names(fit_coefs)

## Add Ocean variable
unique(fit_coefs$JURISDICTION)
carib <- c("PRICO", "USVI", "FLORIDA")
fit_coefs <- fit_coefs %>% 
  mutate(OCEAN = ifelse(JURISDICTION %in% carib, "CARIB", "PACIFIC"), 
         Ocean = ifelse(JURISDICTION %in% carib, "western Atlantic", "Pacific"), 
         REGION = as.factor(group))

## Reorder scale
unique(fit_coefs$scale)
fit_coefs <- fit_coefs %>%
  mutate(scale = factor(scale, levels = c("Cross-basin", "Basin", "Region", "Sub-region")))
levels(fit_coefs$scale)

## Rename and relevel JURISDICTIONS and REGIONS
names(fit_coefs)
fit_coefs <- rename_oasis_data(fit_coefs)
levels(fit_coefs$Region)
levels(fit_coefs$Subregion)

## Simplify dataframe
fit_coefs <- fit_coefs %>%
  select(-c(OCEAN, JURISDICTION, REGION, Jurisdiction, group))
names(fit_coefs)

###### REORDERED PLOT #####

# ## Load ordering from psi plot
# fit_coefs_order_df <- read.csv(paste(path_to_output, "fit_coefs_order_df.csv", sep = "")) %>% 
#   mutate(Region = as.factor(Region), 
#          Subregion = as.factor(Subregion))
# levels(fit_coefs_order_df$Region)
# 
# levels(fit_coefs_order_df$Region) <- c("US Virgin Islands", "Puerto Rico", 
#                             "Florida", "Mariana Islands", 
#                             "Northwest\nHawaiian Islands", 
#                             "Main\nHawaiian Islands", 
#                             "American Samoa")
# 
# fit_coefs_order_df$Region <- fct_rev(fit_coefs_order_df$Region)
# 
# fit_coefs_order_df <- fit_coefs_order_df %>% 
#   arrange(Region, desc(mean_value)) %>% 
#   mutate(variable2 = factor(Subregion, levels = unique(Subregion)))
# 
# fit_coefs_order_df$variable2 <- fct_rev(fit_coefs_order_df$variable2)
# 
# fit_coefs_order_df
# levels(fit_coefs_order_df$Region)
# levels(fit_coefs_order_df$Subregion)
# levels(fit_coefs_order_df$variable2)

fit_coefs2 <- fit_coefs_order_df %>%
  select(Subregion, variable2) %>%
  left_join(fit_coefs, ., by = c("Subregion"))

levels(fit_coefs2$variable2)

fit_coefs2 %>%
  ggplot(aes(y = variable2, x = .value, fill = Region)) +
  geom_vline(xintercept = c(0, 0.2, 0.4, 0.6, 0.8, 1), color = "gray90") + 
  geom_pointinterval(aes(xmin = .lower, xmax = .upper), fatten_point = 1.4) + 
  geom_point(size = 3, pch = 21) + 
  facet_wrap(~ scale, ncol = 4) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  labs(x = expression("Probability of oasis detection"~(italic(p))), 
       y = "")  +   
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) +
  scale_fill_brewer(type = "div", palette = "RdYlBu", direction = 1) + 
  geom_hline(yintercept = c(4.5, 7.5, 12.5, 17.5, 21.5, 29.5), color = "black", size = 0.25)

ggsave(paste(path_to_figs, "FIG_", this_model, "eta_prob_scale.pdf", 
             sep = ""), height = 8, width = 8)
