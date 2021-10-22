################################################################################
##' @title Plot null and model oasis selection results
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-09-10
##' @log 
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")
my_threshold <- "z2_"
this_model <- "occu_hier_binom"

##### PACKAGES #####
library(tidyverse)
library(tidybayes)
library(viridis)
library(forcats)
library(broom)
library(patchwork)
library(ggdist)

theme_set(theme_bw(base_size = 11) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

source("R/inverse_logit.R")

##### COMPILE RESULTS #####

my_files <- list.files(path = path_to_output, pattern = "null_model")
my_files

## Get model fits
fit <- read.csv(file = paste(path_to_output, my_files[1], sep = ""))
fit0 <- fit %>% mutate(scale = "Cross-basin")

fit <- read.csv(file = paste(path_to_output, my_files[2], sep = ""))
fit1 <- fit %>% mutate(scale = "Basin")

fit <- read.csv(file = paste(path_to_output, my_files[3], sep = ""))
fit2 <- fit %>% mutate(scale = "Region")

fit <- read.csv(file = paste(path_to_output, my_files[4], sep = ""))
fit3 <- fit %>% mutate(scale = "Sub-region")

fit_df <- rbind(fit0, fit1, fit2, fit3) %>% tibble()

## Reorder jurisdiction
unique(fit_df$scale)
fit_df <- fit_df %>%
  mutate(scale = factor(scale, levels = c("Cross-basin", "Basin", "Region", "Sub-region"))) %>%
  mutate(scale = fct_rev(scale)) %>%
  mutate(model_null_percentage_ratio = model_percentage / null_percentage)

#### BOXPLOTS ####

ratio_summary <- fit_df %>%
  group_by(scale) %>%
  summarise(median = median(model_null_percentage_ratio), 
            mean = mean(model_null_percentage_ratio))
ratio_summary

naive_median <- fit_df %>% 
  filter(scale == "Cross-basin") %>%
  summarise(median = median(null_percentage)) %>% 
  as.numeric()

fit_df_long <- fit_df %>% 
  select(scale, null_percentage, model_percentage) %>%  
  rename(Null = null_percentage, Model = model_percentage) %>%
  gather(key = "null_or_model", value = "percent_success", Null:Model) %>% 
  as_tibble()

fit_df_long <- fit_df_long %>%
  mutate(scale = factor(scale, levels = c("Cross-basin", "Basin", "Region", "Sub-region")), 
         null_or_model = factor(null_or_model, levels = c("Null", "Model"))) 

my_dodge <- position_dodge(width = 0.75)

fit_df_long %>% 
  ggplot(aes(scale, percent_success, fill = null_or_model)) + 
  #geom_hline(yintercept = naive_median, linetype = "dashed", color = "gray75") + 
  geom_boxplot(position = my_dodge, outlier.shape = 21, 
               outlier.alpha = 0.2, outlier.fill = NA) + 
  labs(y = "Oases detected in chosen cells (%)", 
       x = "Spatial extent") +
  scale_fill_manual(values = c("white", "gray"), guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position = "top", 
        legend.title = element_blank()) +
  #scale_y_continuous(limits = c(0, 50)) +
  # geom_text(data = ratio_summary, 
  #           aes(x = scale, y = 2, group = scale, 
  #               label = paste(round(median, 1), "-fold \nimprovement", sep = "")), 
  #           inherit.aes = FALSE, size = 2.5) + 
  theme(legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.background = element_blank()) 

ggsave(paste(path_to_figs, "FIG_model_null_ncells_boxplot.pdf", sep = ""), 
       height = 3.5, width = 4)

fit_df_long %>% 
  group_by(null_or_model, scale) %>% 
  summarise(median = median(percent_success))

#### RATIO ####

fit_df %>% 
  ggplot(aes(y = scale, x = model_null_ratio)) + 
  stat_halfeye(.width = c(0.8, 0.95), 
               aes(fill = stat(cut_cdf_qi(cdf, .width = c(0.8, 0.95), 
                                     labels = scales::percent_format())))) +
  scale_fill_brewer(direction = -1, na.translate = FALSE) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.4) + 
  coord_cartesian(xlim = c(0.5, 4.5)) + 
  labs(x = "Ratio of oasis detection (model / null)", 
       y = "Spatial extent", 
       fill = "Interval") +
  theme(legend.position = c(0.95, 0.05), 
        legend.justification = c(1,0), 
        legend.title = element_blank())

fit_df %>% 
  ggplot(aes(y = scale, x = model_null_ratio)) + 
  stat_halfeye(.width = c(0.8, 0.95), 
               aes(fill = stat(x > 1))) +
  scale_fill_manual(values = c("gray85", "skyblue")) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.4) + 
  coord_cartesian(xlim = c(0.5, 4.5)) + 
  labs(x = "Ratio of oasis detection (model / null)", 
       y = "Spatial extent", 
       fill = "Ratio > 1") +
  theme(legend.position = c(0.95, 0.05), 
        legend.justification = c(1,0))

ggsave(paste(path_to_figs, "model_null_ncells_ratio.pdf", sep = ""), 
       height = 3.5, width = 4)

#### LOG RATIO ####

fit_df %>% 
  ggplot(aes(y = scale, x = lrr)) + 
  stat_halfeye(.width = c(0.8, 0.95), 
               aes(fill = stat(cut_cdf_qi(cdf, .width = c(0.8, 0.95), 
                                          labels = scales::percent_format())))) +
  scale_fill_brewer(direction = -1, na.translate = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.4) + 
  coord_cartesian(xlim = c(-0.5, 1.5)) + 
  labs(x = "Log ratio of oasis detection (model / null)", 
       y = "Spatial extent", 
       fill = "Interval") +
  theme(legend.position = c(0.95, 0.05), 
        legend.justification = c(1,0), 
        legend.title = element_blank())

fit_df %>% 
  ggplot(aes(y = scale, x = lrr)) + 
  stat_halfeye(.width = c(0.8, 0.95), 
               aes(fill = stat(x > 0))) +
  scale_fill_manual(values = c("gray85", "skyblue")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.4) + 
  coord_cartesian(xlim = c(-0.4, 1.5)) + 
  labs(x = "Log ratio of oasis detection (model / null)", 
       y = "Spatial extent", 
       fill = "Log ratio > 0") +
  theme(legend.position = c(0.975, 0.025), 
        legend.justification = c(1,0))

ggsave(paste(path_to_figs, "model_null_ncells_logratio.pdf", sep = ""), 
       height = 3.5, width = 4)


