################################################################################
##' @title Test effect of year on oasis designation
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-12-07
##' @log 2021-08-06: removed duplicate sites
################################################################################

##### PATHS #####
# Set paths
source("analyse_oases_global/00_set_paths.R")
source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))

source("R/oasis_functions.R")
source("R/renaming_functions.R")
source("R/ggplot_settings.R")

library(lme4)
library(forcats)
library(patchwork)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

###### PLOT ######

# Rename data
dat3 <- rename_cover_data(dat2)
dat3 %>% count(Ocean)
levels(dat3$Ocean)
levels(dat3$Region)

# Year as character
dat3 <- dat3 %>% mutate(year_cat = as.character(YEAR))

p1 <- dat3 %>% 
  ggplot(aes(year_cat, coral_cover, fill = Region)) + 
  geom_boxplot(alpha = 1, position = position_dodge(preserve = "single"), 
               outlier.size = 0.5, outlier.shape = 1) + 
  facet_wrap(~ Ocean, ncol = 2) + 
  labs(x = "Year", y = "Coral cover (%)") +
  scale_fill_brewer(type = "div", palette = "RdYlBu", direction = 1)
p1

ggsave(paste(path_to_figs, "check_year_cover_boxplot.pdf", sep = ""), height = 3.5, width = 7)

##### SUMMARIZE DATA #####

sdat0 <- dat3 %>% 
  count(Ocean, Region, YEAR, oasis0) %>% 
  spread(key = "oasis0", value = "n", fill = 0) %>% 
  mutate(n_sites = not_oasis + oasis, 
         oasis_per = oasis / n_sites * 100) %>% 
  mutate(scale = "Cross-basin")


sdat1 <- dat3 %>% 
  count(Ocean, Region, YEAR, oasis1) %>% 
  spread(key = "oasis1", value = "n", fill = 0) %>% 
  mutate(n_sites = not_oasis + oasis, 
         oasis_per = oasis / n_sites * 100) %>% 
  mutate(scale = "Basin")


sdat2 <- dat3 %>% 
  count(Ocean, Region, YEAR, oasis2) %>% 
  spread(key = "oasis2", value = "n", fill = 0) %>% 
  mutate(n_sites = not_oasis + oasis, 
         oasis_per = oasis / n_sites * 100) %>% 
  mutate(scale = "Region")


sdat3 <- dat3 %>% 
  count(Ocean, Region, YEAR, oasis3) %>% 
  spread(key = "oasis3", value = "n", fill = 0) %>% 
  mutate(n_sites = not_oasis + oasis, 
         oasis_per = oasis / n_sites * 100) %>% 
  mutate(scale = "Sub-region")

sdat <- rbind(sdat0, sdat1, sdat2, sdat3)
sdat

p2 <- sdat %>% 
  ggplot(aes(YEAR, oasis_per, fill = Region, size = n_sites)) + 
  geom_line(aes(color = Region), size = 0.5) +
  geom_point(pch = 21, alpha = 0.75) + 
  facet_wrap(~ scale, ncol = 1) + 
  scale_size_area() + 
  labs(x = "Year", y = "Percent of reefs classifed as oases") + 
  scale_y_continuous(limits = c(-2, 26)) + 
  guides(size = guide_legend(title = "N (reefs)")) 
p2

ggsave(paste(path_to_figs, "check_year_oasis_dotplot.pdf", sep = ""), height = 6, width = 5)

##### MIXED EFFECTS MODELS #####

## Note that REGION = Sub-jurisdiction

dat2 %>% count(REGION)
dat2 %>% count(YEAR)
dat2 %>% count(REGION, YEAR)

# Center year
dat2 <- dat2 %>% 
  mutate(YEAR_c = YEAR - 2012)

m1a <- lmer(coral_cover ~ 1 + (1 | Jurisdiction), data = dat2, REML = FALSE)
m1b <- lmer(coral_cover ~ YEAR_c + (1 | Jurisdiction), data = dat2, REML = FALSE)
summary(m1b)
anova(m1a, m1b)
plot(m1b)

