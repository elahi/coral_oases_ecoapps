################################################################################
##' @title Test effect of depth on coral cover
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2020-12-04
##' @log 2021-08-06: removed duplicate sites
################################################################################

##### PATHS #####
source("analyse_oases_global/00_set_paths.R")

source(paste(my_path, "01_load_coral_cover_data.R", sep = ""))
source("R/oasis_functions.R")

library(lme4)

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank())
)

## Ocean colors
ocean_pal <- c("#91bfdb", "#fc8d59")

###### PLOT ######

dat2$Jurisdiction <- fct_relevel(dat2$Jurisdiction,
                                 "US Virgin Islands", "Puerto Rico", "Florida", 
                                 "Mariana Islands", "Northwest\nHawaiian Islands", 
                                 "Main\nHawaiian Islands", "American Samoa")

dat2$Jurisdiction <- fct_rev(dat2$Jurisdiction)

dat2 %>% 
  ggplot(aes(DEPTH, coral_cover, color = Ocean)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(color = "black") + 
  facet_wrap(~ Jurisdiction, ncol = 2) + 
  labs(x = "Depth (m)", y = "Coral cover (%)") + 
  scale_color_manual(values = ocean_pal) + 
  theme(legend.position = "none") 

ggsave(paste(path_to_figs, "depth_cover_scatter_jurisdiction.pdf", sep = ""), height = 7, width = 5)

ggsave("00_analyse_oases_global/analysis_figs/depth_cover_scatter_jurisdiction.pdf", height = 7, width = 5)

##### MIXED EFFECTS MODELS #####

## Note that REGION = Jurisdiction

dat2 %>% count(REGION)
dat2 <- dat2 %>% drop_na(DEPTH)

# Standardize depths
dat2 <- dat2 %>% 
  mutate(DEPTH_z = scale2(DEPTH))

par(mfrow = c(1,2))
hist(dat2$DEPTH, breaks = 20)
hist(dat2$DEPTH_z, breaks = 20)

m1a <- lmer(coral_cover ~ 1 + (1 | REGION), data = dat2, REML = FALSE)
m1b <- lmer(coral_cover ~ DEPTH_z + (1 | REGION), data = dat2, REML = FALSE)
summary(m1a)
anova(m1a, m1b)
plot(m1b)

