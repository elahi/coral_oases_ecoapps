

#### RAW DATA ####

## For raw data, after sourcing 01_load_coral_cover_data

rename_cover_data <- function(d){
  
  # Relevel and recode Ocean
  d$Ocean <- fct_relevel(d$Ocean, "PACIFIC")
  d$Ocean <- fct_recode(d$Ocean, `Pacific` = "PACIFIC", `Western Atlantic` = "CARIB")
  
  # Relevel and recode Jurisdictions (= Regions for the paper)
  d$Jurisdiction <- fct_relevel(d$Jurisdiction,
                                   "US Virgin Islands", "Puerto Rico", "Florida", 
                                   "Mariana Islands", "Northwest\nHawaiian Islands", 
                                   "Main\nHawaiian Islands", "American Samoa")
  
  d$Jurisdiction <- fct_rev(d$Jurisdiction)
  
  d <- d %>% mutate(Region = Jurisdiction)

  # Relevel and recode Sub-jurisdictions (= Sub-regions for the paper)
  d$Subregion <- fct_recode(d$REGION, 
                                     `Manua Islands` = "Manua_islands", 
                                     `Marianas-middle` = "Marianas_middle", 
                                     `Marianas-lower` = "Marianas_lower", 
                                     `Marianas-upper` = "Marianas_upper", 
                                     `Keys-upper` = "Keys_upper", 
                                     `Keys-middle` = "Keys_middle", 
                                     `Keys-lower` = "Keys_lower", 
                                     `Southeast Florida` = "SEFCRI", 
                                     `Puerto Rico-north` = "NPRICO", 
                                     `Puerto Rico-east` = "EPRICO", 
                                     `Puerto Rico-southwest` = "SWPRICO", 
                                     `St. Croix` = "STX", 
                                     `St. John` = "STJ", 
                                     `St. Thomas` = "STT", 
                                     `St. J & St. T offshore` = "STT_STJ_offshore")
  
  return(d)
}

## Test
# source("analyse_oases_global/01_load_coral_cover_data.R")
# dat3 <- rename_cover_data(dat2)
# levels(dat3$Ocean)
# levels(dat3$Region)
# levels(dat3$Subregion)

#### GRIDDED DATA ####

## For gridded data, e.g., grid_df_w0
#d <- fit_coefs

rename_oasis_data <- function(d){
  
  d <- d %>% 
    mutate(Ocean = OCEAN, 
           Jurisdiction = as.factor(JURISDICTION))
  
  # Relevel and recode Ocean
  d$Ocean <- fct_relevel(d$Ocean, "PACIFIC")
  d$Ocean <- fct_recode(d$Ocean, `Pacific` = "PACIFIC", `Western Atlantic` = "CARIB")
  #levels(d$Ocean)
  
  # Relevel and recode Jurisdictions (= Regions for the paper)
  #levels(d$Jurisdiction)
  d$Jurisdiction <- fct_relevel(d$Jurisdiction, 
                                "USVI", "PRICO", "FLORIDA",
                                "MARIAN", "NWHI", "MHI", "SAMOA")
  levels(d$Jurisdiction) <- c("US Virgin Islands", "Puerto Rico", 
                               "Florida", "Mariana Islands", 
                               "Northwest\nHawaiian Islands", 
                               "Main\nHawaiian Islands", 
                               "American Samoa")
  d$Jurisdiction <- fct_rev(d$Jurisdiction)
  
  d <- d %>% mutate(Region = Jurisdiction)
  
  # Relevel and recode Sub-jurisdictions (= Sub-regions for the paper)
  d$Subregion <- fct_recode(d$REGION, 
                            `Manua Islands` = "Manua_islands", 
                            `Marianas-middle` = "Marianas_middle", 
                            `Marianas-lower` = "Marianas_lower", 
                            `Marianas-upper` = "Marianas_upper", 
                            `Keys-upper` = "Keys_upper", 
                            `Keys-middle` = "Keys_middle", 
                            `Keys-lower` = "Keys_lower", 
                            `Southeast Florida` = "SEFCRI", 
                            `Puerto Rico-north` = "NPRICO", 
                            `Puerto Rico-east` = "EPRICO", 
                            `Puerto Rico-southwest` = "SWPRICO", 
                            `St. Croix` = "STX", 
                            `St. John` = "STJ", 
                            `St. Thomas` = "STT", 
                            `St. J & St. T offshore` = "STT_STJ_offshore")
  
  return(d)
}

## Test
# source("analyse_oases_global/01_load_coral_cover_data.R")
#d <- read.csv("workspace/grid_df_w0.csv")
# levels(dat3$Ocean)
# levels(dat3$Region)
# levels(dat3$Subregion)

