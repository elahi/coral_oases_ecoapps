################################################################################
##' @title Run all models
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-30
##' @log 
################################################################################

source("analyse_oases_global/00_set_paths.R")

library(beepr)
beep(8)

## Run models
source(paste(my_path, "03_occu_hier_binom_zed0.R", sep = "")) # 19
end_time - start_time
source(paste(my_path, "03_occu_hier_binom_zed1.R", sep = "")) # 17
end_time - start_time
source(paste(my_path, "03_occu_hier_binom_zed2.R", sep = "")) # 18
end_time - start_time
source(paste(my_path, "03_occu_hier_binom_zed3.R", sep = "")) # 
end_time - start_time

beep(8)

## Extract model summaries
source(paste(my_path, "04_extract_model_results_all.R", sep = "")) 

beep(8)


