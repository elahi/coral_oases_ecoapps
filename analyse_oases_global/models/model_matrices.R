################################################################################
##' @title Model matrices
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2021-08-30
##' @log 
################################################################################

if(my_scale == "zed0_"){
  
  X <- model.matrix(~ human_pop50_log_z +
                      kd490_mean_z + 
                      kd490_cv_z + 
                      nAll_30yr_z + 
                      npp_cv_z + 
                      npp_mean_z + 
                      sst_cv_z +
                      wave_cv_z + 
                      wave_mean_z, data = dat)
}

if(my_scale == "zed1_"){
  X <- model.matrix(~ human_pop50_log_z +
                      kd490_mean_z + 
                      kd490_cv_z + 
                      nAll_30yr_z + 
                      npp_cv_z + 
                      npp_mean_z + 
                      sst_cv_z +
                      wave_cv_z + 
                      wave_mean_z, data = dat)
}

if(my_scale == "zed2_" | my_scale == "zed3_"){
  X <- model.matrix(~ human_pop50_log_z +
                      kd490_mean_z + 
                      kd490_cv_z + 
                      nAll_30yr_z + 
                      npp_cv_z + 
                      npp_mean_z + 
                      sst_mean_z +
                      sst_cv_z + 
                      wave_cv_z + 
                      wave_mean_z, data = dat)
}
