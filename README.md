# coral_oases_ecoapps

Data and code used in:

Coral reef oases are consistently associated with high light attenuation

Robin Elahi, Peter J. Edmunds, Ruth D. Gates, Ilsa B. Kuffner, Brian B. Barnes, Iliana Chollett, Travis A. Courtney, James R. Guest, Elizabeth A. Lenz, Lauren T. Toth, T. Shay Viehman, Ivor D. Williams

In review at Ecological Applications. 

Email elahi at stanford.edu with questions

## Overview of folders in this repository

### analyse_oases_global

  - R scripts to assign reef oases, explore the data, run the hierarchical occupancy model, extract output from the model, make figures and tables
  
  - analysis figs: figures (those with FIG preceding the file name appear in the main body of the manuscript)
    
  - analysis_output: summary tables of analyses
  
  - map_cell_probs: maps of oasis occurrence probabilities for Florida (Figure 6)
  
  - covariates_subjurisdiction: plots of covariates at the subjurisdictional scale
  
  - map_covariates: example maps of covariates
  
  - model output
    - model_summaries: summaries of MCMC iterations
    - plot_diagnostics: posterior predictive plots
    - plot_trace: MCMC trace plots
    
  - model_rds: JAGS output, saved as RDS file
  
  - models: hierarchical occupancy model, written for JAGS

### data_output

  - df_samples.csv: summary table of sample sizes associated with each spatial scale

### ignore_folder (ignored on github due to large size)

  - model_rds: MCMC results from JAGS models 
  
### kml

  - kml files for Florida map

### powell2_supplement

 - Appendix S2 includes the details of the hierarchical occupancy model
 
### R

  - accessory R scripts 
  
### workspace

  - data (and metadata) necessary to recreate all of the analyses and figures in the manuscript
