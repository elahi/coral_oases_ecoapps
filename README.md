# coral_oases_ecoapps

Data and code used in:

Scale dependence of coral reef oases and their environmental correlates

Robin Elahi, Peter J. Edmunds, Ruth D. Gates, Ilsa B. Kuffner, Brian B. Barnes, Iliana Chollett, Travis A. Courtney, James R. Guest, Elizabeth A. Lenz, Lauren T. Toth, T. Shay Viehman, Ivor D. Williams

In press at Ecological Applications. 

To reproduce analyses and figures in manuscript, refer to folder 'analyse_oases_global'. 

For maps of the probability of oasis occurrence for all sub-regions, refer to folder 'map_subregions'. 

Please cite the above manuscript if the provided data are used for research or education. 

## Overview of folders in this repository

### 1_process_cortad

  - R scripts to process CORTAD data, used in preliminary analyses for comparison with remote sensing data processed by Barnes (e.g., SST, Kd490)
  
### 1_process_msec

  - R scripts to process MSEC data from Yeager et al. 
  
### 1_process_noaa

  - R scripts to process NOAA data received from collaborators (Viehman, Williams)
  
### 2_compile_data

  - R scripts to compile all datasets
  
### 3_adjust_regions_2021  

  - R scripts to adjust categorical designations of regions
  
### 3_prep_global_data_2021

  - R scripts to prepare data for global analysis used for manuscript submitted to Ecological Applications
  
### analyse_oases_global

  - R scripts to assign reef oases, explore the data, run the hierarchical occupancy model, extract output from the model, make figures and tables
  
  - analysis figs: figures (those with FIG preceding the file name appear in the main body of the manuscript)
    
  - analysis_output: summary tables of analyses
  
  - map_cell_probs: maps of oasis occurrence probabilities for Florida (Figure 6)
    
  - map_subregions: maps of oasis occurrence probabilities for all subregions
  
  - model output
    - model_summaries: summaries of MCMC iterations
    - plot_diagnostics: posterior predictive plots
    - plot_trace: MCMC trace plots
    
  - model_rds: JAGS output, saved as RDS file
  
  - models: model matrices and hierarchical occupancy model, written for JAGS

### data_output

  - cortad_: cortad SST data, in csv format
  - cover_species_list.csv: coral cover, by species
  - df_samples.csv: summary table of sample sizes associated with each spatial scale
  - noaa_ll_date.csv: lat-longs for noaa reef surveys

### ignore_folder (ignored on github due to large size)

  - ecological data from NOAA (Viehman, Williams)
  - storm data (Chollett)
  - NASA remote sensing data (Barnes)
  - model_rds: MCMC results from JAGS models 
  
### kml

  - kml files for Florida map

### map_subregions

  - supplemental maps of psi for subregions

### powell2_supplement

 - Appendix S2 includes the details of the hierarchical occupancy model
 
### R

  - accessory R scripts 
  
### sandbox

  - old scripts and figures not used in manuscript
  
### workspace

  - data necessary to recreate all of the analyses and figures in the manuscript
