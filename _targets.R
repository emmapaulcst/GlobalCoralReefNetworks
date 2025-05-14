# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
# https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("readr", "dplyr", "tidyr", "ggplot2", "rlist", "parallel", "vegan", "bipartite",
               "purrr", "tibble", "brms", "ggpubr", "scales", "loo", "DiagrammeR", "rstantools",
               "grid", "Rcpp", "gridExtra", "cmdstanr", "magick", "ggfortify", "ggcorrplot"))

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
tar_source("R/functions_mat_int.R")
tar_source("R/functions_topo.R")
tar_source("R/functions_bga.R")
tar_source("R/functions_flux_per_cat.R")
tar_source("R/functions_sem.R")
tar_source("R/functions_sem_zscores.R")
tar_source("R/functions_otherSEMs.R")
tar_source("R/functions_makeFigs.R")
tar_source("R/functions_makeSupp.R")
tar_source("R/functions_makeSupp_DAGS.R")

list(
  #### BUILD INT MATRICES ####
  # Get global interaction matrix from ML
  tar_target(path_to_global_mat_int, "data/interaction_matrix.csv", format = "file"),
  tar_target(global_mat_int, get_global_mat_int(path_to_global_mat_int)),
  
  # Get reef fish visual censuses from RLS
  tar_target(path_to_fish_rls, "data/rls_fish_flux.csv", format = "file"),
  tar_target(fish_rls, get_fish_rls(path_to_fish_rls)),
  
  # Save selected SurveyID
  tar_target(surveyID, get_surveyID(fish_rls)),
  # Save site name
  tar_target(site, get_site(fish_rls)),
  # Save carbon fluxes
  tar_target(total_flux_per_sp_per_site, get_c_fluxes(fish_rls)),
  
  # Get all interaction matrices
  tar_target(list_mat_int, gives_list_mat_int(site, fish_rls, global_mat_int)),
  # Rmv empty columns in the matrices
  tar_target(list_mat_int_rmv, gives_list_mat_int_rmv(site, list_mat_int)),
  # Weight matrices with carbon fluxes
  tar_target(list_mat_int_rmv_w, weight_matrices(site, list_mat_int_rmv, total_flux_per_sp_per_site)),
  # Rmv empty columns to be sure
  tar_target(list_mat_int_Ic, gives_list_mat_int_rmv(site, list_mat_int_rmv_w)),
  
  #### ASSESS TOPOLOGY ####
  # It is recommended to use the topology.csv dataset here, to avoid lengthy computational time
  # tar_target(topology, get_all_topo(list_mat_int_Ic, surveyID)),
  
  tar_target(path_to_topology, "data/topology.csv", format = "file"),
  tar_target(topology, read_topology(path_to_topology)),
  
  #### BUILD BGA DATASET ####
  # Get benthos data
  tar_target(path_to_benthos, "data/rls_benthos.csv", format = "file"),
  tar_target(benthos, get_benthos(path_to_benthos, surveyID)),
  
  # Get predictors data
  tar_target(path_to_predictors, "data/rls_predictors.csv", format = "file"),
  tar_target(predictors, get_predictors(path_to_predictors, surveyID)),
  
  # get z-scores
  # The code for z-scores can be found in R/zcores.R script
  # Note that computation is lenghty and requires a server
  tar_target(path_to_z_scores, "data/z_scores.csv", format = "file"),
  tar_target(z_scores, get_z_scores(path_to_z_scores)),
  
  # Gives bga dataset
  tar_target(bga, gives_bga_dataset(topology, z_scores, benthos, predictors)),
  
  #### CARBON FLUXES ####
  # Get prey categories
  tar_target(path_to_prey_cat, "data/prey_categories.csv", format = "file"),
  tar_target(prey_cat, get_prey_cat(path_to_prey_cat)),
  
  # Gives carbon fluxes per categories
  tar_target(flux_per_prey_Ic, gives_flux_per_cat(prey_cat, list_mat_int_Ic)),
  # More info on carbon fluxes
  # tar_target(info_on_fluxes, get_info_on_fluxes(flux_per_prey_Ic))
  
  #### SEM ####
  # Zscore sem
  tar_target(data_z_sem, gives_zdata_SEM(bga, flux_per_prey_Ic)),
  
  # Structural Equation Model
  tar_target(fit_z_sem_5000, make_zSEM_5000(data_z_sem)),
  
  # smaller, quicker sem for an overview
  # tar_target(fit_z_sem, make_zSEM(data_z_sem)),
  
  #### MAKE FIGS ####
  ##### Fig1 M&M by hand ####

  ##### Fig2 PCA Topo ####
  tar_target(Fig2_z, make_zFig2(bga)),

  ##### Fig3 PCA Flux ####
  tar_target(Fig3, makeFig3(bga, flux_per_prey_Ic)),

  ##### Fig 4 SEM & CE ####

  tar_target(Fig4_z_sem_5000, make_zFig4_SEM(fit_z_sem_5000)),
  tar_target(Fig4_z_ce_data_5000, make_zFig4_CE_data(fit_z_sem_5000)),
  tar_target(Fig4_z_ce_5000, make_zFig4_CE(Fig4_z_ce_data_5000)),

  #### MAKE SUPP ####
  ##### S1 Prey Categories by hand ####

  ##### S2 Map of RLS sites ####
  tar_target(map, makeMap(bga)),

  ##### S3 Network theory ####
  tar_target(net_theory, makeSuppTN()),

  ##### S4 DAGS ####
  tar_target(dags, make_DAGS()),

  ##### S5 & S6 ppChecks and scattAvg ####
  tar_target(ppChecks_scattAvg, plot_ppchecks_z(fit_z_sem_5000)),

  ##### S7 PCA 10% top coral/algae/turf ####
  tar_target(pca_cat, makePCA_CAT(bga)),

  ##### S8 Kernell densities of C fluxes ####
  tar_target(kernell, makeKernell(flux_per_prey_Ic)),

  ##### S9 PCA All topo metrics ####
  tar_target(pca_all, makePCA_All(bga)),

  ##### S10 CorrPlot ####
  tar_target(corrplot, makeCorrplot(bga)),

  ##### S11 Full SEM ####
  tar_target(fit_sem_full, make_zSEM_full(data_z_sem)),
  tar_target(full_sem, plot_fullSEM(fit_sem_full))
)



