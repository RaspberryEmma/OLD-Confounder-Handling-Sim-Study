# ****************************************
# Confounder Handling Simulation Study
#
# Isolated form of more flexible wider-sim
# Provides the following:
#    (1) Pre-set conditions, such as the DAG of interest and n_obs
#    (2) Call to simulation procedure
#    (3) Interpretation of results and plotting
#
# Emma Tarmey
#
# Started:          19/03/2024
# Most Recent Edit: 01/07/2024
# ****************************************

# clear R memory
rm(list=ls())

# all external libraries
library(chest)
library(dplyr)
library(DT)
library(ggdag)
library(ggplot2)
library(glmnet)
library(igraph)
library(microbenchmark)
#library(qgraph) # additional layout options, not great but works
library(shiny)
library(shinycssloaders)
library(sjmisc)
library(tidyr)

# fix wd issue
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# simulation-proper code
source("simulation.R")
source("results.R")

# top-level simulation parameters
# reduced values for testing
n_obs_init         <- 200
n_rep_init         <- 10   #100
SE_req_init        <- 0.05
data_split_init    <- 0.50
target_r_sq_init   <- 0.50 # oracle R-squared value we induce


# models to fit and results metrics to measure
model_methods   <- c("linear", "stepwise", "two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")

results_methods <- c("mse", "r_squared",                                     # prediction
                     "causal_effect_bias", "avg_abs_param_bias", "coverage", # beta coefs
                     "open_paths", "blocked_paths", "benchmark")             # other

# limited subset for testing
c_values <- c(1)
#c_values        <- c(0, 1, 2, 5, 10, 20)

for (c in c_values) {
  # initialise DAG
  coef_data      <- generate_coef_data(c = c, target_r_sq = target_r_sq_init, scaling = scaling_init)
  DAG_adj_matrix <- adjacency_matrix_from_coef_data(coef_data = coef_data)
  DAG_labels     <- colnames(DAG_adj_matrix)
  DAG_graph      <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")

  # simulation procedure call
  run(graph           = DAG_graph,
      coef_data       = coef_data,
      n_obs           = n_obs_init,
      n_rep           = n_rep_init,
      labels          = DAG_labels,
      model_methods   = model_methods,
      results_methods = results_methods,
      data_split      = data_split_init,
      target_r_sq     = target_r_sq_init,
      record_results  = TRUE)

  # generate results plots
  generate_all_plots(case = c)
}



