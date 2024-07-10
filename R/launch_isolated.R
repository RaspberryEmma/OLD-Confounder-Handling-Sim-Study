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
# Most Recent Edit: 04/07/2024
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

# fix RNG for reproducibility
set.seed(2024)

# top-level simulation parameters
# reduced values for testing
n_obs_init        <- 10000
n_rep_init        <- 10 # 100
SE_req_init       <- 0.05
data_split_init   <- 0.50

# important that rX < rY for numerical reasons
target_r_sq_X_init     <- 0.40 # oracle R-squared value, proportion of variance in X explained by all Z's, we induce
target_r_sq_Y_init     <- 0.60 # oracle R-squared value, proportion of variance in Y explained by X and all Z's, we induce

oracle_error_mean_init <- 0.00 # error term mean
oracle_error_sd_init   <- 1.00 # error term sd


# models to fit and results metrics to measure
model_methods   <- c("linear", "stepwise", "two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")

results_methods <- c("mse", "r_squared_X", "r_squared_Y",                    # prediction
                     "causal_effect_bias", "avg_abs_param_bias", "coverage", # beta coefs
                     "open_paths", "blocked_paths")             # other

c_values        <- c(1, 2, 4, 8)

for (c in c_values) {
  # initialise DAG
  coef_data      <- generate_coef_data(c             = c,
                                       target_r_sq_X = target_r_sq_X_init,
                                       target_r_sq_Y = target_r_sq_Y_init)
  DAG_adj_matrix <- adjacency_matrix_from_coef_data(coef_data = coef_data)
  DAG_labels     <- colnames(DAG_adj_matrix)
  DAG_graph      <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")

  # simulation procedure call
  run(graph             = DAG_graph,
      coef_data         = coef_data,
      n_obs             = n_obs_init,
      n_rep             = n_rep_init,
      labels            = DAG_labels,
      model_methods     = model_methods,
      results_methods   = results_methods,
      data_split        = data_split_init,
      target_r_sq_X     = target_r_sq_X_init,
      target_r_sq_Y     = target_r_sq_Y_init,
      oracle_error_mean = oracle_error_mean_init,
      oracle_error_sd   = oracle_error_sd_init,
      record_results    = TRUE)

  # generate results plots
  generate_all_plots(case = c)
}



