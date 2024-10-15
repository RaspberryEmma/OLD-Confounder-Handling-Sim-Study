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
# Most Recent Edit: 15/10/2024
# ****************************************


# clear R memory
rm(list=ls())


# fix RNG for reproducibility
set.seed(2024)


# check all external libraries
using<-function(...) {
  libs <- unlist(list(...))
  req  <- unlist(lapply(libs, require, character.only=TRUE))
  need <- libs[req==FALSE]
  if(length(need) > 0){ 
    install.packages(need)
    lapply(need, require, character.only=TRUE)
  }
}
using("dagitty", "dplyr", "ggcorrplot", "ggplot2", "glmnet",
      "igraph", "lars", "matrixStats", "microbenchmark", "sjmisc", "tidyr")


# fix wd issue
if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}


# import simulation-proper code
source("simulation.R")
source("results.R")


# define model types to fit and results metrics to measure
#model_methods <- c("linear")
model_methods   <- c("linear", "stepwise", "stepwise_X", "two_step_LASSO", "two_step_LASSO_X")
results_methods <- c("pred_mse", "r_squared_X", "r_squared_Y",
                     "model_SE", "emp_SE",
                     "causal_effect_est", "causal_effect_mcse", "causal_effect_bias",
                     "avg_abs_param_bias", "coverage",
                     "open_paths", "blocked_paths")


# fixed top-level simulation parameters
n_rep_init             <- 100   # number of repetitons of each scenario
SE_req_init            <- 0.05  # target standard error (determines lower bound for n_rep)
data_split_init        <- NULL  # determines whether wqe split test and training sets
asymmetry_init         <- NaN   # measure of asymmetry within oracle coefficients, set to 1.0 to keep them symmetric
l_zero_X_init          <- TRUE  # force 'L' subgroups affecting X to have an oracle coefficient of 0.0, set to FALSE to use asymmetry
l_zero_Y_init          <- TRUE  # force 'L' subgroups affecting Y to have an oracle coefficient of 0.0, set to FALSE to use asymmetry
oracle_error_mean_init <- 0.00  # error term mean
oracle_error_sd_init   <- 1.00  # error term sd


# top level parameters varied between scenarios
# NB: num_conf must be an integer multiple of 4
# NB: notation for num_conf is c in rest of code, difficult refactor TBC
# NB: important that rX < rY for numerical reasons
# NB: you may create c <= 0 for suff. high values of asymmetry - watch carefully!
# NB: system ill defined for suff. low n_obs
# scenario = c(n_scenario, num_conf, unmeasured_conf, n_obs, causal_strength, r_sq_X, r_sq_Y)
scenarios <- list(c(1,  8, 0,  1000, 1.0, 0.4, 0.6),
                  c(2,  8, 0, 10000, 1.0, 0.4, 0.6),
                  c(3, 16, 0,  1000, 1.0, 0.4, 0.6),
                  c(4, 16, 0, 10000, 1.0, 0.4, 0.6))


for (scenario in scenarios) {
  # extract param values
  n_scenario         <- scenario[1]
  c                  <- scenario[2]
  n_unmeas_conf      <- scenario[3]
  n_obs_init         <- scenario[4]
  causal_str         <- scenario[5]
  target_r_sq_X_init <- scenario[6]
  target_r_sq_Y_init <- scenario[7]
  
  # initialise DAG
  coef_data      <- generate_coef_data(c             = c,
                                       target_r_sq_X = target_r_sq_X_init,
                                       target_r_sq_Y = target_r_sq_Y_init,
                                       asymmetry     = asymmetry_init,
                                       l_zero_X      = l_zero_X_init,
                                       l_zero_Y      = l_zero_Y_init)
  DAG_adj_matrix <- adjacency_matrix_from_coef_data(coef_data = coef_data)
  DAG_labels     <- colnames(DAG_adj_matrix)
  DAG_graph      <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")
  
  # simulation procedure call
  run(n_scenario        = n_scenario,
      graph             = DAG_graph,
      coef_data         = coef_data,
      n_obs             = n_obs_init,
      n_rep             = n_rep_init,
      labels            = DAG_labels,
      model_methods     = model_methods,
      results_methods   = results_methods,
      SE_req            = SE_req_init,
      data_split        = data_split_init,
      target_r_sq_X     = target_r_sq_X_init,
      target_r_sq_Y     = target_r_sq_Y_init,
      asymmetry         = asymmetry_init,
      l_zero_X          = l_zero_X_init,
      l_zero_Y          = l_zero_Y_init,
      oracle_error_mean = oracle_error_mean_init,
      oracle_error_sd   = oracle_error_sd_init,
      record_results    = TRUE)
  
  # generate results plots
  generate_all_plots(case = n_scenario)
}


