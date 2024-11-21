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
# Most Recent Edit: 14/11/2024
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
# forces wd to be the location of this file
if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}


# import simulation-proper code
source("simulation.R")
source("results.R")


# define model types to fit and results metrics to measure
model_methods   <- c("linear", "stepwise", "stepwise_X", "two_step_LASSO", "two_step_LASSO_X")
results_methods <- c("pred_mse", "r_squared_X", "r_squared_Y",
                     "model_SE", "emp_SE",
                     "causal_effect_est", "causal_effect_mcse", "causal_effect_bias",
                     "avg_abs_param_bias", "coverage",
                     "open_paths", "blocked_paths")


# NB: num_conf must be an integer multiple of 4
# NB: notation for num_conf is c in rest of code, difficult refactor TBC
# NB: important that rX < rY for numerical reasons
# NB: you may create c <= 0 for suff. high values of dissimilarity - watch carefully!
# NB: system ill defined for suff. low n_obs


# fixed top-level simulation parameters (constant across all scenarios and all runs)
n_rep_init             <- 100   # number of repetitions of each scenario
SE_req_init            <- 0.05  # target standard error (determines lower bound for n_rep)
data_split_init        <- NULL  # determines whether we split test and training sets
l_zero_X_init          <- FALSE # force 'L' subgroups affecting X to have an oracle coefficient of 0.0, set to FALSE to use dissimilarity
l_zero_Y_init          <- FALSE # force 'L' subgroups affecting Y to have an oracle coefficient of 0.0, set to FALSE to use dissimilarity
oracle_error_mean_init <- 0.0
oracle_error_sd_init   <- 1.0


# top-level parameters held constant between scenarios but varied across simulation runs
# simulation = c(n_simulation, n_obs, correlation_U, r_sq_X, r_sq_Y, causal, dissimilarity_rho)
simulations <- list(c(1, 1000, 0.0, 0.4, 0.6, 0.5, 2.0))


# top level parameters varied between scenarios
# scenario = c(n_scenario, num_conf, unmeasured_conf)
#scenarios <- list(c(1,  8, 0),
#                  c(2,  8, 0),
#                  c(3, 16, 0),
#                  c(4, 16, 0))
scenarios <- list(c(1,  4, 0))


for (simulation in simulations) {
  for (scenario in scenarios) {
    
    # extract simulation param values
    n_simulation       <- simulation[1]
    n_obs_init         <- simulation[2]
    Z_correlation_init <- simulation[3]
    target_r_sq_X_init <- simulation[4]
    target_r_sq_Y_init <- simulation[5]
    causal_effect      <- simulation[6]
    dissimilarity_init <- simulation[7]
    
    # extract scenario param values
    n_scenario      <- scenario[1]
    num_conf        <- scenario[2]
    num_unmeas_conf <- scenario[3]
    
    # initialise DAG
    coef_data      <- generate_coef_data(num_conf      = num_conf,
                                         target_r_sq_X = target_r_sq_X_init,
                                         target_r_sq_Y = target_r_sq_Y_init,
                                         dissimilarity = dissimilarity_init,
                                         l_zero_X      = l_zero_X_init,
                                         l_zero_Y      = l_zero_Y_init)
    
    # replace initial causal effect with target value
    coef_data[1, 'X'] <- causal_effect
    
    # construct DAG objects
    DAG_adj_matrix <- adjacency_matrix_from_coef_data(coef_data = coef_data)
    DAG_labels     <- colnames(DAG_adj_matrix)
    DAG_graph      <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")
    
    # simulation procedure call
    run(
      # indices
      n_simulation    = n_simulation,
      n_scenario      = n_scenario,
      
      # fixed params
      n_rep  = n_rep_init,
      
      # simulation-run and scenario params
      n_obs           = n_obs_init,
      Z_correlation   = Z_correlation_init,
      target_r_sq_X   = target_r_sq_X_init,
      target_r_sq_Y   = target_r_sq_Y_init,
      causal_effect   = causal_effect_init,
      dissimilarity   = dissimilarity_init,
      num_conf        = num_conf,
      num_unmeas_conf = num_unmeas_conf,
      
      # data and tech
      graph           = DAG_graph,
      coef_data       = coef_data,
      labels          = DAG_labels,
      model_methods   = model_methods,
      results_methods = results_methods,
      SE_req          = SE_req_init,
      data_split      = data_split_init,
      l_zero_X        = l_zero_X_init,
      l_zero_Y        = l_zero_Y_init,
      oracle_error_mean = oracle_error_mean_init,
      oracle_error_sd   = oracle_error_sd_init,
      record_results  = TRUE
    )
    
    # generate results plots
    generate_all_plots(case = n_scenario)
    
  } # end scenario loop
} # end simulation loop


