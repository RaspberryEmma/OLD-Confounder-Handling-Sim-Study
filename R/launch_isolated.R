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
# Most Recent Edit: 15/01/2025
# ****************************************


# clear R memory
rm(list=ls())


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
model_methods   <- c("linear", "linear_no_Z",
                     "stepwise", "stepwise_X",
                     "two_step_LASSO", "two_step_LASSO_X")

results_methods <- c("pred_mse", "r_squared_X", "r_squared_Y",
                     "model_SE", "emp_SE",
                     "causal_true_val", "causal_effect_est", "causal_effect_bias", "causal_effect_mcse",
                     "avg_abs_param_bias", "coverage",
                     "open_paths", "blocked_paths")


# NB: num_total_conf must be an integer multiple of 4
# NB: notation for num_total_conf is c in rest of code, difficult refactor TBC
# NB: important that rX < rY for numerical reasons
# NB: you may create c <= 0 for suff. high values of dissimilarity - watch carefully!
# NB: system ill defined for suff. low n_obs


# top-level parameters held constant across all scenarios and simulation runs
n_rep_init             <- 2000   # number of repetitions of each scenario
SE_req_init            <- 0.05  # target standard error (determines lower bound for n_rep)
data_split_init        <- NULL  # determines whether we split test and training sets
l_zero_X_init          <- FALSE # force 'L' subgroups affecting X to have an oracle coefficient of 0.0, set to FALSE to use dissimilarity
l_zero_Y_init          <- FALSE # force 'L' subgroups affecting Y to have an oracle coefficient of 0.0, set to FALSE to use dissimilarity
binary_X_init          <- TRUE
binary_Y_init          <- TRUE
oracle_error_mean_init <- 0.0
oracle_error_sd_init   <- 1.0


# top-level parameters held constant between scenarios but varied across simulation runs
# simulation = c(n_simulation, n_obs, correlation_U, r_sq_X, r_sq_Y, causal, dissimilarity_rho)
all_simulations <- list(
  c(1, 1000, 0.0, 0.5, 0.7, 0.50, 1.0),
  c(2, 1000, 0.0, 0.6, 0.8, 0.50, 1.0)
)


# top level parameters varied between scenarios
# scenario = c(n_scenario, num_total_conf, measured_conf, unmeasured_conf)
# all_scenarios <- list(
#   c(1,  16,  16,  0),
#   c(2,  16,  12,  4),
#   c(3,  32,  32,  0),
#   c(4,  32,  24,  8),
#   c(5,  64,  64,  0),
#   c(6,  64,  48, 16),
#   c(7, 128, 128,  0),
#   c(8, 128,  96, 32)
# )
all_scenarios <- list(
  c(1, 4, 4, 0),
  c(2, 4, 3, 1)
)


# subset the runs and scenarios to be computed
# we do so only if subsets are provided in cmd
args = commandArgs(trailingOnly=TRUE)

# TESTING ONLY
# manually set args
args = c("1", "1")

if (length(args) > 0) {
  args_simulations <- stringr::str_split(args[1], ",")[[1]]
  args_simulations <- as.numeric(args_simulations)
  simulations      <- all_simulations[args_simulations]
  
  args_scenarios <- stringr::str_split(args[2], ",")[[1]]
  args_scenarios <- as.numeric(args_scenarios)
  scenarios      <- all_scenarios[args_scenarios]
  
} else {
  simulations <- all_simulations
  scenarios   <- all_scenarios
  
  args_simulations <- c(1:length(all_simulations))
  args_scenarios   <- c(1:length(all_scenarios))
}


# fix RNG seed based on current run and scenario
seeds_df <- read.csv(file = "../data/precomputed_RNG_seeds.csv")
seed     <- seeds_df %>%
  filter(simulation_run == min(args_simulations)) %>%
  filter(simulation_scenario == min(args_scenarios))
set.seed(seed$seed)

message("***** Confounder Handling Simulation Study *****")
message("Simulation Runs:")
print(simulations)
message("Simulation Scenarios:")
print(scenarios)
message("Locally starting from precomputed seed:")
print(seed)

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
    num_total_conf  <- scenario[2]
    num_meas_conf   <- scenario[3]
    num_unmeas_conf <- scenario[4]
    
    # initialise DAG
    coef_data      <- generate_coef_data(num_total_conf      = num_total_conf,
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
      num_total_conf  = num_total_conf,
      num_meas_conf   = num_meas_conf,
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
      binary_X        = binary_X_init,
      binary_Y        = binary_Y_init,
      oracle_error_mean = oracle_error_mean_init,
      oracle_error_sd   = oracle_error_sd_init,
      record_results  = TRUE
    )
    
    # generate results plots
    generate_all_plots(case = n_scenario)
    
  } # end scenario loop
} # end simulation loop



