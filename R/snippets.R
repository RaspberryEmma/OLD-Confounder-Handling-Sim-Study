# ****************************************
# Confounder Handling Simulation Study
#
# snippets, storafe for code blocks not currently in use
#
# Emma Tarmey
#
# Started:          09/04/2024
# Most Recent Edit: 10/04/2024
# ****************************************

# else if (method == "change_in_est") {
#   model <- chest::chest_lm(crude    = "y ~ X",          # exposure and outcome always included
#                            xlist    = labels[-c(1, 2)], # all Z's as potential
#                            data     = data,
#                            indicate = TRUE)
#   print(model)
#   chest_plot(model) %>% print()
#   stop("change in est modelling")
# }


# # DAG 1
# 
# # intialise DAG
# coef_data      <- read.csv("../data/key-input-coef-data-1.csv")
# DAG_adj_matrix <- read.csv("../data/key-input-adjacency-matrix-1.csv") %>% as.matrix()
# 
# DAG_labels               <- DAG_adj_matrix[, 1]
# rownames(DAG_adj_matrix) <- DAG_labels
# DAG_adj_matrix           <- DAG_adj_matrix[, -1]
# DAG_graph                <- graph_from_adjacency_matrix(DAG_adj_matrix, mode = "directed")
# 
# # models to fit and results metrics to measure
# model_methods   <- c("linear", "stepwise", "LASSO")
# results_methods <- c("r_squared", "param_bias", "causal_effect_precision",
#                      "causal_effect_bias", "coverage", "open_paths",
#                      "blocked_paths", "z_inclusion", "benchmark")
# 
# # simulation procedure call
# run(graph           = DAG_graph,
#     coef_data       = coef_data,
#     n_obs           = n_obs_init,
#     n_rep           = n_rep_init,
#     labels          = DAG_labels,
#     model_methods   = model_methods,
#     results_methods = results_methods,
#     data_split      = data_split_init)
# 
# # generate results plots
# generate_all_plots()
