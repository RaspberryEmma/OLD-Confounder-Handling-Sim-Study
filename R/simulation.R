# ****************************************
# Confounder Handling Simulation Study
# 
# Launch the simulation procedure itself
# 
# Emma Tarmey
#
# Started:          13/02/2024
# Most Recent Edit: 22/02/2024
# ****************************************

# interpret_DAG <- function() {
  # bespoke implementation
  # putting on ice for now
  
  # # partition columns to generate by existence of prior causes
  # cols.with.priors    <- c()
  # cols.without.priors <- c()
  # 
  # for (i in 1:n_node) {
  #   print(paste("node", i, "rowSum", sum( adj_matrix[, i] )))
  #   if (sum( adj_matrix[, i] ) == 0) {
  #     cols.without.priors    <- c(cols.without.priors, i)
  #   }
  #   else {
  #     cols.with.priors <- c(cols.with.priors, i)
  #   }
  # }
  # 
  # # empty data-set of desired size
  # dataset <- data.frame(matrix(NA, ncol=1, nrow=n_obs))[-1]
  # 
  # # generate columns without priors
  # for (i in cols.without.priors) {
  #   dataset[ labels[i] ] <- rnorm(n = n_obs, mean = 0, sd = 1)
  # }
  # 
  # # generate columns with priors
  # # algorithm here requires at most (n_node)^2 passes
  # # convergence assumes graphs is a properly formed DAG (i.e acyclic)
  # while( any(is.na(dataset)) ) {
  #   
  #   # pass over all nodes
  #   for (i in cols.with.priors) {
  #     # detect all priors of i
  #     priors.of.i <- which(adj_matrix[, i] != 0, arr.ind = T)
  #     
  #     # if all priors have been generated (i.e, no NA values in the sub-matrix)
  #     # we may generate our new column
  #     # TODO: generate sub-matrix!
  #     if ( !any(is.na( ... )) ) {
  #       dataset[ labels[i] ] <- rep("FOUND!", n_obs)
  #     }
  #   }
  #   
  #   # remove columns we generate by checking NA's
  #   cols.with.priors <- cols.with.priors[ !any(is.na(dataset[ labels[cols.with.priors] ] )) ]
  # }
# }

# generate_bespoke_dataset <- function(n_obs = NULL, labels = NULL) {
#   # we assume a labelled DAG of the following structure:
#   # 
#   # 
#   # 
#   
#   # empty data-set of desired size
#   dataset <- data.frame(matrix(NA, ncol=1, nrow=n_obs))[-1]
#   
#   # generate columns without priors
#   for (i in cols.without.priors) {
#     dataset[ labels[i] ] <- rnorm(n = n_obs, mean = 0, sd = 1)
#   }
#   
#   return (dataset)
# }

# TODO: check column ordering(!), implement unmeasuredness
generate_dataset <- function(graph = NULL, n_obs = NULL, labels = NULL) {
  adj_matrix <- as_adj(graph)
  n_node     <- length(graph)
  edges      <- as_edgelist(graph)
  
  dag.data <- spacejam::generate.dag.data(g        = graph,
                              n        = n_obs,
                              basesd   = 1.0,
                              basemean = 0.0)
  
  # TODO: check this!
  dataset           <- data.frame(dag.data$X)
  colnames(dataset) <- labels
  
  return (dataset)
}


run <- function(graph = NULL, n_obs = NULL, labels = NULL) {
  print("running!")
  
  # generate data
  data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
  data        %>% dim()    %>% print()
  data        %>% head()   %>% print()
  writeLines("\n")
  
  # fit models
  # stepwise regression
  # change-in-est
  # TMLE
  
  print("finished!")
}



