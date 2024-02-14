# ****************************************
# Confounder Handling Simulation Study
# 
# Launch the simulation procedure itself
# 
# Emma Tarmey
#
# Started:          13/02/2024
# Most Recent Edit: 14/02/2024
# ****************************************


# TODO: implement causality, implement unmeasuredness
generate_dataset <- function(graph = NULL, n_obs = NULL, labels = NULL) {
  adj_matrix <- as_adj(graph)
  n_node     <- length(graph)
  edges      <- as_edgelist(graph)
  
  dataset <- data.frame(matrix(NA, ncol=1, nrow=n_obs))[-1]
  newCol  <- c()
  
  for (i in 1:n_node) {
    newCol <- rnorm(n = n_obs, mean = 0, sd = 1)
    dataset[ labels[i] ] <- newCol
  }
  
  return (dataset)
}

run <- function() {
  print("running!")
  print("finished!")
}

