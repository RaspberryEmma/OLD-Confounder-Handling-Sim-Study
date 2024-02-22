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


# TODO: migrate from igraph to dagR, complete model generation, implement unmeasuredness


generate_dataset <- function(graph = NULL, n_obs = NULL, labels = NULL) {
  # requires spacejam package, which is no longer on CRAN
  DAG.data <- spacejam::generate.dag.data(g        = graph,
                                          n        = n_obs,
                                          basesd   = 1.0,
                                          basemean = 0.0)
  
  dataset           <- data.frame(DAG.data$X)
  oracle.solution   <- DAG.data$funclist
  colnames(dataset) <- labels
  
  return (dataset)
}


run <- function(graph = NULL, n_obs = NULL, labels = NULL, model_methods = NULL) {
  print("running!")
  
  # generate data
  data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
  data        %>% dim()    %>% print()
  data        %>% head()   %>% print()
  writeLines("\n")
  
  # fit models
  print(model_methods)
  
  # TODO: fix this!
  models <- c()
  for (method in model_methods) {
    model = switch(method,
                   "stepwise"      = step(object    = lm(y ~ ., data = data), # all variable base
                                          direction = "both"),
                   
                   "change_in_est" = chest::chest_glm(crude = "y ~ X",          # exposure and outcome
                                                      xlist = labels[-c(1, 2)], # all Z's as potential
                                                      data  = data),
                   
                   "TMLEs"         = tmle::tmle(Y = "y", # outcome
                                                A = "X", # exposure
                                                W = data)
    )
    models <- c(models, model)
  }
  
  print(models)
  
  writeLines("\n")
  
  print("finished!")
  writeLines("\n")
}



