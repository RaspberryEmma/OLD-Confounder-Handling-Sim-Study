# ****************************************
# Confounder Handling Simulation Study
# 
# The simulation procedure itself, specifically:
# Provides the following:
#    (1) Data Generation
#    (2) Model Fitting
#    (3) Measurement of Results
# 
# Emma Tarmey
#
# Started:          13/02/2024
# Most Recent Edit: 28/02/2024
# ****************************************


# TODO: implement unmeasuredness

normalise <- function(column = NULL) {
  return ( (column - min(column)) / (max(column) - min(column)) )
}

generate_dataset <- function(graph = NULL, n_obs = NULL, labels = NULL) {
  # requires spacejam package, which is no longer on CRAN
  DAG.data <- spacejam::generate.dag.data(g        = graph,
                                          n        = n_obs,
                                          basesd   = 1.0,
                                          basemean = 0.0)
  
  dataset           <- data.frame(DAG.data$X)
  oracle.solution   <- DAG.data$funclist
  colnames(dataset) <- labels
  
  # force 0 <= y <= 1
  dataset[,  1] <- normalise( dataset[,  1] )
  
  return (dataset)
}


run_once <- function(graph = NULL, n_obs = NULL, labels = NULL, model_methods = NULL) {
  print("running!")
  
  # generate data
  data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
  data        %>% dim()    %>% print()
  writeLines("\nData")
  data        %>% head()   %>% print()
  writeLines("\ny")
  data[,  1]  %>% head()   %>% print()
  writeLines("\nX")
  data[, -1]  %>% head()   %>% print()
  writeLines("\n")
  
  # fit models
  print(model_methods)
  
  models <- c()
  for (method in model_methods) {
    model = switch(method,
                   "stepwise"      = step(object    = lm(y ~ ., data = data), # all variable base
                                          direction = "both"),
                   
                   "change_in_est" = chest::chest_glm(crude  = "y ~ X",          # exposure and outcome
                                                      xlist  = labels[-c(1, 2)], # all Z's as potential
                                                      family = quasibinomial,    # data is normalised, but still non-binary
                                                      data   = data),
                   
                   "LASSO"         = glmnet::glmnet(x            = data[, -1], # exposure and all other covariates
                                                    y            = data[,  1], # outcome
                                                    alpha        = 1,          # LASSO penalty
                                                    family.train = "gaussian", # objective function
                                                    intercept    = F)
    )
    models <- c(models, model)
  }
  
  print(models)
  
  writeLines("\n")
  
  print("finished!")
  writeLines("\n")
}

run <- function(graph = NULL, n_obs = NULL, n_rep = NULL, labels = NULL, model_methods = NULL) {
  print("running!")
  
  for (i in 1:n_rep) {
    # progress
    print( paste("Running ", i, "/", n_rep, sep = "") )
    
    # generate data
    data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
  
    # fit models
    models <- c()
    for (method in model_methods) {
      model = switch(method,
                     "stepwise"      = step(object    = lm(y ~ ., data = data), # all variable base
                                            direction = "both"),
                     
                     "change_in_est" = chest::chest_glm(crude  = "y ~ X",          # exposure and outcome
                                                        xlist  = labels[-c(1, 2)], # all Z's as potential
                                                        family = quasibinomial,    # data is normalised, but still non-binary
                                                        data   = data),
                     
                     "LASSO"         = glmnet::glmnet(x            = data[, -1], # exposure and all other covariates
                                                      y            = data[,  1], # outcome
                                                      alpha        = 1,          # LASSO penalty
                                                      family.train = "gaussian", # objective function
                                                      intercept    = F)
      ) %>% ddpcr::quiet() # suppress excess output
      models <- c(models, model)
    }
  }
  
  
  writeLines("\n")
  print("finished!")
  writeLines("\n")
}



