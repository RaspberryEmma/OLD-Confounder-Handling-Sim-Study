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
# Most Recent Edit: 29/02/2024
# ****************************************


# TODO: FIX LISTS, implement results measurement. implement data generation features

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
  # fit models
  print(model_methods)
  
  # run one iteration
  run(graph = graph, n_obs = n_obs, n_rep = 1, labels = labels, model_methods = model_methods, messages = TRUE)
}


run <- function(graph = NULL, n_obs = NULL, n_rep = NULL, labels = NULL, model_methods = NULL, messages = FALSE) {
  print("running!")
  
  for (i in 1:n_rep) {
    # progress
    print( paste("Running ", i, "/", n_rep, sep = "") )
    
    # generate data
    data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
  
    # generate penalty.factor sequence using variable labels
    # ensure exposures (variables marked with 'X') are always included
    labels.no.y <- labels[-1]
    penalty.factor <- rep(1, length(labels.no.y))
    for (i in 1:length(labels.no.y)) {
      if ( sjmisc::str_contains(labels.no.y[i], "X") ) { penalty.factor[i] <- 0 }
    }
    
    models <- vector(mode="list", length=length(model_methods))
    
    for (i in 1:length(model_methods)) {
      method <- model_methods[i]
      model = switch(method,
                     "stepwise"      = step(object    = lm(y ~ ., data = data),                  # all variable base
                                            direction = "both",                                  # stepwise, not fwd or bwd
                                            scope     = list(upper = "y ~ .", lower = "y ~ X")), # exposure X always included
                     
                     "change_in_est" = chest::chest_glm(crude  = "y ~ X",          # exposure and outcome always included
                                                        xlist  = labels[-c(1, 2)], # all Z's as potential
                                                        family = quasibinomial,    # data is normalised, but still non-binary
                                                        data   = data),
                     
                     "LASSO"         = glmnet::glmnet(x              = data[, -1],     # exposure and all other covariates
                                                      y              = data[,  1],     # outcome
                                                      alpha          = 1,              # LASSO penalty
                                                      family.train   = "gaussian",     # objective function
                                                      intercept      = F,
                                                      penalty.factor = penalty.factor) # exposure X always included
                     
      ) %>% ddpcr::quiet(., all = messages)  # suppress excess output
      models[[i]] <- model
    }
    
    View(models)
    
    if (messages) {
      data        %>% dim()    %>% print()
      writeLines("\nData")
      data        %>% head()   %>% print()
      writeLines("\ny")
      data[,  1]  %>% head()   %>% print()
      writeLines("\nX")
      data[, -1]  %>% head()   %>% print()
      writeLines("\n")
      
      print("Penalty Factors (for LASSO)")
      print(labels.no.y)
      print(penalty.factor)
      writeLines("\n")
      
      for (i in 1:length(model_methods)) { print(models[[i]]) }
      writeLines("\n")
    }
  }
  
  print("finished!")
  writeLines("\n")
}



