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
# Most Recent Edit: 18/03/2024
# ****************************************


# TODO: implement data generation features

normalise <- function(column = NULL) {
  return ( (column - min(column)) / (max(column) - min(column)) )
}

r_squared <- function(model          = NULL,
                      optimal_lambda = NULL,
                      model_method   = NULL,
                      test_data      = NULL) {
  
  test_X <- test_data[, -1]
  test_y <- test_data[,  1]
  
  mean_y <- mean(test_y)
  pred_y <- c()
  
  if (model_method == "LASSO") {
    pred_y <- predict( model, s = optimal_lambda, newx = as.matrix(test_X) )
  }
  else {
    pred_y <- predict( model, test_X )
  }
  
  
  SSR <- sum((test_y - pred_y))
  SST <- sum((test_y - mean_y))
  
  R2  <- (1 - (SSR / SST))
  return (R2)
}

param_bias <- function(param_values, true_value) {
  return (mean(param_values) - true_value)
}

bias <- function(model = NULL, true_values = NULL) {
  total_bias <- 0
  
  # TODO - implement
  
  return (total_bias)
}

benchmark <- function(model_method = NULL, data = NULL) {
  time <- NaN
  
  data_X <- data[, -1]
  data_y <- data[,  1]
  
  if (model_method == "stepwise") {
    
    bench <- microbenchmark::microbenchmark(
      step(object = lm(y ~ ., data = data), direction = "both", scope = list(upper = "y ~ .", lower = "y ~ X")),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
    
  }
  else if (model_method == "LASSO") {
    
    bench <- microbenchmark::microbenchmark(
      glmnet::cv.glmnet(x = as.matrix(data_X), y = data_y, alpha = 1,family.train = "gaussian", intercept = F),
      glmnet::glmnet(x = as.matrix(data_X), y = data_y,alpha = 1,family.train = "gaussian",intercept = F),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
    
  }
  
  writeLines("\nBenchmark:")
  print(bench)
  writeLines("\n")
  
  time <- mean(bench$time)
  return (time)
}

generate_dataset <- function(graph = NULL, n_obs = NULL, labels = NULL) {
  # generate data from DAG
  # requires spacejam package, which is no longer on CRAN
  DAG.data <- spacejam::generate.dag.data(g        = graph,
                                          n        = n_obs,
                                          basesd   = 1.0,
                                          basemean = 0.0)
  
  # coerce to dataframe
  dataset <- data.frame(DAG.data$X)
  
  # cast to double
  dataset <- mutate_all(dataset, function(x) as.numeric(as.character(x)))
  
  # apply labels
  colnames(dataset) <- labels
  
  # force 0 <= y <= 1
  dataset[,  1] <- normalise( dataset[,  1] )

  # extact true coef values
  oracle.solution   <- DAG.data$funclist
  
  return (dataset)
}


run_once <- function(graph = NULL, n_obs = NULL, labels = NULL, model_methods = NULL, results_methods = NULL) {
  # fit models
  print(model_methods)
  
  # run one iteration
  run(graph = graph, n_obs = n_obs, n_rep = 1, labels = labels, model_methods = model_methods, results_methods = results_methods, messages = TRUE)
}


run <- function(graph = NULL, n_obs = NULL, n_rep = NULL, labels = NULL, model_methods = NULL, results_methods = NULL, messages = FALSE) {
  print("running!")
  
  # constants
  M <- length(model_methods)
  R <- length(results_methods)
  
  # initialize results array
  # dim = (#methods * #results * #iterations)
  results <- array(data = NaN,
                   dim  = c(M, R, n_rep),
                   dimnames = list(model_methods, results_methods, 1:n_rep))
  print(results)
  print(dim(results))
  
  for (i in 1:n_rep) {
    # progress
    print( paste("Running ", i, "/", n_rep, sep = "") )
    
    # generate data
    data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
    
    data_X <- data[, -1]
    data_y <- data[,  1]
    
    writeLines("\nData")
    data   %>% head()   %>% print()
    data   %>% typeof() %>% print()
    writeLines("\nTest y")
    data_y %>% head()   %>% print()
    data_y %>% typeof() %>% print()
    writeLines("\nTest X")
    data_X %>% head()   %>% print()
    data_X %>% typeof() %>% print()
    
    as.matrix(data_X) %>% head() %>% print()
    
    # generate penalty.factor sequence using variable labels
    # ensure exposures (variables marked with 'X') are always included
    labels.no.y <- labels[-1]
    penalty.factor <- rep(1, length(labels.no.y))
    for (label in 1:length(labels.no.y)) {
      if ( sjmisc::str_contains(labels.no.y[label], "X") ) { penalty.factor[label] <- 0 }
    }
    
    # fit all models and record all results
    for (m in 1:M) {
      method <- model_methods[m]
      
      if (method == "stepwise") {
        model <- step(object    = lm(y ~ ., data = data),                 # all variable base
                      direction = "both",                                 # stepwise, not fwd or bwd
                      scope     = list(upper = "y ~ .", lower = "y ~ X")) # exposure X always included
      }
      else if (method == "change_in_est") {
        model <- chest::chest_glm(crude  = "y ~ X",          # exposure and outcome always included
                                  xlist  = labels[-c(1, 2)], # all Z's as potential
                                  family = quasibinomial,    # data is normalised, but still non-binary
                                  data   = data) 
      }
      else if (method == "LASSO") {
        # Find optimal lambda parameter via cross-validation
        print("FINDING LAMBDA")
        cv_model <- glmnet::cv.glmnet(x              = as.matrix(data_X), # exposure and all other covariates
                                      y              = data_y,          # outcome
                                      alpha          = 1,               # LASSO penalty
                                      family.train   = "gaussian",      # objective function
                                      intercept      = F,
                                      penalty.factor = penalty.factor)  # exposure X always included
        
        optimal_lambda <- cv_model$lambda.min
        print(optimal_lambda)
        
        # Fit LASSO model with single optimal lambda parameter
        print("FITTING LASSO")
        model <- glmnet::glmnet(x              = as.matrix(data_X), # exposure and all other covariates
                                y              = data_y,            # outcome
                                alpha          = 1,                 # LASSO penalty
                                lambda         = optimal_lambda,    # use optimised lambda parameter
                                family.train   = "gaussian",        # objective function
                                intercept      = F,
                                penalty.factor = penalty.factor)    # exposure X always included
      }
      
      # Testing
      writeLines("\n\n")
      print(paste("Summary of ", method, " model", sep = ''))
      print(summary(model))
      print(coef(model))
      
      # Generate test set
      test_data <- generate_dataset(graph = graph, n_obs = n_obs, labels = labels)
      
      # Record results
      for (r in 1:R) {
        # TODO: implement results measurement!
        result = results_methods[r]
        result_value <- NaN
        
        if (result == "r-squared") {
          result_value <- r_squared(model          = model,
                                    optimal_lambda = optimal_lambda,
                                    model_method   = method,
                                    test_data      = test_data)
        }
        else if (result == "bias") {
          result_value <- bias(model       = model,
                               true_values = NULL)
        }
        else if (result == "benchmark") {
          result_value <- benchmark(model_method = method,
                                    data         = test_data)
        }
        
        results[m, r, i] <- result_value
      }
    }
    
    
    if (messages) {
      print("Data-set Size")
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
    }
  }
  
  # Generate Results Table
  writeLines("\n")
  print("Results Table")
  results_aggr <- apply(results, c(1,2), mean)
  print(results_aggr)
  writeLines("\n")
  
  # Generate pre-sets table
  presets <- data.frame(
    preset <- c("n_rep", "n_obs"),
    value  <- c(n_rep, n_obs)
  )
  
  # Record current date time
  date_string <- Sys.time()
  
  # Save input
  write.csv((graph %>% as_adjacency_matrix() %>% as.matrix()), paste("../../data/", date_string, "-input-DAG.csv", sep = ""))
  
  # Save pre-sets
  write.csv(presets, paste("../../data/", date_string, "-presets.csv", sep = ""))
  
  # Save output
  write.csv(results_aggr, paste("../../data/", date_string, "-results-table.csv", sep = ""))
  
  print("finished!")
}



