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
# Most Recent Edit: 04/04/2024
# ****************************************


# clear R memory
rm(list=ls())

# all external libraries
library(chest)
library(dplyr)
library(DT)
library(ggdag)
library(ggplot2)
library(glmnet)
library(igraph)
library(microbenchmark)
library(shiny)
library(shinycssloaders)
library(sjmisc)


normalise <- function(column = NULL) {
  return ( (column - min(column)) / (max(column) - min(column)) )
}


dagitty_from_adjacency_matrix <- function(adj_DAG  = NULL,
                                          exposure = NULL,
                                          outcome  = NULL) {
  # number of covariates
  labels <- colnames(adj_DAG)
  n_node <- length(labels)
  
  # convert to numeric
  adj_DAG <- matrix( as.numeric(adj_DAG), ncol = n_node)
  
  # initialise strings
  newline        <- ""
  dagitty_string <- "dag{ "
  
  # for every covariate, check whether we need a new arrow unto any other covariate
  for (i in seq_along(labels)) {
    
    ## covariates with no parents or children
    if ( sum(adj_DAG[, i] + adj_DAG[i, ]) == 0 ){
      newline        <- paste(labels[i], ";", sep = " ")
      dagitty_string <- paste(dagitty_string, newline, sep = "")
    }
    
    # covariates with parents and/or children (i = row, j = col)
    for(j in seq_along(labels)) {
      # edge i->j exists
      if ((adj_DAG[i,j] == 1) & (adj_DAG[j,i] == 0)) {
        newline         <- paste(labels[i], "->", labels[j], "; ", sep=" ")
        dagitty_string  <- paste(dagitty_string, newline, sep = "")
      }
    }
  }
  
  # close string
  newline        <- "}"
  dagitty_string <- dagitty_string  <- paste(dagitty_string, newline, sep = "")
  
  # return dagitty object
  return ( dagitty::dagitty(dagitty_string) )
}


r_squared <- function(model          = NULL,
                      optimal_lambda = NULL,
                      model_method   = NULL,
                      test_data      = NULL) {
  
  # test-training data split
  test_X <- test_data[, -1]
  test_y <- test_data[,  1]
  
  # generate predicted value vector for each model type
  pred_y <- c()
  if (model_method == "LASSO") {
    pred_y <- predict( model, s = optimal_lambda, newx = as.matrix(test_X) ) %>% as.vector()
  }
  else {
    pred_y <- predict( model, test_X ) %>% as.vector()
  }
  
  # find R2 value for each model type
  R2 <- NULL
  if (model_method == "LASSO") {
    # see documentation: https://glmnet.stanford.edu/reference/glmnet.html
    R2 <- model$dev.ratio
  }
  else {
    SSR <- sum((pred_y - test_y)^2)
    SST <- sum((test_y - mean(test_y))^2)
    R2  <- (1 - (SSR / SST))
  }
  
  return (R2)
}


errors <- function(fitted_params = NULL, true_params = NULL) {
  P <- length(true_params)
  param_errors <- rep(0, times = P)
  
  for (i in 1:P) {
    param_errors[i] <- fitted_params[i] - true_params[i]
  }
  
  return (param_errors)
}


param_bias <- function() {
  return (NaN)
}


# TODO: continue work here!
blocked_paths <- function(model = NULL, adj_DAG = NULL) {
  paths <- NaN
  
  # convert graph to dagitty DAG
  dagitty_DAG <- dagitty_from_adjacency_matrix(adj_DAG)
  
  # count open paths of DAG
  open_DAG_paths <- dagitty::paths( dagitty_DAG,
                                    from = "X",
                                    to   = "y")
  #print(open_DAG_paths)
  
  # count open paths in final model
  open_model_paths <- NaN
  
  return (paths)
}


benchmark <- function(model_method = NULL, data = NULL) {
  time <- NaN
  
  data_X <- data[, -1]
  data_y <- data[,  1]
  
  if (model_method == "linear") {
    bench <- microbenchmark::microbenchmark(
      model <- lm(y ~ ., data = data),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
  }
  
  else if (model_method == "stepwise") {
    bench <- microbenchmark::microbenchmark(
      step(object = lm(y ~ ., data = data), direction = "both",
           scope = list(upper = "y ~ .", lower = "y ~ X")),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
  }
  
  else if (model_method == "LASSO") {
    bench <- microbenchmark::microbenchmark(
      glmnet::cv.glmnet(x = as.matrix(data_X), y = data_y, alpha = 1,
                        family.train = "gaussian", intercept = F),
      glmnet::glmnet(x = as.matrix(data_X), y = data_y,alpha = 1,
                     family.train = "gaussian",intercept = F),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
  }
  
  time <- mean(bench$time)
  return (time)
}


all_priors_exist <- function(i = NULL, dataset = NULL, i_coef = NULL) {
  all_exist   <- TRUE
  priors_of_i <- which(!is.na(i_coef))
  
  # check all columns corresponding to priors within the dataset
  # if any such columns contain NAs, then not all priors exist
  for (p in priors_of_i) {
    if (sum(is.na(dataset[, p])) != 0) { all_exist = FALSE }
  }
  
  return (all_exist)
}


generate_dataset <- function(coef_data = NULL, n_obs = NULL, labels = NULL) {
  # initialize
  n_node            <- length(labels)
  dataset           <- data.frame(matrix(NA, nrow = n_obs, ncol = n_node))
  colnames(dataset) <- labels
  
  # separate un-caused and caused
  indices <- c(1:n_node)
  uncaused <- c()
  caused   <- c()
  for (i in indices) {
    if (coef_data$cause[i] == 0) {
      uncaused <- c(uncaused, i)
    }
    else {
      caused <- c(caused, i)
    }
  }
  
  # generate un-caused variables
  for (i in uncaused) {
    dataset[, i] <- rnorm(n = n_obs, mean = 0, sd = 1)
  }
  
  # iteratively generate caused variables
  while( sum(is.na(dataset)) != 0 ) { # while not all cols have been generated
    for (i in caused) {
      
      if ( all_priors_exist(i, dataset, coef_data[i, -c(1, 2)]) ) {
        # generate column i with corresponding beta values
        priors_of_i  <- which(!is.na(coef_data[i, -c(1, 2)]))
        
        # intercept
        dataset[, i] <- rep(coef_data$intercept[i], times = n_obs)
        
        # add beta_p * var_p for all priors p
        for (p in priors_of_i) {
          coef_p       <- coef_data[i, p+2]
          var_p        <- coef_p * dataset[, p]
          dataset[, i] <- rowSums( cbind(dataset[, i], var_p), na.rm = TRUE)
        }
        
        # error term
        error        <- rnorm(n = n_obs, mean = 0, sd = 1) # normal errors
        dataset[, i] <- rowSums( cbind(dataset[, i], error), na.rm = TRUE)
        
        # remove i from "caused" list
        caused <- caused[ !caused == i]
      }
      else {
        # do not generate
      }
      
    }
  }
  
  # force 0 <= y <= 1
  dataset[,  1] <- normalise( dataset[,  1] )
  
  return (dataset)
}


run_once <- function(graph           = NULL,
                     coef_data       = NULL,
                     n_obs           = NULL,
                     labels          = NULL,
                     model_methods   = NULL,
                     results_methods = NULL,
                     data_split      = NULL,
                     using_shiny     = FALSE) {
  
  # run one iteration
  run(graph = graph,
      coef_data       = coef_data,
      n_obs           = n_obs,
      n_rep           = 1,
      labels          = labels,
      model_methods   = model_methods,
      results_methods = results_methods,
      data_split      - data_split,
      using_shiny     = using_shiny,
      messages        = TRUE)
}


run <- function(graph           = NULL,
                coef_data       = NULL,
                n_obs           = NULL,
                n_rep           = NULL,
                labels          = NULL,
                model_methods   = NULL,
                results_methods = NULL,
                data_split      = NULL,
                using_shiny     = FALSE,
                messages        = FALSE) {
  print("running!")
  
  # constants
  M <- length(model_methods)
  R <- length(results_methods)
  
  # initialize results array
  # dim = (#methods * #results * #iterations)
  # we have bias per parameter in final model
  # TODO: implement here!
  results <- array(data = NaN,
                   dim  = c(M, R, n_rep),
                   dimnames = list(model_methods, results_methods, 1:n_rep))
  
  for (i in 1:n_rep) {
    # progress
    message( paste("\n\nRunning Iteration ", i, "/", n_rep, "\n", sep = "") )
    
    # generate data according to split parameter
    # if NULL, use the same data for testing and training
    if(is.null(data_split)) {
      # generate training data
      data <- generate_dataset(coef_data = coef_data, n_obs = n_obs, labels = labels)
      
      # test on same data
      test_data <- data
      
      # record this data
      representative_data <- data
    }
    else {
      train_split <- ceiling(data_split * n_obs)
      test_split  <- (n_obs - train_split)
      
      # generate training data
      data <- generate_dataset(coef_data = coef_data, n_obs = train_split, labels = labels)
      
      # generate seperate testing data
      test_data <- generate_dataset(coef_data = coef_data, n_obs = test_split, labels = labels)
      
      # record this data
      representative_data <- rbind(data, test_data)
    }
    
    # seperate outcome from all other covariates
    data_X <- data[, -1]
    data_y <- data[,  1]
    
    # generate penalty.factor sequence using variable labels
    # ensure exposures (variables marked with 'X') are always included
    labels.no.y <- labels[-1]
    penalty.factor <- rep(1, length(labels.no.y))
    for (label in seq_along(labels.no.y)) {
      if ( sjmisc::str_contains(labels.no.y[label], "X") ) { penalty.factor[label] <- 0 }
    }
    
    # fit all models and record all results
    for (m in 1:M) {
      method <- model_methods[m]
      
      if (method == "linear") {
        model <- lm(y ~ ., data = data)
      }
      else if (method == "stepwise") {
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
        cv_model <- glmnet::cv.glmnet(x              = as.matrix(data_X), # exposure and all other covariates
                                      y              = data_y,          # outcome
                                      alpha          = 1,               # LASSO penalty
                                      family.train   = "gaussian",      # objective function
                                      intercept      = F,
                                      penalty.factor = penalty.factor)  # exposure X always included
        optimal_lambda <- cv_model$lambda.min
        
        # Fit LASSO model with single optimal lambda parameter
        model <- glmnet::glmnet(x              = as.matrix(data_X), # exposure and all other covariates
                                y              = data_y,            # outcome
                                alpha          = 1,                 # LASSO penalty
                                lambda         = optimal_lambda,    # use optimised lambda parameter
                                family.train   = "gaussian",        # objective function
                                intercept      = F,
                                penalty.factor = penalty.factor)    # exposure X always included
      }
      
      # Record results
      for (r in 1:R) {
        result = results_methods[r]
        result_value <- NaN
        
        if (result == "r_squared") {
          result_value <- r_squared(model          = model,
                                    optimal_lambda = optimal_lambda,
                                    model_method   = method,
                                    test_data      = test_data)
        }
        
        # TODO: fix here!
        # dimensions of object must change!
        else if (result == "param_bias") {
          result_value <- param_bias()
        }
        
        else if (result == "blocked_paths") {
          adj_DAG           <- as_adj(graph = graph)
          colnames(adj_DAG) <- labels
          result_value      <- blocked_paths(model = model,
                                             adj_DAG = adj_DAG)
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
  
  # Sim parameters
  params <- data.frame(
    preset <- c("n_rep", "n_obs", "data_split"),
    value  <- c(n_rep, n_obs, data_split)
  )
  
  
  
  # Record current date time
  date_string <- Sys.time()
  
  if (using_shiny) { setwd("..") }
  
  message("\nSaving results to file\n")
  
  # Save sim parameters
  write.csv(params, paste("../data/", date_string, "-sim-parameters.csv", sep = ""))
  
  # Save adjacency matrix
  write.csv((graph %>% as_adjacency_matrix() %>% as.matrix()), paste("../data/", date_string, "-adjacency-matrix.csv", sep = ""))
  
  # Save coef data
  write.csv(coef_data, paste("../data/", date_string, "-coef-data.csv", sep = ""), row.names = FALSE)
  
  # Save one data-set
  write.csv(representative_data, paste("../data/", date_string, "-dataset.csv", sep = ""))
  
  # Save output
  write.csv(results_aggr, paste("../data/", date_string, "-results-table.csv", sep = ""))
  
  if (using_shiny) { setwd("sim_frontend") }
  
  print("finished!")
}



