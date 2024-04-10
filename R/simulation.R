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
# Most Recent Edit: 10/04/2024
# ****************************************

# TODO: fix deliberate NaN's!

# all external libraries
library(chest)
library(dplyr)
library(DT)
library(ggdag)
library(ggplot2)
library(glmnet)
library(igraph)
library(lars)
library(microbenchmark)
library(shiny)
library(shinycssloaders)
library(sjmisc)
library(tidyr)


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


mse <- function(model          = NULL,
                optimal_lambda = NULL,
                model_method   = NULL,
                test_data      = NULL) {
  value <- NaN
  
  if (model_method == "LASSO") {
    # separate outcome from other covariates
    test_X    <- test_data[, -1]
    test_y    <- test_data[,  1]
    pred_y    <- predict( model, s = optimal_lambda, newx = as.matrix(test_X) ) %>% as.vector()
    residuals <- test_y - pred_y
    value     <- mean(residuals^2)
  }
  
  else if (model_method == "least_angle") {
    value <- NaN
  }
  
  else {
    value <- mean(model$residuals^2)
  }
  
  return (value)
}


r_squared <- function(model          = NULL,
                      optimal_lambda = NULL,
                      model_method   = NULL,
                      test_data      = NULL) {
  
  # separate outcome from other covariates
  test_X <- test_data[, -1]
  test_y <- test_data[,  1]
  
  # generate predicted value vector for each model type
  pred_y <- c()
  if (model_method == "LASSO") {
    pred_y <- predict( model, s = optimal_lambda, newx = as.matrix(test_X) ) %>% as.vector()
  }
  else if (model_method == "least_angle") {
    # NaN
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
  
  else if (model_method == "least_angle") {
    R2 <- NaN
  }
  
  else {
    SSR <- sum((pred_y - test_y)^2)
    SST <- sum((test_y - mean(test_y))^2)
    R2  <- (1 - (SSR / SST))
  }
  
  return (R2)
}


# calculates individual error terms per coefficient
errors <- function(fitted_params = NULL, true_params = NULL) {
  # interpret no-causal-link as beta = 0
  true_params[is.na(true_params)] <- 0
  P <- length(true_params)
  
  param_errors <- as.vector( rep(NaN, times = P) )
  names(param_errors) <- names(true_params)
  
  for (param in names(true_params)) {
    # if model has selected variable
    if (param %in% names(fitted_params)) {
      param_errors[param] <- fitted_params[param] - true_params[param]
    }
    # interpret non-selection as beta = 0
    else {
      param_errors[param] <- 0.0 - true_params[param]
    }
  }
  
  param_errors <- unlist(param_errors)
  return (param_errors)
}


param_bias <- function(model_method = NULL,
                       model        = NULL,
                       true_values  = NULL) {
  
  coefs <- c()
  
  if (model_method == "LASSO") {
    coefs <- lasso_coefs(model = model, n_var = ncol(true_values))
  }
  else if (model_method == "least_angle") {
    coefs <- lars_coefs(model = model)
  }
  else {
    coefs <- model$coefficients
  }
  
  # fix intercept name differences
  names(coefs)[names(coefs) == "(Intercept)"] <- "intercept"
  
  param_error <- errors(coefs, true_values[1, ])
  
  return (sum(param_error))
}


# TODO: implement!
causal_effect_precision <- function() {
  return (NaN)
}


causal_effect_bias <- function(model_method = NULL, model = NULL, true_value  = NULL) {
  error <- 0.0
  
  if (model_method == "LASSO") {
    model_betas  <- as.data.frame(as.matrix(model$beta))
    error <- (model_betas['X', 's0'] - true_value)
  }
  
  else if (model_method == "least_angle") {
    model_betas <- lars_coefs(model = model)
    error <- (model_betas['X'] - true_value)
  }
  
  else {
    error <- (model$coefficients['X'] - true_value)
  }
  
  return (error)
}


find_vars_in_model <- function(model_method = NULL, model = NULL) {
  vars <- c()
  
  if (model_method == "LASSO") {
    vars <- rownames(model$beta)
    vars <- vars[vars != "X"]
  }
  else {
    vars <- names(model$coefficients)
    vars <- vars[vars != "(Intercept)"]
    vars <- vars[vars != "X"]
  }
  
  return (vars)
}


find_vars_in_path <- function(path = NULL) {
  vars <- strsplit(path, "\\s+")[[1]]
  vars <- vars[vars != "<-"]
  vars <- vars[vars != "->"]
  
  return (vars)
}


open_paths <- function(adj_DAG = NULL) {
  # convert graph to dagitty DAG
  dagitty_DAG <- dagitty_from_adjacency_matrix(adj_DAG)
  
  # count open paths of DAG
  DAG_paths      <- dagitty::paths(dagitty_DAG, from = "X", to = "y")
  open_DAG_paths <- DAG_paths$paths[ DAG_paths$open ]
  
  # remove direct "X -> y" association from open paths
  open_DAG_paths <- open_DAG_paths[open_DAG_paths != "X -> y"]
  
  return(length(open_DAG_paths))
}


blocked_paths <- function(model_method = NULL, model = NULL, adj_DAG = NULL) {
  paths <- 0
  
  # convert graph to dagitty DAG
  dagitty_DAG <- dagitty_from_adjacency_matrix(adj_DAG)
  
  # count open paths of DAG
  DAG_paths      <- dagitty::paths(dagitty_DAG, from = "X", to = "y")
  open_DAG_paths <- DAG_paths$paths[ DAG_paths$open ]
  
  # remove direct "X -> y" association from open paths
  open_DAG_paths <- open_DAG_paths[open_DAG_paths != "X -> y"]
  
  # identify all covariates included in model
  vars_in_model <- find_vars_in_model(model_method = model_method, model = model)
  
  # determine whether open paths are blocked properly in model
  for (path in open_DAG_paths) {
    vars_on_path <- find_vars_in_path(path)
    
    # detect if path is blocked by whether path var is included in model
    blocked <- FALSE
    for (path_var in vars_on_path) {
      if (path_var %in% vars_in_model) { blocked <- TRUE }
    }
    
    if (blocked) {
      paths <- (paths+1)
    }
  }
  
  return (paths)
}


z_inclusion <- function(model_method = NULL, model = NULL, adj_DAG = NULL) {
  included <- 0.0
  
  # identify all covariates included in model
  vars_in_model <- find_vars_in_model(model_method = model_method, model = model)
  
  # convert graph to dagitty DAG
  dagitty_DAG <- dagitty_from_adjacency_matrix(adj_DAG)
  
  # determine correct Z set (vars_to_include)
  DAG_paths       <- dagitty::paths(dagitty_DAG, from = "X", to = "y")
  open_DAG_paths  <- DAG_paths$paths[ DAG_paths$open ]
  
  vars_to_include <- c()
  for (path in open_DAG_paths) {
    vars_on_path <- find_vars_in_path(path)
    vars_to_include <- union(vars_to_include, vars_on_path)
  }
  vars_to_include <- vars_to_include[vars_to_include != "X"]
  vars_to_include <- vars_to_include[vars_to_include != "y"]
  
  # determine whether the correct set is fully included within the model
  if (all(vars_to_include %in% vars_in_model)) {
    included <- 1.0
  }
  
  # TESTING
  #print(vars_to_include)
  #print(vars_in_model)
  #print(included)
  
  return (included)
}


# TODO: implement for LASSO
coverage <- function(model_method = NULL, model = NULL, true_value = NULL) {
  within_CI <- 0.0
  
  if (model_method == "LASSO") {
    # extract coefficients
    model_betas  <- as.data.frame(as.matrix(model$beta))
    beta_x       <- model_betas['X', 's0']
    
    # determine critical t-value
    # standard 95% CI is assumed
    t_val <- qt(p = 0.975, df = (model$nobs - 2))
    
    # TODO: FIX HERE!
    # determine standard error of beta_x
    # SE =(approx) root(mse) where mse = full regression mse
    # SE = std / sqrt(n_obs)
    se_x <- NaN / sqrt(model$nobs)
    
    # determine lower and upper bounds of CI
    lower <- beta_x - (t_val * se_x)
    upper <- beta_x + (t_val * se_x)
    
    # TESTING
    # print(beta_x)
    # print(true_value)
    # print(t_val)
    # print(se_x)
    within_CI <- NaN # remove when implemented
    #stop("coverage LASSO")
    
    #if ((true_value > lower) && (true_value < upper)) {
    #  within_CI <- 1.0
    #}
    
  }
  
  else if (model_method == "least_angle") {
    within_CI <- NaN
  }
  
  else {
    CI    <- confint(model, 'X', level = 0.95)
    
    if ((true_value > CI[1]) && (true_value < CI[2])) {
      within_CI <- 1.0
    }
    
    writeLines("\n")
    print(CI)
    print(true_value)
    print(within_CI)
    writeLines("\n")
  }
  
  return (within_CI)
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
                        family.train = "gaussian", intercept = T),
      glmnet::glmnet(x = as.matrix(data_X), y = data_y,alpha = 1,
                     family.train = "gaussian",intercept = T),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
  }
  
  else if (model_method == "least_angle") {
    bench <- microbenchmark::microbenchmark(
      lars(x         = as.matrix(data_X), # exposure and all other covariates
           y         = data_y,            # outcome
           type      = "lar",
           intercept = TRUE),
      times = 1 # repetitions at higher level!
    ) %>% invisible()
  }
  
  time <- mean(bench$time)
  return (time)
}


stepwise_fill_in_blanks <- function(coefs = NULL, labels = NULL) {
  names(coefs)[names(coefs) == "(Intercept)"] <- "intercept"
  
  for (label in labels) {
    # if a given variable doesn't exist, create as NaN
    if (!(label %in% names(coefs))) {
      coefs[label] <- NaN
    }
  }
  
  # assert variable ordering
  coefs <- coefs[order(factor(names(coefs), levels = labels))]
  names(coefs)[names(coefs) == "intercept"] <- "(Intercept)"
  
  return (coefs)
}


lasso_coefs <- function(model = NULL, n_var = NULL) {
  # extract coefficients
  model_betas  <- as.data.frame(as.matrix(model$beta))
  coefs        <- model_betas$s0
  
  # infer intercept term
  zeroes    <- matrix(rep(0, times = (n_var - 1)), nrow = 1)
  intercept <- predict( model, s = model$lambda, newx = zeroes )
  intercept <- intercept[1, "s1"]
  names(intercept) <- NULL
  
  # combine results into named vector
  coefs <- c(intercept, coefs)
  names(coefs) <- c("(Intercept)", rownames(model$beta))
  
  return (coefs)
}


lars_coefs <- function(model = NULL) {
  model_stats      <- summary(model)
  coefs            <- coef(model, s = which.min(model_stats$Cp), mode="step")
  intercept        <- model$mu
  names(intercept) <- "(Intercept)"
  
  # combine results into named vector
  coefs <- c(intercept, coefs)
  
  return (coefs)
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
        # error sd is quite important
        error        <- rnorm(n = n_obs, mean = 0, sd = 0.1)
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
                     record_results  = NULL,
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
      record_results  = record_results,
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
                record_results  = NULL,
                using_shiny     = FALSE,
                messages        = FALSE) {
  
  print("running!")
  
  # constants
  beta_names <- colnames(coef_data)[-c(1, 3)]
  B <- length(beta_names) # number of betas
  M <- length(model_methods)
  R <- length(results_methods)
  
  # initialize results array
  # dim = (#methods * #results * #iterations)
  results <- array(data     = NaN,
                   dim      = c(M, R, n_rep),
                   dimnames = list(model_methods, results_methods, 1:n_rep))
  
  # initialize coefficients array
  # dim = (#methods * #betas * #iterations)
  model_coefs <- array(data     = NaN,
                       dim      = c(M, B, n_rep),
                       dimnames = list(model_methods, beta_names, 1:n_rep) )
  
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
        
        # Record coefficients
        model_coefs[m, , i] <- model$coefficients
      }
      
      else if (method == "stepwise") {
        model <- step(object    = lm(y ~ ., data = data),                # all variable base
                      direction = "both",                                # stepwise, not fwd or bwd
                      scope     = list(upper = "y ~ .", lower = "y ~ X") # exposure X always included
                      ) %>% invisible()
        
        # Record coefficients
        model_coefs[m, , i] <- stepwise_fill_in_blanks(model$coefficients, beta_names)
      }

      else if (method == "LASSO") {
        # Find optimal lambda parameter via cross-validation
        cv_model <- glmnet::cv.glmnet(x              = as.matrix(data_X), # exposure and all other covariates
                                      y              = data_y,          # outcome
                                      alpha          = 1,               # LASSO penalty
                                      family.train   = "gaussian",      # objective function
                                      intercept      = T,
                                      penalty.factor = penalty.factor)  # exposure X always included
        optimal_lambda <- cv_model$lambda.min
        
        # Fit LASSO model with single optimal lambda parameter
        model <- glmnet::glmnet(x              = as.matrix(data_X), # exposure and all other covariates
                                y              = data_y,            # outcome
                                alpha          = 1,                 # LASSO penalty
                                lambda         = optimal_lambda,    # use optimised lambda parameter
                                family.train   = "gaussian",        # objective function
                                intercept      = T,
                                penalty.factor = penalty.factor)    # exposure X always included
        
        # Record coefficients
        model_coefs[m, , i] <-  lasso_coefs(model = model, n_var = length(beta_names))
      }
      
      else if (method == "least_angle") {
        model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                      y         = data_y,            # outcome
                      type      = "lar",
                      intercept = TRUE)
        
        # Record coefficients
        model_coefs[m, , i] <- lars_coefs(model = model)
      }
      
      # Record results
      for (r in 1:R) {
        result = results_methods[r]
        result_value <- NaN
        
        if (result == "mse") {
          result_value <- mse(model          = model,
                              optimal_lambda = optimal_lambda,
                              model_method   = method,
                              test_data      = test_data)
        }
        
        else if (result == "r_squared") {
          result_value <- r_squared(model          = model,
                                    optimal_lambda = optimal_lambda,
                                    model_method   = method,
                                    test_data      = test_data)
        }
        
        else if (result == "param_bias") {
          result_value <- param_bias(model_method = method,
                                     model        = model,
                                     true_values  = coef_data[, -c(1, 3)]) # all beta terms
        }
        
        else if (result == "causal_effect_precision") {
          result_value <- causal_effect_precision()
        }
        
        else if (result == "causal_effect_bias") {
          result_value <- causal_effect_bias(model_method = method,
                               model        = model,
                               true_value   = coef_data[1, "X"])
        }
        
        else if (result == "open_paths") {
          adj_DAG           <- as_adj(graph = graph)
          colnames(adj_DAG) <- labels
          result_value      <- open_paths(adj_DAG = adj_DAG)
        }
        
        else if (result == "blocked_paths") {
          adj_DAG           <- as_adj(graph = graph)
          colnames(adj_DAG) <- labels
          result_value      <- blocked_paths(model_method = method,
                                             model        = model,
                                             adj_DAG      = adj_DAG)
        }
        
        else if (result == "z_inclusion") {
          adj_DAG           <- as_adj(graph = graph)
          colnames(adj_DAG) <- labels
          result_value      <- z_inclusion(model_method = method,
                                           model        = model,
                                           adj_DAG      = adj_DAG)
        }
        
        else if (result == "coverage") {
          result_value      <- coverage(model_method = method,
                                        model        = model,
                                        true_value   = coef_data[1, "X"])
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
  results_aggr <- apply(results, c(1,2), mean)
  writeLines("\n")
  print("Results Table")
  print(results_aggr)
  writeLines("\n")
  
  # Generate Coefficients Table
  coefs_aggr  <- apply(model_coefs, c(1,2), mean)
  true_values <- coef_data[1, -c(1, 3)]
  rownames(true_values) <- c("true_values")
  coefs_aggr <- rbind(true_values, coefs_aggr)
  writeLines("\n")
  print("Coefficients Table")
  print(coefs_aggr)
  writeLines("\n")
  
  # Sim parameters
  params <- data.frame(
    preset <- c("n_rep", "n_obs", "data_split"),
    value  <- c(n_rep, n_obs, data_split)
  )
  
  if (record_results) {
    # Record current date time
    # replace : with - for windows compatibility
    date_string <- gsub(":", "-", Sys.time())
    
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
    
    # Save coefficients
    write.csv(coefs_aggr, paste("../data/", date_string, "-model-coefs.csv", sep = ""))
    
    # Save results measures
    write.csv(results_aggr, paste("../data/", date_string, "-results-table.csv", sep = ""))
    
    if (using_shiny) { setwd("sim_frontend") }
  }
  
  print("finished!")
}



