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
# Most Recent Edit: 11/09/2024
# ****************************************


# all external libraries
library(dagitty)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(glmnet)
library(igraph)
library(lars)
library(matrixStats)
library(microbenchmark)
library(sjmisc)
library(tidyr)


normalise <- function(column = NULL) {
  return ( (column - min(column)) / (max(column) - min(column)) )
}


null_string_catch = function(value = NULL){
  out <- ""
  if(is.null(value)) {
    out <- "NULL"
  }
  else {
    out <- as.character(value)
  }
  return (out)
}


adjacency_matrix_from_coef_data <- function(coef_data = NULL) {
  adj_matrix <- coef_data[, -c(1, 2)]
  labels     <- colnames(adj_matrix)
  
  adj_matrix[is.na(adj_matrix)]  <- 0
  adj_matrix[adj_matrix != 0]    <- 1 # all non-NA entries correspond to arrows
  adj_matrix                     <- t(adj_matrix)
  colnames(adj_matrix)           <- labels
  
  return (adj_matrix)
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
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  if (model_method %in% lasso_variants) {
    # separate outcome from other covariates
    test_X    <- test_data[, -1]
    test_y    <- test_data[,  1]
    
    model_stats <- summary(model)
    pred_y      <- predict( model, s = which.min(model_stats$Cp), newx = as.matrix(test_X) )$fit
    residuals   <- test_y - pred_y
    value       <- mean(residuals^2)
  }
  else {
    value <- mean(model$residuals^2)
  }
  
  return (value)
}


r_squared_X <- function(model          = NULL, # NEW model for predicting X in terms of Z
                        optimal_lambda = NULL,
                        model_method   = NULL,
                        test_data      = NULL) {
  R2 <- NaN

  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")

  # exclude Y altogether, seperate exposure X from Z's
  test_Zs <- test_data[, -c(1, 2), drop = F]
  test_X  <- test_data[, 2]

  # generate predicted value vector for each model type
  pred_X <- c()
  if (model_method %in% lasso_variants) {
    model_stats <- summary(model)
    pred_X      <- predict( model, s = which.min(model_stats$Cp), newx = as.matrix(test_Zs) )$fit
  }
  else {
    pred_X <- predict( model, test_Zs ) %>% as.vector()
  }
  
  SSR <- sum((pred_X - test_X)^2)
  SST <- sum((test_X - mean(test_X))^2)
  R2  <- (1 - (SSR / SST))
  
  return (R2)
}


r_squared_Y <- function(model          = NULL,
                        optimal_lambda = NULL,
                        model_method   = NULL,
                        test_data      = NULL) {
  R2 <- NaN
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  # separate outcome from other covariates
  test_X <- test_data[, -1, drop = F]
  test_y <- test_data[,  1]
  
  # generate predicted value vector for each model type
  pred_y <- c()
  if (model_method %in% lasso_variants) {
    model_stats <- summary(model)
    pred_y      <- predict( model, s = which.min(model_stats$Cp), newx = as.matrix(test_X) )$fit
  }
  else {
    pred_y <- predict( model, test_X ) %>% as.vector()
  }
  
  SSR <- sum((pred_y - test_y)^2)
  SST <- sum((test_y - mean(test_y))^2)
  R2  <- (1 - (SSR / SST))
  
  return (R2)
}


# TODO: implement!
model_SE <- function(model = NULL, model_method = NULL) {
  return (NaN)
}


# TODO - implement!
emp_SE <- function(model = NULL, model_method = NULL) {
  return (NaN)
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
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  if (model_method %in% lasso_variants) {
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


avg_abs_param_bias <- function(model_method = NULL,
                                        model        = NULL,
                                        true_values  = NULL) {
  
  coefs <- c()
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  if (model_method %in% lasso_variants) {
    coefs <- lars_coefs(model = model)
  }
  else {
    coefs <- model$coefficients
  }
  
  # fix intercept name differences
  names(coefs)[names(coefs) == "(Intercept)"] <- "intercept"
  
  param_error         <- errors(coefs, true_values[1, ])
  avg_abs_param_error <- mean(abs(param_error))
  
  return (avg_abs_param_error)
}


causal_effect_bias <- function(model_method = NULL, model = NULL, true_value  = NULL) {
  error <- 0.0
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  if (model_method %in% lasso_variants) {
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
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  if (model_method %in% lasso_variants) {
    vars <- names(lars_coefs(model = model))
    vars <- vars[vars != "(Intercept)"]
    vars <- vars[vars != "X"]
  }
  
  else if (model_method %in% two_step_variants) {
    vars <- names(model$coefficients)
    vars <- vars[vars != "(Intercept)"]
    vars <- vars[vars != "X"]
  }
  
  else if (model_method == "CBPS") {
    # epsilon    <- 0.01 # TODO: Autotune this value!
    # cbps_coefs <- model$coefficients[,1]
    # cbps_coefs <- cbps_coefs[abs(cbps_coefs) > epsilon]
    # 
    # vars <- names(cbps_coefs)
    # vars <- vars[vars != "(Intercept)"]
    # vars <- vars[vars != "X"]
  }
  
  # linear, prop_score_based
  else {
    vars <- names(model$coefficients)
    vars <- vars[vars != "(Intercept)"]
    vars <- vars[vars != "X"]
  }
  
  return (vars)
}

lasso_selection <- function(model_method = NULL, model = NULL) {
  all_vars <- find_vars_in_model(model_method = "LASSO", model = model)
  
  if (model_method == "two_step_LASSO_X") {
    all_coefs <- lars_coefs(model = model)[-c(1)] # remove intercept
  }
  else if (model_method == "two_step_LASSO") {
    all_coefs <- lars_coefs(model = model)[-c(1, 2)] # remove intercept and X
  }
  
  # keep all variables whose coefs have not shrunk to zero
  vars_selected <- all_vars[ (all_coefs != 0.0) ]
  
  # print(all_vars)
  # print(all_coefs)
  # print(vars_selected)
  
  return (vars_selected)
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
  
  return (included)
}


coverage <- function(model_method = NULL, model = NULL, true_value = NULL) {
  within_CI <- 0.0
  
  lasso_variants    <- c("LASSO", "least_angle", "inf_fwd_stage")
  two_step_variants <- c("two_step_LASSO", "two_step_least_angle", "two_step_inf_fwd_stage")
  
  if (model_method %in% lasso_variants) {
    within_CI <- NaN
  }
  
  else {
    CI <- confint(model, 'X', level = 0.95)
    if ((true_value > CI[1]) && (true_value < CI[2])) {
      within_CI <- 1.0
    }
  }
  
  return (within_CI)
}


benchmark <- function(model_method = NULL, data = NULL, times = NULL) {
  time <- NaN
  
  data_X <- data[, -1, drop = F]
  data_y <- data[,  1]
  
  if (model_method == "linear") {
    bench <- microbenchmark::microbenchmark(
      model <- lm(y ~ ., data = data),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "stepwise") {
    bench <- microbenchmark::microbenchmark(
      model <- step(object = lm(y ~ ., data = data), direction = "both",
                    scope = list(upper = "y ~ .", lower = "y ~ X")),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "LASSO") {
    bench <- microbenchmark::microbenchmark(
      model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "lasso",
                    intercept = TRUE),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "least_angle") {
    bench <- microbenchmark::microbenchmark(
      model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "lar",
                    intercept = TRUE),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "inf_fwd_stage") {
    bench <- microbenchmark::microbenchmark(
      model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "forward.stagewise",
                    intercept = TRUE),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "two_step_LASSO") {
    bench <- microbenchmark::microbenchmark(
      model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "lasso",
                    intercept = TRUE),
      model <- lm(y ~ ., data = data),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "two_step_least_angle") {
    bench <- microbenchmark::microbenchmark(
      model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "lar",
                    intercept = TRUE),
      model <- lm(y ~ ., data = data),
      times = times
    ) %>% invisible()
  }

  else if (model_method == "two_step_inf_fwd_stage") {
    bench <- microbenchmark::microbenchmark(
      model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                    y         = data_y,            # outcome
                    type      = "forward.stagewise",
                    intercept = TRUE),
      model <- lm(y ~ ., data = data),
      times = times
    ) %>% invisible()
  }
  
  else if (model_method == "iv_2sls") {
    bench <- microbenchmark::microbenchmark(
      model <- ivreg(y ~ ., data = data),
      times = times
    )
  }
  
  else if (model_method == "prop_score_based") {
    bench <- microbenchmark::microbenchmark(
      cbps_model <- CBPS(y ~ ., data = data, ATT = TRUE),
      model      <- lm(y ~ ., data = data),
      times = times
    )
  }
  
  else {
    message("Unrecognised model type!")
    bench <- data.frame(time <- c(NaN))
  }

  time <- mean(bench$time)
  return (time)
}


fill_in_blanks <- function(coefs = NULL, labels = NULL) {
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


# glmnet LASSO implementation
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


# lars object implementation of LASSO and subvariants
lars_coefs <- function(model = NULL) {
  model_stats      <- summary(model)
  coefs            <- coef(model, s = which.min(model_stats$Cp), mode="step")
  
  if (!is.null(model$mu)) {
    intercept        <- model$mu
    names(intercept) <- "(Intercept)"
    # combine results into named vector
    coefs <- c(intercept, coefs)
  }
  
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


generate_dataset <- function(coef_data         = NULL,
                             n_obs             = NULL,
                             oracle_error_mean = NULL,
                             oracle_error_sd   = NULL,
                             labels            = NULL) {
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
        error        <- rnorm(n = n_obs, mean = oracle_error_mean, sd = oracle_error_sd)
        dataset[, i] <- rowSums( cbind(dataset[, i], error), na.rm = TRUE)
        
        # remove i from "caused" list
        caused <- caused[ !caused == i]
      }
      else {
        # do not generate
      }
      
    }
  }
  
  return (dataset)
}


# The value for all beta coefficients used for generating X
beta_X_formula <- function(num_conf = NULL, target_r_sq_X = NULL) {
  # short-hand
  m   <- num_conf
  r_X <- target_r_sq_X
  
  value <- sqrt( (1/m) * (r_X/(1 - r_X)) )
  
  return (value)
}


beta_X_levels_condition <- function(num_conf      = NULL,
                                    target_r_sq_X = NULL,
                                    beta_Xs       = NULL) {
  # short-hand
  m   <- num_conf
  r_X <- target_r_sq_X
  
  LHS     <- sum(beta_Xs^2)
  RHS     <- (4/m) * (r_X/(1 - r_X))
  epsilon <- 0.001
  
  return ((LHS - RHS) < epsilon)
}


# The values for all beta coefficients used for generating Y
# Tuned to induce the given value of R2_X in generated data-sets
beta_X_levels_formula <- function(num_conf = NULL, target_r_sq_X = NULL, asymmetry = NULL, l_zero_X = NULL, l_zero_Y = NULL) {
  
  # short-hand
  m   <- num_conf
  r_X <- target_r_sq_X
  b   <- beta_X_formula(num_conf = m, target_r_sq_X = r_X) # symmetric value
  
  if (l_zero_X) {
    b_1 <- 0
    b_2 <- 0
  }
  else {
    b_1 <- (1/asymmetry) * b
    b_2 <- (1/asymmetry) * b
  }
  
  numerator   <- ((b_1^2 + b_2^2) * (r_X - 1)) + ((4/m) * r_X)
  denominator <- 2 * (1 - r_X)
  
  b_3 <- sqrt( numerator / denominator )
  b_4 <- sqrt( numerator / denominator )
  
  return (c(b_1, b_2, b_3, b_4))
}


# The values for all beta coefficients used for generating Y
beta_Y_levels_formula <- function(num_conf = NULL, target_r_sq_X = NULL, asymmetry = NULL, l_zero_X = NULL, l_zero_Y = NULL) {
  
  # short-hand
  m   <- num_conf
  r_X <- target_r_sq_X
  b   <- beta_X_formula(num_conf = m, target_r_sq_X = r_X) # symmetric value
  
  if (l_zero_Y) {
    d_1 <- 0
    d_3 <- 0
  }
  else {
    d_1 <- (1/asymmetry) * b
    d_3 <- (1/asymmetry) * b
  }
  
  numerator   <- ((d_1^2 + d_3^2) * (r_X - 1)) + ((4/m) * r_X)
  denominator <- 2 * (1 - r_X)
  
  d_2 <- sqrt( numerator / denominator )
  d_4 <- sqrt( numerator / denominator )
  
  return (c(d_1, d_2, d_3, d_4))
}


# Mean of X we induce
mean_X_formula <- function(intercept_X = NULL) {
  return (intercept_X)
}


# Variance for X we induce
var_X_formula <- function(num_conf = NULL, beta_Xs = NULL) {
  return (((num_conf/4) * sum(beta_Xs^2)) + 1)
}


# Mean for Y we induce
mean_Y_formula <- function(intercept_Y = NULL, causal = NULL, mean_X = NULL) {
  return (intercept_Y + (causal * mean_X))
}


# Variance for Y we induce
var_Y_formula <- function(num_conf = NULL,
                          beta_Xs  = NULL,
                          beta_Ys  = NULL,
                          causal   = NULL) {
  return ( ((num_conf/4) * sum(( causal*beta_Xs + beta_Ys )^2)) + causal^2 + 1 )
}


analytic_r_sq_X <- function(num_conf = NULL,
                            beta_Xs  = NULL) {
  
  numerator   <- ((num_conf/4) * sum(beta_Xs^2))
  denominator <- ((num_conf/4) * sum(beta_Xs^2)) + 1
  return (numerator / denominator)
}


analytic_r_sq_Y <- function(num_conf = NULL,
                            beta_Xs  = NULL,
                            beta_Ys  = NULL,
                            causal   = NULL) {
  
  numerator   <- ((num_conf/4) * sum( ((causal*beta_Xs) + beta_Ys)^2 )) + causal^2
  denominator <- ((num_conf/4) * sum( ((causal*beta_Xs) + beta_Ys)^2 )) + causal^2 + 1
  
  return (numerator / denominator)
}


analytic_levels_causal_effect <- function(num_conf      = NULL,
                                          beta_Xs       = NULL,
                                          beta_Ys       = NULL,
                                          target_r_sq_Y = NULL) {
  # short-hand
  m   <- num_conf
  r_Y <- target_r_sq_Y
  
  quad_a <- ((m/4) * (r_Y - 1) * sum( beta_Xs^2 )) + r_Y - 1
  quad_b <- (m/4) * 2 * (r_Y - 1) * sum( beta_Xs * beta_Ys )
  quad_c <- ((m/4) * (r_Y - 1) * sum( beta_Ys^2 )) + r_Y
  
  disc  <- (quad_b^2) + (-4 * quad_a * quad_c)
  sol_1 <- ((-1 * quad_b) + sqrt(disc)) / (2 * quad_a)
  sol_2 <- ((-1 * quad_b) - sqrt(disc)) / (2 * quad_a)
  
  causal <- sol_1
  if (sol_1 < sol_2) { causal <- sol_2 }
  
  return (causal)
}


# Building the table of coefficients we use for data-set generation
generate_coef_data <- function(c             = NULL,
                               target_r_sq_X = NULL,
                               target_r_sq_Y = NULL,
                               asymmetry     = NULL,
                               l_zero_X      = NULL,
                               l_zero_Y      = NULL) {
  
  var_labels <- c("y", "X", "Z1")
  
  if (c > 0) {
    Z_list     <- c(2:(c+1))
    for (i in Z_list) {
      # add label corresponding to variable i
      new_label <- paste("Z", i, sep = "")
      var_labels <- c(var_labels, new_label)
    }
  }
  
  n_var <- length(var_labels)
  
  cause <- rep(0,  times = n_var)
  cause[ match("y", var_labels) ] <- 1
  if (c > 0) {
    cause[ match("X", var_labels) ] <- 1
  }
  
  intercept <- rep(NA, times = n_var)
  intercept[ match("y", var_labels) ] <- 1
  if (c > 0) {
    intercept[ match("X", var_labels) ] <- 1
  }
  
  # generate initial DAG table
  coef_data <- data.frame(cause, intercept)
  for (var in var_labels) {
    coef_data[var] <- rep(NA, times = n_var)
  }
  
  # populate table with non-NA entries corresponding to arrows in DAG
  # from X to y
  coef_data[ match("y", var_labels),  "X" ] <- 1
  
  # index change guarantees dummy variable is always last
  for (i in seq.int(from = 1, to = (c), length.out = c)) {
    # from confounder i to y
    coef_data[ match("y", var_labels), paste("Z", i, sep = "") ] <- 1
    
    # from confounder i to X
    coef_data[ match("X", var_labels), paste("Z", i, sep = "") ] <- 1
  }
  
  # Adjust all beta values in order to control R2
  beta_Xs               <- beta_X_levels_formula(num_conf = c, target_r_sq_X = target_r_sq_X, asymmetry = asymmetry, l_zero_X = l_zero_X, l_zero_Y = l_zero_Y)
  beta_Ys               <- beta_Y_levels_formula(num_conf = c, target_r_sq_X = target_r_sq_X, asymmetry = asymmetry, l_zero_X = l_zero_X, l_zero_Y = l_zero_Y)
  oracle_causal_effect  <- analytic_levels_causal_effect(num_conf = c, beta_Xs = beta_Xs, beta_Ys = beta_Ys, target_r_sq_Y = target_r_sq_Y)
  
  # subset by caused / uncaused
  all_vars        <- var_labels
  vars_with_prior <- var_labels[ coef_data$cause == 1 ]
  
  # for every variable generated as a linear combination, re-scale incoming betas
  for (var in vars_with_prior) {
    var_index    <- which(all_vars == var)
    
    # LL group
    for (i in seq.int(from = 1, to = (c/4), length.out = c/4)) {
      coef_data[ match("y", var_labels), paste("Z", i, sep = "") ] <- beta_Ys[1]
      coef_data[ match("X", var_labels), paste("Z", i, sep = "") ] <- beta_Xs[1]
    }
    
    # LH
    for (i in seq.int(from = ((c/4)+1), to = (c/2), length.out = c/4)) {
      coef_data[ match("y", var_labels), paste("Z", i, sep = "") ] <- beta_Ys[2]
      coef_data[ match("X", var_labels), paste("Z", i, sep = "") ] <- beta_Xs[2]
    }
    
    # HL
    for (i in seq.int(from = ((c/2)+1), to = ((3/4)*c), length.out = c/4)) {
      coef_data[ match("y", var_labels), paste("Z", i, sep = "") ] <- beta_Ys[3]
      coef_data[ match("X", var_labels), paste("Z", i, sep = "") ] <- beta_Xs[3]
    }
    
    # HH
    for (i in seq.int(from = ((3/4)*c+1), to = (c), length.out = c/4)) {
      coef_data[ match("y", var_labels), paste("Z", i, sep = "") ] <- beta_Ys[4]
      coef_data[ match("X", var_labels), paste("Z", i, sep = "") ] <- beta_Xs[4]
    }
    
    # select and insert appropriate beta_X,Y
    coef_data[ match("y", var_labels), "X" ] <- oracle_causal_effect
  }
  
  return (coef_data)
}


determine_cov_selection <- function(case   = NULL,
                                    method = NULL,
                                    model  = NULL) {
  var_labels <- c("X", "Z1")
  if (case > 0) {
    Z_list     <- c(2:(case+1))
    for (i in Z_list) {
      # add label corresponding to variable i
      new_label <- paste("Z", i, sep = "")
      var_labels <- c(var_labels, new_label)
    }
  }
  
  selection        <- rep(0, times = length(var_labels))
  names(selection) <- var_labels
  model_selection  <- names(model$coefficients[-c(1)])
  
  for (var in model_selection) {
    selection[var] <- 1
  }
  
  return (selection)
}


# NB: X and dummy are groups of covariates of size 1
mean_across_groups <- function(table = NULL, case = NULL) {
  method_col <- rownames(table)
  
  if ("intercept" %in% colnames(table)) {
    intercept_col <- table[, 1]
    X_col         <- table[, 2]
    Z_subtable    <- table[, 3:(case+3)]
  }
  else {
    X_col         <- table[, 1]
    Z_subtable    <- table[, 2:(case+2)]
  }
  
  #print(table)
  #print(X_col)
  #print(Z_subtable)
  
  if (case == 4) {
    Z_LL <- Z_subtable[ , 1]
    Z_LH <- Z_subtable[ , 2]
    Z_HL <- Z_subtable[ , 3]
    Z_HH <- Z_subtable[ , 4]
    Z_dummy <- Z_subtable[ , 5]
  }
  else if (nrow(table) > 1) {
    Z_LL <- rowMeans(Z_subtable[ ,                 1:(0.25 * case)])
    Z_LH <- rowMeans(Z_subtable[ , ((0.25 * case)+1):(0.50 * case)])
    Z_HL <- rowMeans(Z_subtable[ , ((0.50 * case)+1):(0.75 * case)])
    Z_HH <- rowMeans(Z_subtable[ , ((0.75 * case)+1):case])
    Z_dummy <- Z_subtable[ , case+1]
  }
  else {
    Z_LL <- mean(Z_subtable[                1:(0.25 * case)])
    Z_LH <- mean(Z_subtable[((0.25 * case)+1):(0.50 * case)])
    Z_HL <- mean(Z_subtable[((0.50 * case)+1):(0.75 * case)])
    Z_HH <- mean(Z_subtable[((0.75 * case)+1):case])
    Z_dummy <- Z_subtable[ case+1]
  }
  
  if ("intercept" %in% colnames(table)) {
    new_table <- cbind(intercept_col, X_col, Z_LL, Z_LH, Z_HL, Z_HH, Z_dummy)
    colnames(new_table) <- c("intercept", "X", "Z_LL", "Z_LH", "Z_HL", "Z_HH", "Z_dummy")
    rownames(new_table) <- method_col
  }
  else {
    new_table <- cbind(X_col, Z_LL, Z_LH, Z_HL, Z_HH, Z_dummy)
    colnames(new_table) <- c("X", "Z_LL", "Z_LH", "Z_HL", "Z_HH", "Z_dummy")
    rownames(new_table) <- method_col
  }
  
  return (new_table)
}


# NB: X and dummy are groups of covariates of size 1
# -> variance = 0.0 WITHIN group
var_across_groups <- function(table = NULL, case = NULL) {
  method_col <- rownames(table)
  
  if ("intercept" %in% colnames(table)) {
    intercept_col <- table[, 1]
    X_col         <- table[, 2]
    Z_subtable    <- table[, 3:(case+3)]
  }
  else {
    X_col         <- rep(0.0, times = length(table[, 1]))
    Z_subtable    <- table[, 2:(case+2)]
  }
  
  #print(table)
  #print(X_col)
  #print(Z_subtable)
  
  if (case == 4) {
    Z_LL    <- 0.0
    Z_LH    <- 0.0
    Z_HL    <- 0.0
    Z_HH    <- 0.0
    Z_dummy <- 0.0
  }
  else if (nrow(table) > 1) {
    Z_LL    <- rowVars(Z_subtable[ ,                 1:(0.25 * case)])
    Z_LH    <- rowVars(Z_subtable[ , ((0.25 * case)+1):(0.50 * case)])
    Z_HL    <- rowVars(Z_subtable[ , ((0.50 * case)+1):(0.75 * case)])
    Z_HH    <- rowVars(Z_subtable[ , ((0.75 * case)+1):case])
    Z_dummy <- 0.0
  }
  else {
    Z_LL    <- var(Z_subtable[                1:(0.25 * case)])
    Z_LH    <- var(Z_subtable[((0.25 * case)+1):(0.50 * case)])
    Z_HL    <- var(Z_subtable[((0.50 * case)+1):(0.75 * case)])
    Z_HH    <- var(Z_subtable[((0.75 * case)+1):case])
    Z_dummy <- 0.0
  }
  
  if ("intercept" %in% colnames(table)) {
    new_table <- cbind(intercept_col, X_col, Z_LL, Z_LH, Z_HL, Z_HH, Z_dummy)
    colnames(new_table) <- c("intercept", "X", "Z_LL", "Z_LH", "Z_HL", "Z_HH", "Z_dummy")
    rownames(new_table) <- method_col
  }
  else {
    new_table <- cbind(X_col, Z_LL, Z_LH, Z_HL, Z_HH, Z_dummy)
    colnames(new_table) <- c("X", "Z_LL", "Z_LH", "Z_HL", "Z_HH", "Z_dummy")
    rownames(new_table) <- method_col
  }
  
  return (new_table)
}


run_once <- function(graph             = NULL,
                     coef_data         = NULL,
                     n_obs             = NULL,
                     labels            = NULL,
                     model_methods     = NULL,
                     results_methods   = NULL,
                     SE_req            = NULL,
                     data_split        = NULL,
                     target_r_sq_X     = NULL,
                     target_r_sq_Y     = NULL,
                     asymmetry         = NULL,
                     l_zero_X          = NULL,
                     l_zero_Y          = NULL,
                     oracle_error_mean = NULL,
                     oracle_error_sd   = NULL,
                     record_results    = NULL,
                     using_shiny       = FALSE) {
  
  # run one iteration
  run(graph = graph,
      coef_data         = coef_data,
      n_obs             = n_obs,
      n_rep             = 1,
      labels            = labels,
      model_methods     = model_methods,
      results_methods   = results_methods,
      SE_req            = SE_req,
      data_split        = data_split,
      target_r_sq_X     = target_r_sq_X,
      target_r_sq_Y     = target_r_sq_Y,
      asymmetry         = asymmetry,
      l_zero_X          = l_zero_X,
      l_zero_Y          = l_zero_Y,
      oracle_error_mean = oracle_error_mean,
      oracle_error_sd   = oracle_error_sd,
      using_shiny       = using_shiny,
      record_results    = record_results,
      messages          = TRUE)
}


run <- function(graph             = NULL,
                coef_data         = NULL,
                n_obs             = NULL,
                n_rep             = NULL,
                labels            = NULL,
                model_methods     = NULL,
                results_methods   = NULL,
                SE_req            = NULL,
                data_split        = NULL,
                target_r_sq_X     = NULL,
                target_r_sq_Y     = NULL,
                asymmetry         = NULL,
                l_zero_X          = NULL,
                l_zero_Y          = NULL,
                oracle_error_mean = NULL,
                oracle_error_sd   = NULL,
                record_results    = NULL,
                using_shiny       = FALSE,
                messages          = FALSE) {
  
  print("running!")
  
  # constants
  beta_names <- colnames(coef_data)[-c(1, 3)]
  B <- length(beta_names)
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
  
  # initialize covariate selection array
  # dim = (#methods * #non-Y-covariates * #iterations)
  cov_selection <- array(data     = NaN,
                         dim      = c(M, B-1, n_rep),
                         dimnames = list(model_methods, beta_names[-c(1)], 1:n_rep) )
  
  for (i in 1:n_rep) {
    # progress
    message( paste("\n\nRunning Scenario ", c, ", Iteration ", i, "/", n_rep, "\n", sep = "") )
    
    # generate data according to split parameter
    # if NULL, use the same data for testing and training
    if(is.null(data_split)) {
      # generate training data
      data <- generate_dataset(coef_data         = coef_data,
                               n_obs             = n_obs,
                               oracle_error_mean = oracle_error_mean,
                               oracle_error_sd   = oracle_error_sd,
                               labels            = labels)
      
      # test on same data
      test_data <- data
      
      # record this data
      representative_data <- data
    }
    else {
      train_split <- ceiling(data_split * n_obs)
      test_split  <- (n_obs - train_split)
      
      # generate training data
      data <- generate_dataset(coef_data         = coef_data,
                               n_obs             = train_split,
                               oracle_error_mean = oracle_error_mean,
                               oracle_error_sd   = oracle_error_sd,
                               labels            = labels)
      
      # generate seperate testing data
      test_data <- generate_dataset(coef_data         = coef_data,
                                    n_obs             = test_split,
                                    oracle_error_mean = oracle_error_mean,
                                    oracle_error_sd   = oracle_error_sd,
                                    labels            = labels)
      
      # record this data
      representative_data <- rbind(data, test_data)
    }
    
    # seperate outcome from all other covariates
    data_X <- data[, -1, drop = F]
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
      method  <- model_methods[m]
      
      if (method == "linear") {
        model <- lm(y ~ ., data = data)
        
        # Record coefficients
        model_coefs[m, , i] <- model$coefficients
        
        # Record covariate selection
        cov_selection[m, , i] <- determine_cov_selection(case   = c,
                                                         method = method,
                                                         model  = model)
      }
      
      else if (method == "stepwise") {
        model <- step(object    = lm(y ~ ., data = data),                 # all variable base
                      direction = "both",                                 # stepwise, not fwd or bwd
                      scope     = list(upper = "y ~ .", lower = "y ~ X"), # exposure X always included
                      trace     = 0                                       # suppress output
                      )
        
        # Record coefficients
        model_coefs[m, , i] <- fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        cov_selection[m, , i] <- determine_cov_selection(case   = c,
                                                         method = method,
                                                         model  = model)
      }
      
      else if (method == "stepwise_X") {
        # fit stepwise model for X
        X_data  <- data[, -c(1)] # drop Y
        X_model <- step(object    = lm(X ~ ., data = X_data),               # all variable base
                        direction = "both",                                 # stepwise, not fwd or bwd
                        scope     = list(upper = "X ~ .", lower = "X ~ 0"), # effectively empty model as lower
                        trace     = 0                                       # suppress output
        )
        
        # determine vars to include
        vars_selected <- find_vars_in_model(model_method = method, model = X_model)
        
        # fit new model for Y on the subset of covariates selected
        formula_string <- "y ~ X"
        for (var in vars_selected) {
          formula_string <- paste(formula_string, " + ", var, sep = "")
        }
        formula <- as.formula( formula_string )
        model   <- lm(formula = formula, data = data)
        
        # Record coefficients
        model_coefs[m, , i] <- fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        cov_selection[m, , i] <- determine_cov_selection(case   = c,
                                                         method = method,
                                                         model  = model)
      }

      else if (method == "LASSO") {
        model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                      y         = data_y,            # outcome
                      type      = "lasso",
                      intercept = TRUE)
        
        # Record coefficients
        model_coefs[m, , i] <- lars_coefs(model = model)
        
        # Record covariate selection
        cov_selection[m, , i] <- determine_cov_selection(case   = c,
                                                         method = method,
                                                         model  = model)
      }
      
      else if (method == "two_step_LASSO") {
        # fit LASSO model
        model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                      y         = data_y,            # outcome
                      type      = "lasso",
                      intercept = TRUE)
        
        # find vars in model
        vars_selected <- lasso_selection(model_method = method, model = model)
        
        # fit new model on the subset of covariates selected
        formula_string <- "y ~ X"
        for (var in vars_selected) {
          formula_string <- paste(formula_string, " + ", var, sep = "")
        }
        formula <- as.formula( formula_string )
        model   <- lm(formula = formula, data = data)
        
        # Record coefficients
        model_coefs[m, , i] <-  fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        cov_selection[m, , i] <- determine_cov_selection(case   = c,
                                                         method = method,
                                                         model  = model)
      }
      
      else if (method == "two_step_LASSO_X") {
        # fit LASSO model on X
        X_data  <- data[, c(2)]     # outcome X
        Z_data  <- data[, -c(1, 2)] # drop Y, X
        X_model <- lars(x         = as.matrix(Z_data), # exposure and all other covariates
                        y         = X_data,            # outcome
                        type      = "lasso",
                        intercept = TRUE)
        
        # find vars in model
        vars_selected <- lasso_selection(model_method = method, model = X_model)
        
        # fit new model for Y on the subset of covariates selected
        formula_string <- "y ~ X"
        for (var in vars_selected) {
          formula_string <- paste(formula_string, " + ", var, sep = "")
        }
        formula <- as.formula( formula_string )
        model   <- lm(formula = formula, data = data)
        
        # Record coefficients
        model_coefs[m, , i] <-  fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        cov_selection[m, , i] <- determine_cov_selection(case   = c,
                                                         method = method,
                                                         model  = model)
      }
      
      else if (method == "least_angle") {
        # model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
        #               y         = data_y,            # outcome
        #               type      = "lar",
        #               intercept = TRUE)
        # 
        # # Record coefficients
        # model_coefs[m, , i] <- lars_coefs(model = model)
        
        # Record covariate selection
        # TODO: implement
        stop("run - covariate selection")
      }
      
      else if (method == "two_step_least_angle") {
        model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                      y         = data_y,            # outcome
                      type      = "lar",
                      intercept = TRUE)
        
        # # find vars in model
        # vars_selected <- lasso_selection(model_method = method, model = model)
        # 
        # # fit new model on the subset of covariates selected
        # formula_string <- "y ~ X"
        # for (var in vars_selected) {
        #   formula_string <- paste(formula_string, " + ", var, sep = "")
        # }
        # formula <- as.formula( formula_string )
        # model   <- lm(formula = formula, data = data)
        # 
        # # Record coefficients
        # model_coefs[m, , i] <-  fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        # TODO: implement
        stop("run - covariate selection")
      }
      
      else if (method == "inf_fwd_stage") {
        model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                      y         = data_y,            # outcome
                      type      = "forward.stagewise",
                      intercept = TRUE)
        
        # # Record coefficients
        # model_coefs[m, , i] <- lars_coefs(model = model)
        
        # Record covariate selection
        # TODO: implement
        stop("run - covariate selection")
      }
      
      else if (method == "two_step_inf_fwd_stage") {
        model <- lars(x         = as.matrix(data_X), # exposure and all other covariates
                      y         = data_y,            # outcome
                      type      = "forward.stagewise",
                      intercept = TRUE)
        
        # # find vars in model
        # vars_selected <- lasso_selection(model_method = method, model = model)
        # 
        # # fit new model on the subset of covariates selected
        # formula_string <- "y ~ X"
        # for (var in vars_selected) {
        #   formula_string <- paste(formula_string, " + ", var, sep = "")
        # }
        # formula <- as.formula( formula_string )
        # model   <- lm(formula = formula, data = data)
        # 
        # # Record coefficients
        # model_coefs[m, , i] <- fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        # TODO: implement
        stop("run - covariate selection")
      }
      
      else if (method == "iv_2sls") {
        # # instrumental variable based 2-stage least-squares regression
        # model <- ivreg(y ~ ., data = data)
        # 
        # # Record coefficients
        # model_coefs[m, , i] <- fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        # TODO: implement
        stop("run - covariate selection")
      }
      
      else if (method == "prop_score_based") {
        # # CBPS Covariate Balancing Propensity Score method
        # cbps_model <- CBPS(y ~ ., data = data, ATT = TRUE)
        # 
        # # determine vars to include
        # # model_method overwritten as this is stage 1 of 2 stage procedure
        # vars_selected <- find_vars_in_model(model_method = "CBPS", model = cbps_model)
        # 
        # # fit new model on the subset of covariates selected
        # formula_string <- "y ~ X"
        # for (var in vars_selected) {
        #   formula_string <- paste(formula_string, " + ", var, sep = "")
        # }
        # formula <- as.formula( formula_string )
        # model   <- lm(formula = formula, data = data)
        # 
        # # Record coefficients
        # model_coefs[m, , i] <- fill_in_blanks(model$coefficients, beta_names)
        
        # Record covariate selection
        # TODO: implement
        stop("run - covariate selection")
      }
      
      # Record results
      for (r in 1:R) {
        result = results_methods[r]
        result_value <- NaN
        
        if (result == "pred_mse") {
          result_value <- mse(model          = model,
                              optimal_lambda = optimal_lambda,
                              model_method   = method,
                              test_data      = test_data)
        }
        
        else if (result == "r_squared_X") {
          result_value <- r_squared_X(model          = lm(X ~ ., data = data[, -1, drop = F]),
                                      optimal_lambda = optimal_lambda,
                                      model_method   = method,
                                      test_data      = test_data)
        }
        
        else if (result == "r_squared_Y") {
          result_value <- r_squared_Y(model          = model,
                                      optimal_lambda = optimal_lambda,
                                      model_method   = method,
                                      test_data      = test_data)
        }
        
        else if (result == "model_SE") {
          result_value <- model_SE(model        = model,
                                   model_method = method)
        }
        
        else if (result == "emp_SE") {
          result_value <- emp_SE(model        = model,
                                 model_method = method)
        }
        
        else if (result == "param_bias") {
          result_value <- param_bias(model_method = method,
                                     model        = model,
                                     true_values  = coef_data[, -c(1, 3)]) # all beta terms
        }
        
        else if (result == "avg_abs_param_bias") {
          result_value <- avg_abs_param_bias(model_method = method,
                                                      model        = model,
                                                      true_values  = coef_data[, -c(1, 3)]) # all beta terms
        }
        
        else if (result == "causal_effect_precision") {
          result_value <- NaN # not implemented
        }
        
        else if (result == "causal_effect_est") {
          result_value <- NaN # calculated afterwards
        }
        
        else if (result == "causal_effect_mcse") {
          result_value <- NaN # calculated afterwards
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
          result_value <- NaN # calculated afterwards
        }
        
        # append result
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
  
  # Benchmark methods after run-time
  if ("benchmark" %in% results_methods) {
  for (m in 1:M) {
      method <- model_methods[m]
      results_aggr[m, "benchmark"] <- benchmark(model_method = method, data = data, times = 10)
    }
  }
  
  # Monte carlo standard error of causal effect
  if ("causal_effect_est" %in% results_methods) {
    for (m in 1:M) {
      method                               <- model_methods[m]
      causal_effect_estimates              <- model_coefs[method, 'X', ]
      results_aggr[m, "causal_effect_est"] <- mean(causal_effect_estimates)
    }
  }
  
  # Monte carlo standard error of causal effect
  if ("causal_effect_mcse" %in% results_methods) {
    for (m in 1:M) {
      # we have n_rep estimates of the causal effect
      # hence n = n_rep in the context of sampling this random variable
      method                                <- model_methods[m]
      causal_effect_estimates               <- model_coefs[method, 'X', ]
      results_aggr[m, "causal_effect_mcse"] <- sqrt( var(causal_effect_estimates)/n_rep )
    }
  }
  
  # Generate coefficients table
  model_coefs           <- replace(model_coefs, is.na(model_coefs), 0.0)
  coefs_last            <- model_coefs[, , n_rep]
  coefs_aggr            <- apply(model_coefs, c(1,2), mean)
  true_values           <- coef_data[1, -c(1, 3)]
  rownames(true_values) <- c("true_values")
  coefs_last            <- rbind(true_values, coefs_last)
  coefs_aggr            <- rbind(true_values, coefs_aggr)
  
  # Generate grouped coefficients table
  coefs_group_aggr <- mean_across_groups(table = coefs_aggr, case = c)
  
  # Generate covariate selection table
  cov_selection_aggr <- apply(cov_selection, c(1, 2), mean)
  
  # Generate grouped covariate selection table
  cov_selection_group_aggr <- mean_across_groups(table = cov_selection_aggr, case = c)
  
  # Generate grouped covariate selection table
  cov_dispersion_group <- var_across_groups(table = cov_selection_aggr, case = c)
  
  # Generate oracle variances table
  var_labels    <- colnames(coef_data)[-c(1, 2)]
  metric_names  <- c("mean_Y", "var_Y", "mean_X", "var_X")
  
  oracle_beta_Xs <- beta_X_levels_formula(num_conf = c, target_r_sq_X = target_r_sq_X, asymmetry = asymmetry, l_zero_X = l_zero_X, l_zero_Y = l_zero_Y)
  oracle_beta_Ys <- beta_Y_levels_formula(num_conf = c, target_r_sq_X = target_r_sq_X, asymmetry = asymmetry, l_zero_X = l_zero_X, l_zero_Y = l_zero_Y)
  
  oracle_causal  <- coef_data[ match("y", var_labels), "X" ]
  mean_X         <- mean_X_formula(intercept_X = coef_data[ match("X", var_labels), "intercept" ])
  
  oracle_var <- c(mean_Y_formula(intercept_Y = coef_data[ match("y", var_labels), "intercept" ],
                                 causal      = oracle_causal,
                                 mean_X      = mean_X),
                  var_Y_formula(num_conf = c,
                                beta_Xs  = oracle_beta_Xs,
                                beta_Ys  = oracle_beta_Ys,
                                causal   = oracle_causal),
                  mean_X,
                  var_X_formula(num_conf      = c,
                                beta_Xs       = oracle_beta_Xs)
  )
  names(oracle_var) <- metric_names
  
  # Generate coef subtable
  coef_subtable        <- c(oracle_beta_Xs[1], oracle_beta_Xs[2], oracle_beta_Xs[3], oracle_beta_Xs[4],
                            oracle_beta_Ys[1], oracle_beta_Ys[2], oracle_beta_Ys[3], oracle_beta_Ys[4],
                            oracle_causal)
  
  names(coef_subtable) <- c('beta_X_LL', 'beta_X_LH', 'beta_X_HL', 'beta_X_HH',
                            'beta_Y_LL', 'beta_Y_LH', 'beta_Y_HL', 'beta_Y_HH',
                            'causal')
  
  # Generate Sample Variances Table
  sample_mean <- sapply(data, mean, na.rm = T)
  sample_sd   <- sapply(data, sd, na.rm = T)
  sample_var  <- sapply(data, sd, na.rm = T)^2
  sample_vars <- rbind(sample_mean, sample_sd, sample_var)
  
  # Generate R2 Table
  r2_values        <- c(target_r_sq_X,
                        analytic_r_sq_X(num_conf = c, beta_Xs = oracle_beta_Xs),
                        results_aggr[1, 'r_squared_X'],
                        target_r_sq_Y,
                        analytic_r_sq_Y(num_conf = c,
                                        beta_Xs  = oracle_beta_Xs,
                                        beta_Ys  = oracle_beta_Ys,
                                        causal   = oracle_causal),
                        results_aggr[1, 'r_squared_Y'])
  names(r2_values) <- c("Target_R2_X", "Analytic_R2_X", "Sample R2_X", "Target_R2_Y", "Analytic_R2_Y", "Sample R2_Y")
  
  message("\nResults of Simulation")
  
  writeLines("\n")
  print("R2 Control Table")
  print(r2_values)
  
  
  writeLines("\n\n")
  print("Oracle Distribution")
  print(oracle_var)
  
  writeLines("\n")
  print("Sample Distributions")
  print(sample_vars)
  
  writeLines("\n\n")
  print("Key Oracle Coefficients")
  print(coef_subtable)
  
  writeLines("\n\n")
  print("Oracle Coefficients")
  print(coef_data)
  
  writeLines("\n")
  print("Sample Coefficients for Y Table")
  print(coefs_aggr)
  
  writeLines("\n")
  print("Grouped Sample Coefficients for Y Table")
  print(coefs_group_aggr)
  
  writeLines("\n")
  print("Individual Covariate Selection Table")
  print(cov_selection_aggr)
  
  #writeLines("\n")
  #print("Covariate Selection Dispersion by Covariate Table")
  #print(cov_dispersion_ind)
  
  writeLines("\n")
  print("Grouped Covariate Selection Table")
  print(cov_selection_group_aggr)
  
  writeLines("\n")
  print("Covariate Selection Dispersion Within Groups Table")
  print(cov_dispersion_group)
  
  writeLines("\n")
  print("Results Table")
  print(results_aggr)
  
  # Sim parameters
  params <- data.frame(
    preset <- c("n_obs",
                "n_rep",
                "SE_req",
                "data_split",
                "target_r_sq_X",
                "target_r_sq_Y",
                "asymmetry",
                "l_zero_X",
                "l_zero_Y"),
    value  <- c(n_obs,
                n_rep,
                SE_req,
                null_string_catch(data_split),
                target_r_sq_X,
                target_r_sq_Y,
                asymmetry,
                l_zero_X,
                l_zero_Y)
  )
  
  if (record_results) {
    case_string <- paste("c", (ncol(coef_data) - 5), sep = "")
    
    if (using_shiny) { setwd("..") }
    
    message("\nSaving results to file\n")
    
    # Save oracle coef data
    write.csv(coef_data, paste("../data/", case_string, "-coef-data.csv", sep = ""), row.names = FALSE)
    
    # Save one simulated data-set
    write.csv(representative_data, paste("../data/", case_string, "-dataset.csv", sep = ""))
    
    # Save sample coefficients
    write.csv(coefs_aggr,       paste("../data/", case_string, "-model-coefs.csv", sep = ""))
    write.csv(coefs_group_aggr, paste("../data/", case_string, "-model-coefs-grouped.csv", sep = ""))
    
    # Save covariate selection
    write.csv(cov_selection_aggr,       paste("../data/", case_string, "-cov-selection.csv", sep = ""))
    write.csv(cov_selection_group_aggr, paste("../data/", case_string, "-cov-selection-grouped.csv", sep = ""))
    
    # Save covariate dispersion
    write.csv(cov_dispersion_group, paste("../data/", case_string, "-cov-dispersion-grouped.csv", sep = ""))
    
    # Save results measures
    write.csv(results_aggr, paste("../data/", case_string, "-results-table.csv", sep = ""))
    
    # Save params of simulation
    write.csv(params, paste("../data/", case_string, "-params.csv", sep = ""))
    
    if (using_shiny) { setwd("sim_frontend") }
  }
  
  print("finished!")
}



