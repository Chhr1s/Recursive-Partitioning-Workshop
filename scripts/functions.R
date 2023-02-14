
mse <- function(observed_y, predicted_y){
  
    (sum(observed_y - predicted_y)^2)/length(observed_y)
  
}

rmse <- function(observed_y, predicted_y){
  sqrt(
    mse(observed_y, predicted_y)
    )
}



mae <- function(observed_y, predicted_y) 
{mean(abs(observed_y - predicted_y))/length(observed_y)}

# internal function to `cv_it()`
fit_model_ <- 
  function(
    model_type, 
    tuning_grid, 
    mod_formula,
    analysis_data, 
    tuning_param1, 
    tuning_param2,
    tuning_param3 = NULL
    ){
  if (model_type == 'dt'){
    
    
      return(rpart(
        data = analysis_data, 
        formula = mod_formula, 
        cp = tuning_param1,
        minsplit = tuning_param2
      ))
    
  }
  
  if (model_type == 'rf'){
    
    if(is.null(tuning_param3)){
    
      ranger(
        data = analysis_data, 
        formula = mod_formula, 
        mtry = tuning_param1,
        num.trees = tuning_param2
      )
      
    }
    
    ranger(
      data = analysis_data, 
      formula = mod_formula, 
      mtry = tuning_param1,
      num.trees = tuning_param2,
      min.node.size = tuning_param3
      
    )
    

  }
}




# function for cv & tuning

cv_it_complex <- 
  function (
    cv_obj, 
    model_type,
    outcome_string,
    seed = 713, 
    mod_formula, 
    tuning_grid, 
    verbose = TRUE,
    mode = 'regression',
    ...) 
  {
    # want it to be reproducible, even if you don't remember to set it
    set.seed(seed)
    # assess "k" or "v" used for k-fold cross validation
    number_cv_sets <- length(cv_obj$splits)
    
    results <- tibble()
    # loop through the tuning grid
    for (j in 1:nrow(tuning_grid)){
      
      # store tuning parameters from grid
      
      if (model_type == 'rf'){
        tp1 <- tuning_grid$mtry[[j]]
        tp2 <- tuning_grid$trees[[j]]
        
        if (!is.null(tuning_grid$min_n)){
          tp3 <- tuning_grid$min_n[[j]]
        }
          
      }
      
      if (model_type == 'dt'){
        
        tp1 <- tuning_grid$cp_[[j]]
        tp2 <- tuning_grid$minsize_[[j]]
      }
      
      # make vectors for the fit metrics
      fit_1 <- vector(mode = 'numeric', length = number_cv_sets)
      fit_2 <- vector(mode = 'numeric', length = number_cv_sets)
      
      # loop through cv object at this tuning value
      for (i in 1:number_cv_sets) {
        # use analysis set for training
        
        temp_analysis <- analysis(cv_obj$splits[[i]])
        # fit model
        
        fitted_result <- 
          fit_model_(
            model_type = model_type, 
            tuning_grid = tuning_grid, 
            mod_formula = mod_formula,
            analysis_data = temp_analysis,
            tuning_param1 = tp1,
            tuning_param2 = tp2,
            tuning_param3 = tp3
          )
        
        
        
        # get predictions
        temp_assessment <- assessment(cv_obj$splits[[i]])
        
        if (model_type == 'rf') {
          temp_predictions <- predict(fitted_result, data = temp_assessment)$predictions
        }
        
        if (model_type == 'dt'){
          
          temp_predictions <- predict(fitted_result, newdata = temp_assessment)
        }
        temp_new_Y <- temp_assessment %>% pull(sym(outcome_string))
        
        # store fit metrics
        
        if (mode == 'regression')
        {
          fit_type1 <- 'mse_mean'
          fit_type2 <- 'mae_mean'
          fit_type1se <- 'mse_se'
          fit_type2se <- 'mae_se'
        
          
          fit_1[i] <- mse(observed_y = temp_new_Y, predicted_y = temp_predictions)
          fit_2[i] <- mae(observed_y = temp_new_Y, predicted_y = temp_predictions)
          
        }
        
        if (mode == 'classification'){
          fit_type1 <- 'classification_accuracy'
          fit_type2 <- 'mae'
          warning('classification not set up')
        }
        
        if(verbose == TRUE) {message(paste0("cv index ", i, " complete"))}
      }
      # take average
      mean_fit_1 <- mean(fit_1)
      mean_fit_2 <- mean(fit_2)
      # calculate std. error
      se_fit_1 <- sd(fit_1)/sqrt(length(fit_1))
      se_fit_2 <- sd(fit_2)/sqrt(length(fit_2))
      
      # put it together
      temp_results <- 
        tuning_grid[j, ] %>% 
        mutate(
          grid_index = j, 
          '{fit_type1}' := mean_fit_1, 
          '{fit_type1se}' := se_fit_1,
          '{fit_type2}' := mean_fit_2,
          '{fit_type2se}' := se_fit_2
        ) %>% 
        select(grid_index, everything())
      
      
      results <- bind_rows(results, temp_results)
      if(verbose == TRUE) {message(paste0("hyperparameter index ", j, " complete"))}
      
    }
    return(results)
  }
