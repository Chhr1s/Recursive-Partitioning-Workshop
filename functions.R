
rmse <- function(observed_y, predicted_y){
  sqrt(
    (sum(observed_y - predicted_y)^2)/length(observed_y)
    )
}

# function for cv & tuning

cv_it <- 
  function (
    cv_obj, 
    outcome_string,
    seed = 713, 
    mod_formula, 
    tuning_grid, 
    verbose = TRUE,
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
      mtry_temp <- tuning_grid$mtry[[j]]
      trees_temp <- tuning_grid$trees[[j]]
      
      # make vectors for the fit metrics
      rmse_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      mae_temp <- vector(mode = 'numeric', length = length(number_cv_sets))
      
      # loop through cv object at this tuning value
      for (i in 1:number_cv_sets) {
        # use analysis set for training
        temp_analysis <- analysis(cv_obj$splits[[i]])
        # fit rf
        
        
        fitted_result <- 
          ranger(
            data = temp_analysis, 
            formula = mod_formula, 
            mtry = mtry_temp,
            num.trees = trees_temp
          )
        # get predictions
        temp_assessment <- assessment(cv_obj$splits[[i]])
        temp_predictions <- predict(fitted_result, data = temp_assessment)
        temp_new_Y <- temp_assessment %>% pull(sym(outcome_string))
        
        # store fit metrics
        rmse_temp[i] <- rmse(observed_y = temp_new_Y, predicted_y = temp_predictions$predictions)
        mae_temp[i] <- mae(observed_y = temp_new_Y, predicted_y = temp_predictions$predictions)
        
        if(verbose == TRUE) {message(paste0("cv index ", i, " complete"))}
      }
      # take average
      mean_rmse <- mean(rmse_temp)
      mean_mae <- mean(mae_temp)
      # calculate std. error
      se_rmse <- sd(rmse_temp)/sqrt(length(rmse_temp))
      se_mae <- sd(mae_temp)/sqrt(length(mae_temp))
      
      # put it together
      temp_results <- 
        tuning_grid[j, ] %>% 
        mutate(
          grid_index = j, 
          mean_rmse = mean_rmse, 
          se_rmse = se_rmse,
          mean_mae = mean_mae,
          se_mae = se_mae
        ) %>% 
        select(grid_index, everything())
      
      
      results <- bind_rows(results, temp_results)
      if(verbose == TRUE) {message(paste0("hyperparameter index ", j, " complete"))}
      
    }
    return(results)
  }
