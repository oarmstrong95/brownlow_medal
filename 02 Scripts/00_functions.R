#------------------------------------------------------------------------------
# DEFINE FUNCTIONS
#------------------------------------------------------------------------------
# Define function to normalise our numeric variables
normalise_fun <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

# Define function to check tuning parameters to optimize the grid search
# on the second tuning iteration
tuning_parameters_fun <- function(data){
  
  graph <- data %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    select(mean, min_n, mtry) %>%
    pivot_longer(min_n:mtry,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "AUC")
  
  return(graph)
  
}

