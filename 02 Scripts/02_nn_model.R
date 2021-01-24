#------------------------------------------------------------------------------
# NEURAL NETWORK
#------------------------------------------------------------------------------
# Create a recipe for the model
nn_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # turn the game outcome into a dummy variables
  step_dummy(game_outcome) %>%
  # remove any correlated variables
  step_pca(all_predictors()) %>%
  # down sample
  step_nearmiss(brownlow_votes, under_ratio = 1.1, seed = 121314) %>%
  # up sample
  step_bsmote(brownlow_votes, over_ratio = 1, seed = 131415) 

# Create a model specification
nn_spec <- 
  mlp(epochs = 100,
      hidden_units = tune(),
      dropout = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kernlab")

# Create a work flow
nn_workflow <- 
  workflow() %>% 
  add_recipe(nn_recipe) %>% 
  add_model(nn_spec)

# Check the accuracy on the bootstrap samples
set.seed(141516)
nn_tune <-
  tune_grid(nn_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = 25,
            # save the predictions
            control = control_stack_grid()
  )
