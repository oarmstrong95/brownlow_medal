#------------------------------------------------------------------------------
# KKNN
#------------------------------------------------------------------------------
# Create a recipe for the model
kknn_recipe <- 
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
  step_nearmiss(brownlow_votes, under_ratio = 1.1, seed = 91011) %>%
  # up sample
  step_bsmote(brownlow_votes, over_ratio = 1, seed = 101112) 

# Create a model specification
kknn_spec <- 
  nearest_neighbor(neighbors = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn")

# Create a work flow
kknn_workflow <- 
  workflow() %>% 
  add_recipe(kknn_recipe) %>% 
  add_model(kknn_spec)

# Set up a grid for the tuning process
kknn_grid <- grid_regular(
  # The number of predictors that will be randomly sampled at
  # each split when creating the tree models.
  neighbors(range = c(10, 20)),
  levels = 3
)

# Check the accuracy on the bootstrap samples
set.seed(111213)
kknn_tune <-
  tune_grid(kknn_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = kknn_grid,
            # save the predictions
            control = control_stack_grid()
  )


