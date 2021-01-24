#------------------------------------------------------------------------------
# XGBoost MODEL
#------------------------------------------------------------------------------
# Create a recipe for the model
xgboost_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # turn the game outcome into a dummy variables
  step_dummy(game_outcome) %>%
  # remove any correlated variables
  step_corr(all_predictors(), -all_nominal()) %>%
  # down sample
  step_nearmiss(brownlow_votes, under_ratio = 1.1, seed = 678) %>%
  # up sample
  step_bsmote(brownlow_votes, over_ratio = 1, seed = 789)

# Create a model specification
xgboost_spec <- 
  boost_tree(trees = 1000, 
             min_n = tune(), 
             tree_depth = tune(), 
             learn_rate = tune(), 
             loss_reduction = tune(), 
             mtry = tune(), 
             sample_size = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost") 

# Create a work flow
xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec)

# Check the accuracy on the bootstrap samples
set.seed(8910)
xgboost_tune <-
  tune_grid(xgboost_workflow, 
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = 5,
            # save the predictions
            control = control_stack_grid()
  )

