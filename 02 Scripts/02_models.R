#------------------------------------------------------------------------------
# GLOBAL INPUTS
#------------------------------------------------------------------------------
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)
tic()

#------------------------------------------------------------------------------
# RANDOM FOREST MODEL
#------------------------------------------------------------------------------
# Create a recipe for the model
ranger_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # turn vars into dummy variables
  step_dummy(game_outcome, cluster, upset_expected, real_impact_proxy,
             experience, kicked_bag, huge_game) %>%
  # remove any correlated variables
  step_corr(all_predictors(), -all_nominal()) %>%
  # down sample
  step_nearmiss(brownlow_votes, under_ratio = 1, seed = 345)

# Create a model specification
ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

# Create a work flow
ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec)

# Set up a grid for the tuning process
rf_grid <- grid_regular(
  # The number of predictors that will be randomly sampled at
  # each split when creating the tree models.
  mtry(range = c(10, 15)),
  # The minimum number of data points in a node that are
  # required for the node to be split further
  min_n(range = c(5, 15)),
  levels = 3
)

# Check the accuracy on the bootstrap samples to tune hyper parameters
set.seed(567)
ranger_tune <-
  tune_grid(ranger_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, specificity),
            # pass the grid space
            grid = rf_grid
  )