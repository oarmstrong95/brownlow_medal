#------------------------------------------------------------------------------
# RANDOM FOREST MODEL
#------------------------------------------------------------------------------
# Split the historical data into training and test sets
set.seed(123)
split <- initial_split(model_data, strata = brownlow_votes)
train <- training(split)
test <- testing(split)

# Split the training data into bootstrap samples
set.seed(234)
bootstrap_folds <- 
  bootstraps(train, strata = brownlow_votes)

# Create a recipe for the model
ranger_recipe <- 
  recipe(formula = brownlow_votes ~ ., data = model_data) %>%
  # create id roles for variables not used in the model
  step_rm(match_round, match_date, match_home_team,
          match_away_team, player_team, player_name) %>%
  update_role(season, match_id, player_id, new_role = 'id') %>%
  # turn the game outcome into a dummy variables
  step_dummy(game_outcome) %>%
  # down sample
  step_nearmiss(brownlow_votes, under_ratio = 1.1, seed = 345) %>%
  # up sample
  step_bsmote(brownlow_votes, over_ratio = 1, seed = 456) %>%
  # remove any correlated variables
  step_corr(all_predictors(), -all_nominal())

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

# # Set up a grid for the tuning process
# rf_grid <- grid_regular(
#   # The number of predictors that will be randomly sampled at 
#   # each split when creating the tree models.
#   mtry(range = c(5, 15)),
#   # The minimum number of data points in a node that are 
#   # required for the node to be split further
#   min_n(range = c(5, 25)),
#   levels = 5
# )

# Start parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl, cores = detectCores())

# Check the accuracy on the bootstrap samples
set.seed(567)
ranger_tune <-
  tune_grid(ranger_workflow,
            # pass the bootstrap folds
            resamples = bootstrap_folds,
            # specify the metrics to assess the model on
            metrics = 
              metric_set(roc_auc, accuracy, sensitivity, 
                         specificity),
            # pass the grid space
            grid = 25,
            # save the predictions
            control = control_grid(save_pred = TRUE)
  )

# End parallel processing
stopImplicitCluster()

#------------------------------------------------------------------------------
# CHECK ACCURACY ON THE TRAINING DATA
#------------------------------------------------------------------------------
ranger_best <- ranger_tune %>%
  select_best(metric = "roc_auc")

ranger_metrics <- ranger_tune %>%
  collect_metrics()

ranger_best_string <- ranger_best %>%
  select(.config) %>%
  pull()

ranger_metrics_best_tune <- ranger_metrics %>%
  filter(.config == ranger_best_string)

ranger_roc_graph <- ranger_tune %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(brownlow_votes, .pred_0:.pred_3) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  facet_wrap(~.level) +
  coord_equal()

ranger_conf_mat <- ranger_tune %>%
  collect_predictions() %>%
  conf_mat(brownlow_votes, .pred_class)


