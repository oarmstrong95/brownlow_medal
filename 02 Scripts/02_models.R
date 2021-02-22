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
  step_dummy(game_outcome, cluster, upset_expected, real_impact_proxy) %>%
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
            grid = rf_grid,
            # save the predictions
            control = control_stack_grid()
  )

# #------------------------------------------------------------------------------
# # XGBoost MODEL
# #------------------------------------------------------------------------------
# # Create a recipe for the model
# xgboost_recipe <- 
#   recipe(formula = brownlow_votes ~ ., data = model_data) %>%
#   # create id roles for variables not used in the model
#   step_rm(match_round, match_date, match_home_team,
#           match_away_team, player_team, player_name) %>%
#   update_role(season, match_id, player_id, new_role = 'id') %>%
#   # turn the game outcome into a dummy variables
#   step_dummy(game_outcome) %>%
#   # remove any correlated variables
#   step_corr(all_predictors(), -all_nominal()) %>%
#   # down sample
#   step_nearmiss(brownlow_votes, under_ratio = 1.1, seed = 678) %>%
#   # up sample
#   step_bsmote(brownlow_votes, over_ratio = 1, seed = 789)
# 
# # Create a model specification
# xgboost_spec <- 
#   boost_tree(trees = 1000, 
#              min_n = tune(), 
#              tree_depth = tune(), 
#              learn_rate = tune(), 
#              loss_reduction = tune(), 
#              mtry = tune(), 
#              sample_size = tune()) %>% 
#   set_mode("classification") %>% 
#   set_engine("xgboost") 
# 
# # Create a work flow
# xgboost_workflow <- 
#   workflow() %>% 
#   add_recipe(xgboost_recipe) %>% 
#   add_model(xgboost_spec)
# 
# # Set up a grid for the tuning process
# xgb_grid <- grid_latin_hypercube(
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), bootstrap_folds),
#   learn_rate(),
#   size = 25
# )
# 
# # Check the accuracy on the bootstrap samples
# set.seed(8910)
# xgboost_tune <-
#   tune_grid(xgboost_workflow, 
#             # pass the bootstrap folds
#             resamples = bootstrap_folds,
#             # specify the metrics to assess the model on
#             metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
#             # pass the grid space
#             grid = xgb_grid,
#             # save the predictions
#             control = control_stack_grid()
#   )
# 
# #------------------------------------------------------------------------------
# # SUPPORT VECTOR MACHINE 
# #------------------------------------------------------------------------------
# # Create a recipe for the model
# svm_recipe <- 
#   recipe(formula = brownlow_votes ~ ., data = model_data) %>%
#   # create id roles for variables not used in the model
#   step_rm(match_round, match_date, match_home_team,
#           match_away_team, player_team, player_name) %>%
#   update_role(season, match_id, player_id, new_role = 'id') %>%
#   # turn the game outcome into a dummy variables
#   step_dummy(game_outcome) %>%
#   # remove any correlated variables
#   step_pca(all_predictors()) %>%
#   # down sample
#   step_nearmiss(brownlow_votes, under_ratio = 1.1, seed = 121314) %>%
#   # up sample
#   step_bsmote(brownlow_votes, over_ratio = 1, seed = 131415) 
# 
# # Create a model specification
# svm_spec <- 
#   svm_poly(cost = tune(),
#            degree = tune(),
#            scale_factor = tune()) %>% 
#   set_mode("classification") %>% 
#   set_engine("kernlab")
# 
# # Create a work flow
# svm_workflow <- 
#   workflow() %>% 
#   add_recipe(svm_recipe) %>% 
#   add_model(svm_spec)
# 
# # Set up a grid for the tuning process
# svm_grid <- 
#   grid_regular(parameters(svm_spec), levels = 9) %>%
#   filter(scale_factor > 0.05) %>%
#   filter(cost > 0.1) %>%
#   filter(degree < 3)
# 
# # Check the accuracy on the bootstrap samples
# set.seed(141516)
# svm_tune <-
#   tune_grid(svm_workflow,
#             # pass the bootstrap folds
#             resamples = bootstrap_folds,
#             # specify the metrics to assess the model on
#             metrics = 
#               metric_set(roc_auc, accuracy, sensitivity, specificity),
#             # pass the grid space
#             grid = svm_grid,
#             # save the predictions
#             control = control_stack_grid()
#   )
# 
# 
# # Create df to store model checks
# sub_models <- tibble(model = c("xgboost", "svm", "randomforest"),
#                      tuned_ouputs = list(xgboost_tune, svm_tune, ranger_tune))
# 
# # Imperatively apply function to tune outputs to check 
# # if hyper parameters are optimised for best model
# check_tuned_results <- sub_models %>%
#   mutate(metrics = map(tuned_ouputs, collect_metrics),
#          roc_graphs = map(tuned_ouputs, roc_curve_fun))

toc()