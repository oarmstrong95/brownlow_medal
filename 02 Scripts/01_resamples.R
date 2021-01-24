#------------------------------------------------------------------------------
# CREATE RESAMPLES FOR TUNING
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