#------------------------------------------------------------------------------
# GLOBAL INPUT
#------------------------------------------------------------------------------
setwd("Z:/My Documents/GitHub/brownlow_medal/02 Scripts")

#------------------------------------------------------------------------------
# BASE SETUP
#------------------------------------------------------------------------------
source('00_packages.R')
source('00_functions.R')
source('01_model_data.R')

#------------------------------------------------------------------------------
# GET DATA
#------------------------------------------------------------------------------
# Run the function to get entire data set
output <- get_data(MIN_YEAR = 2017, MAX_YEAR = 2020)

# Split the data into previous seasons and new seasons
model_data <- output[[1]]
new_data <- output[[2]]

#------------------------------------------------------------------------------
# RUN MODELS
#------------------------------------------------------------------------------
source('01_resamples.R')
source('02_models.R')

# See how the model performs on the test set
metrics <- out_of_sample_accuracy()

#------------------------------------------------------------------------------
# GET PREDICTED VOTES
#------------------------------------------------------------------------------
# Fit the final model and predict on the new data
predicted_votes <- predict_function()

#------------------------------------------------------------------------------
# OUTPUTS
#------------------------------------------------------------------------------
top20 <- totals_table()

ranger_tune
