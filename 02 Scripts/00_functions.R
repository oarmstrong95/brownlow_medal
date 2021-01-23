#------------------------------------------------------------------------------
# DEFINE FUNCTIONS
#------------------------------------------------------------------------------
# Define function to normalise our numeric variables
normalise_fun <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}