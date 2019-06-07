## New version of the party::varimp function: S3 Method "permimp"
## Should work for 
##  - "RandomForest" (party package) 
##  - "randomForest" (randomForest package)
##  - "randomForest.formula" (randomForest package)

permimp <- function(object, ...)
  UseMethod("permimp")


## --------------------------------------------------------------------------
## --------------------------------------------------------------------------


## permimp for non-randomForest and non-cforest objects
permimp.default <- function(object, ...)
  stop("The permimp functions only works for random forest objects from the party- and randomForest-packages.")


## --------------------------------------------------------------------------
## --------------------------------------------------------------------------

