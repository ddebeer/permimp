## This file contains methods for randomForest (randomForest) 
## and RandomForest (party) objects


## --------------------------------------------------------------------------
## --------------------------------------------------------------------------


## getTree Method
getTree <- function(object, treeNr) 
  UseMethod("getTree")


# getTree.default
getTree.default <- function(object, treeNr)
{
  stop("The ", sQuote("getree"), " function only works for random forest objects from the party- and randomForest-packages.")
}


# getTree Method for randomForest - based on randomForest::getTree
getTree.randomForest <- function(object, treeNr)
{
  oneTree <- object
  oneTree$forest$ndbigtree <- oneTree$forest$ndbigtree[treeNr]
  oneTree$forest$nodestatus <- oneTree$forest$nodestatus[, treeNr, drop = FALSE]
  if(object$type == "regression"){
    oneTree$forest$leftDaughter <- oneTree$forest$leftDaughter[, treeNr, drop = FALSE]
    oneTree$forest$rightDaughter <- oneTree$forest$rightDaughter[, treeNr, drop = FALSE]
  } else if(object$type == "classification"){
    oneTree$forest$treemap <- oneTree$forest$treemap[, , treeNr, drop = FALSE]
  }
  oneTree$forest$bestvar <- oneTree$forest$bestvar[, treeNr, drop = FALSE]
  oneTree$forest$nodepred <- oneTree$forest$nodepred[, treeNr, drop = FALSE]
  oneTree$forest$xbestsplit <- oneTree$forest$xbestsplit[, treeNr, drop = FALSE]
  oneTree$forest$ntree <- oneTree$ntree <- 1
  return(oneTree)
}


# getTree Method for RandomForest (party)
getTree.RandomForest <- function(object, treeNr)
{
  return(object@ensemble[[treeNr]])
}


## --------------------------------------------------------------------------


## getOOB Method
getOOB <- function(object, treeNr) 
  UseMethod("getOOB")


# getOOB.default
getOOB.default <- function(object, treeNr)
{
  stop("The ", sQuote("getOOB"), " function only works for random forest objects from the party- and randomForest-packages.")
}


# getOOB Method for randomForest (randomForest package)
getOOB.randomForest <- function(object, treeNr)
{
  return(object$inbag[, treeNr] == 0)
}


# getOOB Method for RandomForest (party)
getOOB.RandomForest <- function(object, treeNr)
{
  return(object@weights[[treeNr]] == 0)
}


## --------------------------------------------------------------------------


## getOutcomeType Method
getOutcomeType <- function(object)
  UseMethod("getOutcomeType")


# Function to get the outcome type for randomForest (randomForest)
getOutcomeType.randomForest <- function(object)
  return(object$type)
  
  
# Function to get the outcome type for RandomForest (party)
getOutcomeType.RandomForest <- function(object)
{
  # multivariate outcome
  if(length(object@responses@variables) != 1)
    stop("cannot compute permutation importance measure for multivariate response")
  # survival outcome
  if (length(object@responses@variables) == 1 && inherits(object@responses@variables[[1]], "Surv")){
    stopifnot(requireNamespace("ipred", quietly = TRUE))
    cat("\nPermutation importance for survival forests; this feature is _experimental_\n\n")
    return("survival")
  }
  else {
    # NO survival outcome
    # nominal outcomes (classification)
    if (all(object@responses@is_nominal)) {
      if(nlevels(object@responses@variables[[1]]) == 2) return("nominal2")
      else return("nominal")
    }
    else {
      # ordinal outcomes (classification)
      if (all(object@responses@is_ordinal)) return("ordinal")
      # interval/continous outcome (regression)
      else return("regression")
    }
  }
}


## --------------------------------------------------------------------------


## selectPred Method
selectPred <- function(object, type, w, inp, y)
  UseMethod("selectPred")


# Function to select the prediction function for randomForest (randomForest)
selectPred.randomForest <- function(object, type, w, inp, y)
  return(function(tree, inputs = NULL,  mincriterion = NULL, varPerm = NULL, input = NULL) 
    
    ## --------------------------------------
    ## return(getPredsRF(tree, newData = input)))   # for old tree object
    ## --------------------------------------
    
    return(predict(tree, newdata = input)))


# Function to select the prediction function for RandomForest (party)
selectPred.RandomForest <- function(object, type, w, inp, y)
{
  if (type == "survival") return(
    function(tree, inputs, mincriterion, varperm = -1L, input = NULL,
             weights = w, oldinputs = inp, Y = y){
      where <- .R_get_nodeID(tree, oldinputs, mincriterion, -1L)
      wh <- .R_get_nodeID(tree, inputs, mincriterion, as.integer(varperm))
      swh <- sort(unique(wh))
      RET <- vector(mode = "list", length = length(wh))
      for (i in 1:length(swh)) {
        w <- weights * (where == swh[i])
        RET[wh == swh[i]] <- list(mysurvfit(Y, weights = w))
      }
      return(RET)
    }
  )
  else {return(function(tree, inputs, mincriterion, varperm = -1L, input = NULL)
    return(.R_predict(tree, inputs, mincriterion, as.integer(varperm)))
  )
  }
}



## --------------------------------------------------------------------------


