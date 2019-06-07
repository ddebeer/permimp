## these functions help with dealing with party-objects and party-functionallity

## Wrapper around internal party function.
.R_predict <- function(tree, inputs, mincriterion, varperm)
  party::party_intern(tree, inputs, mincriterion, varperm, fun = "R_predict")


## Wrapper around internal party function.
.R_get_nodeID <- function(tree, inputs, mincriterion, varperm)
  party::party_intern(tree, inputs, mincriterion, varperm, fun = "R_get_nodeID")


## non-exported party functions: mysurvfit()

## mysurvfit()
mysurvfit <- function(y, weights, ...) {
  
  stopifnot(extends(class(y), "Surv"))
  ### see comment on weights and subset in ?survfit
  y <- y[weights > 0,]
  weights <- weights[weights > 0]
  return(survival::survfit(y ~ 1, weights = weights, ...))
}


## getdepth() returns an integer with the maximal depth of a tree (only internal nodes)
getDepth <- function(tree) 
{
  depth <- function(node) {
    if (node[[4]]) return(0)
    current <- 1
    nextLeft <- depth(node[[8]])
    nextRight <- depth(node[[9]])
    return(current + max(nextLeft, nextRight))
  }
  return(depth(tree))
}


## countSplits() returns the counts for a selection of variable IDs
## It is a simple and fast implementation of table(), allowing 
## more/less values to compute the frequency for.
countSplits <- function(SplitVars, whichVarIDs){
  vapply(whichVarIDs, 
         function(varID){sum(SplitVars == varID)}, FUN.VALUE = 1L)
}
