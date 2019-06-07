## doPermimp is the workinghorse of the permimp methods. 
## is called by all the permimp methods.

doPermimp <- function(object, input, inp, y, OOB, threshold, conditional, 
                      whichxnames, ntree, nperm, scaled,
                      progressBar, thresholdDiagnostics, 
                      w, AUC, pre1.0_0, mincriterion, asParty)
{
  # Check if conditional permuation importance is possible
  if (conditional) {
    if(!all(complete.cases(input)))
      stop("cannot compute variable importance measure with missing values")
    
    if (conditional && threshold == 1) {
      warning(sQuote("permimp"), 
              paste0(": Unable to permute conditionally. \n",  
                     "The chosen threshold is too high, no variables to condition on were selected. \n",
                     "Instead the unconditional permimp values are computed. "),
              call. = FALSE, immediate. = TRUE)
      doPermimpCall <- match.call()
      doPermimpCall$conditional <- FALSE
      return(eval(doPermimpCall))
    }
  }
  
  # select the predictors for which to compute the permutation importance
  xnames <- colnames(input)
  if(is.null(whichxnames)) {
    whichxnames <- xnames
    whichVarIDs <- seq_along(xnames)
  }
  else {
    whichVarIDs <- match(whichxnames, table = xnames)
    if(length(whichVarIDs) < 1){
      stop("Error: whichxnames is not a subset of the predictor variable names in the forest.")
    }
    whichVarIDs <- whichVarIDs[order(whichVarIDs)]
  } 
  
  # Check outcome and selet the relevant error- and pred-functions
  type <- getOutcomeType(object)
  error <- selectError(type)
  nullError <- selectNullError(type)
  pred <- selectPred(object, type, w, inp, y)
  
  # when asParty == TRUE, collect cond list first
  if (conditional && asParty) {
    cond_list <- create_cond_list(binnedVars = NULL, threshold,
                                  input, seq_along(xnames), asParty = TRUE)
  }
  
  ## list for several permutations
  ## this array is initialized with values 0 so that a tree that does not 
  ## contain the current variable adds importance 0 to its average importance
  perror <- array(0, dim = c(ntree, length(xnames), nperm), 
                  dimnames = list(NULL, xnames, NULL))
  
  ## this matrix will be used to give suggestions to de/increase the used threshold
  ## it is initialized with values NA.
  changeThres <- array(NA, dim = c(ntree, length(xnames), nperm), 
                       dimnames = list(NULL, xnames, NULL))
  
  # start progressbar
  if(progressBar) pBar <- txtProgressBar(min = 0, max = ntree, 
                                         style = 3, char = "|")
  
  # for all trees (treeNr) in the forest
  for (treeNr in seq_len(ntree)){
    tree <- getTree(object, treeNr)
    
    ## if OOB == TRUE use only oob observations, otherwise use all observations in learning sample
    if(OOB){oob <- getOOB(object, treeNr)} else {oob <- rep(TRUE, length(y))}
    
    ## prediction & error before permutation
    p <- pred(tree, inp, mincriterion, -1L, input)
    eoob <- error(p, oob, y)
    
    ## select variables that are used for splitting in the current tree
    varsInTree <- intersect(unique(varIDs(tree)), whichVarIDs)
    
    ## Only make the binned variables based on splitting points when conditional == TRUE
    if(conditional) {
      ## make list of variables, categorized/binned using the used splitting points
      binnedVars <- makeBinnedVars(varsInTree, tree, oob, input) 
      if (!asParty) {
        cond_list <- create_cond_list(binnedVars, threshold, input, varsInTree, asParty = FALSE)
      }
    }
    
    
    ## for all variables (j) in the tree (j = number of variables) 
    for(j in varsInTree){
      
      ## for every permutation
      for (per in 1:nperm){
        if (!conditional && !pre1.0_0){
          ## splitwise permutation only possible for RandomForest (party) object.
          p <- pred(tree, inp, mincriterion, as.integer(j))
        }
        else {
          if(conditional){
            changeThres[treeNr, j, per] <- 0  # if variable is in tree, NA -> 0
            ## only condition on variables that are in tree, 
            ## and that are associated with the current variable
            if(asParty) 
              varsToCondOn <- intersect(cond_list[[as.character(j)]], varsInTree)
            else varsToCondOn <- cond_list[[as.character(j)]]
            
            if(length(varsToCondOn) < 1){
              ## If there are no variables to condition on, conditionally permution is impossible.
              ## -1 corresponds to a suggestion to decrease the used threshold
              changeThres[treeNr, j, per] <- -1  
              perm <- sample(which(oob))
            } else {
              perm <- conditional_perm(varID = j, varsToCondOn,
                                       binnedVars, oob, asParty)
            }
          }
          else{
            perm <- sample(which(oob))
          }
          if(is.null(perm)) {
            ## if conditionally permution cannot result in different outcomes: 
            ## (a) +1 correstponds to a suggestion to increase the threshold; (treeNr) jump to next varInTree
            changeThres[treeNr, j, per] <- 1
            break}
          
          ## different treatment for 
          if(!is.null(inp)){
            tmp <- inp
            ## switch observations for only the selected var
            tmp@variables[[j]][oob] <- tmp@variables[[j]][perm]
            
            ## predict with permuted var 
            p <- pred(tree, tmp, mincriterion, -1L)
          }
          else {
            tmp <- input
            ## switch observations for only the selected var
            tmp[oob,j] <- tmp[perm,j] 
            
            ## predict with permuted var 
            p <- pred(tree, input = tmp)
          }
        }
        
        ## run through all rows of perror
        perror[treeNr, j, per] <- (error(p, oob, y) - eoob)
        
      } ## end of for (per in 1:nperm)
    } ## end of for(j in varsInTree)
    
    if(scaled){
      perror[treeNr, , per] <- perror[treeNr, , per]/nullError(y, oob)
    }
    
    if(progressBar) setTxtProgressBar(pBar , treeNr)
    } ## end of for (treeNr in 1:ntree)
    
    perror <- apply(perror[ , whichVarIDs, , drop = FALSE], c(1, 2), mean)
    perror <- as.data.frame(perror)
    
    if(thresholdDiagnostics){
      changeThres <- apply(changeThres[ , whichVarIDs, , drop = FALSE], 2, mean, na.rm = TRUE)
      increaseThres <- changeThres > .5
      decreaseThres <- changeThres < -.5
      if(any(increaseThres[!is.na(increaseThres)])){
        warning(sQuote("permimp"), 
                paste0(" Unable to permute conditionally for ",
                       sum(increaseThres), 
                       " variable(s) in 50 percent of the cases.\n",  
                       "Increasing the threshold may help. \n", 
                       "The variables for which conditionally permuting (often) was impossible are: ",
                       ifelse(sum(increaseThres) > 6, 
                              paste0("(showing only six) \n   - ", 
                                     paste0(whichxnames[increaseThres][1:6], collapse = "\n   - ")),
                              paste0("\n   - ", 
                                     paste0(whichxnames[increaseThres], collapse = "\n   - ")))),
                call. = FALSE, immediate. = TRUE)
      }
      if(any(decreaseThres[!is.na(decreaseThres)])){
        warning(sQuote("permimp"), 
                paste0(" Conditionally permuting the predictor values of ",
                       sum(decreaseThres),
                       " variable(s) had no impact in 50 percent of the cases.\n",  
                       "Decreasing the threshold may help. \n", 
                       "The variables for which conditionally permuting (often) had no impact are: ",
                       ifelse(sum(decreaseThres) > 6, 
                              paste0("(showing only six) \n   - ", 
                                     paste0(whichxnames[decreaseThres][1:6], collapse = "\n   - ")),
                              paste0("\n   - ", 
                                     paste0(whichxnames[decreaseThres], collapse = "\n   - ")))),
                call. = FALSE, immediate. = TRUE)
      }
    }
    
    info <- list()
    if(conditional){
      info$threshold = threshold
      if (asParty) info$conditioning = "as party"
      else info$conditioning = "permimp implementation"
    } 
    info$outcomeType = type
    if (info$outcomeType == "nominal2") info$outcomeType <- "binary"
    if (type == "survival") info$errorType <- "Brier score"
    else if (type == "regression") info$errorType <- "MSE"
    else info$errorType <- "accuracy"
    if(AUC && type == "nominal2") info$errorType <- "AUC"  
    
    # if(scaled) return(ScaledMeanDecreaseAccuracy = colMeans(perror)/apply(perror, 2, sd)) 
    out <- as.VarImp(perror, 
                     FUN = mean,
                     type = 'if'(conditional, "Conditional Permutation", "Permutation"),
                     info = info)
    if(progressBar) close(pBar)
    return(out)
}