## Safer version. See examples of "?sample"
.resample <- function(x, ...) x[sample.int(length(x), ...)]



## Quick Chisq.test.
quickChisqTest <- function(table)
{
   n <- sum(table)
   sr <- rowSums(table)
   sc <- colSums(table)
   E <- outer(sr, sc, "*")/n
   return(sum((table - E)^2/E))
}

# ## Quick Chisq.test (with correction based on Haberman (1988), 
# ## which comes from Zelterman (1987))
# quickChisqTest <- function(table)
# {
#   n <- sum(table)
#   sr <- rowSums(table)
#   sc <- colSums(table)
#   E <- outer(sr, sc, "*")/n
#   return(sum( ( (table - E)^2 - table + E ) / E ))
# }


## faster interaction
fastInteraction <- function(list){
   out <- as.factor(do.call(paste0, list))
   levels(out) <- seq_len(nlevels(out))
   return(out)
}


## get mode of a factor
Mode <- function(x) {
  lx <- levels(x)
  lx[which.max(tabulate(match(x, lx)))]
}


## getSplitVars() returns a vector with the variable IDs for 
## each split in a tree.
getSplitVars <- function(tree) 
{
   getSplitVar <- function(node) {
      if (node[[4]]) return(NULL)
      current <- node[[5]][[1]]
      nextLeft <- getSplitVar(node[[8]])
      nextRight <- getSplitVar(node[[9]])
      return(c(current, nextLeft, nextRight))
   }
   return(getSplitVar(tree))
}


## Function to select the error function; based on party.
selectError <- function(type, AUC = FALSE)
{
  ## when AUC = TRUE
  if (AUC && !type == "nominal2")
    warning("AUC = TRUE works only for binary y\n classification; error rate is used instead of AUC")
  if (AUC && type == "nominal2")
    return(function(x, oob, y) {
      xoob <- sapply(x, function(x) x[1])[oob]
      yoob <- y[oob]
      which1 <- which(yoob==levels(y)[1])
      noob1 <- length(which1)
      noob <- length(yoob)
      if (noob1==0|noob1==noob) { return(NA) }       # AUC cannot be computed if all OOB-observations are from one class
      return(1-sum(kronecker(xoob[which1] , xoob[-which1],">"))/(noob1*(length(yoob)-noob1)))       # calculate AUC
    })
  if (type == "survival") 
    return(function(x, oob, y) ipred::sbrier(y[oob, , drop = FALSE], x[oob]))
  else if (type %in% c("nominal2", "nominal"))
    return(function(x, oob, y) mean((levels(y)[sapply(x, which.max)] != y)[oob]))
  else if (type == "ordinal")
    return(function(x, oob, y) mean((sapply(x, which.max) != y)[oob]))
  else if (type == "regression")
    return(function(x, oob, y) mean((unlist(x) - y)[oob]^2))
  else if (type == "classification")
    return(function(x, oob, y) mean((levels(y)[x] != y)[oob]))
}


## Function to select the baseRate function; based on party.
#selectBaseRate <- function(type, AUC = FALSE)
#{
#  ## when AUC = TRUE
#  if (AUC && !type == "nominal2")
#    warning("AUC = TRUE works only for binary y\n classification; error rate is used instead of AUC")
#  if (AUC && type == "nominal2")
#    return(function(x, oob, y) {
#      xoob <- sapply(x, function(x) x[1])[oob]
#      yoob <- y[oob]
#      which1 <- which(yoob==levels(y)[1])
#      noob1 <- length(which1)
#      noob <- length(yoob)
#      if (noob1==0|noob1==noob) { return(NA) }       # AUC cannot be computed if all OOB-observations are from one class
#      return(1-sum(kronecker(xoob[which1] , xoob[-which1],">"))/(noob1*(length(yoob)-noob1)))       # calculate AUC
#    })
#  if (type == "survival") 
#    return(function(x, oob, y) ipred::sbrier(y[oob, , drop = FALSE], x[oob]))
#  else if (type %in% c("nominal2", "nominal"))
#    return(function(x, oob, y) mean((levels(y)[sapply(x, which.max)] != y)[oob]))
#  else if (type == "ordinal")
#    return(function(x, oob, y) mean((sapply(x, which.max) != y)[oob]))
#  else if (type == "regression")
#    return(function(forest, oob, y) mean((unlist(x) - y)[oob]^2))
#  else if (type == "classification")
#    return(function(x, oob, y) mean((levels(y)[x] != y)[oob]))
#}


## Function to select the observed variance function.
selectNullError <- function(type)
{
  if (type == "survival") 
    return(function(y, oob) NULL)
  else if (type %in% c("nominal2", "nominal", "classification"))
    return(function(y, oob) mean(y[oob] != Mode(y[oob])))
  else if (type == "ordinal")
    return(function(y, oob) {
      y <- as.factor(y)
      mean(y[oob] != Mode(y[oob]))
      })
  else if (type == "regression")
    return(function(y, oob) mean((y[oob] - mean(y[oob]))^2))
}


## varIDs returns the variable ID of the variables that are used for splitting
## in a tree.
## For trees in cforest, based on varIDs in party
varIDs <- function(tree) 
{
  # different treatment when tree is based on randomForest-object
  
  ## --------------------------------------
  ## if(is.null(tree$nodeInfo)){     # for old tree object
  ## --------------------------------------
    
  if(is.null(tree$forest)){
    v <- c()
    foo <- function(node) {
      if (node[[4]]) 
        return(NULL)
      v <<- c(v, node[[5]][[1]])
      foo(node[[8]])
      foo(node[[9]])
    }
    foo(tree)
    return(v)
  }
  else {

    ## --------------------------------------
    ## return(tree$nodeInfo[,3])   # for old tree object
    ## --------------------------------------
    
    return(tree$forest$bestvar[,1])
  }
}


## getCutPoints returns:
## - vector of cutpoints (length=number of cutpoints) 
##   if variable is continuous
## - vector of indicators (length=number of categories x number of cutpoints)
##   if variable is categorical (nominal or ordered)
## For trees in cforest, based on cutpoint_list() in party
getCutPoints <- function(tree, variableID) 
{
  # different treatment when tree is based on randomForest-object
  
  ## --------------------------------------
  ## if(is.null(tree$nodeInfo)){     # for old tree object
  ## --------------------------------------
  
  if(is.null(tree$forest)){          # tree grown by the party-package 
    cutp <- function(node) {
      if (node[[4]]) return(NULL)
      cp <- NULL
      if (node[[5]][[1]] == variableID)
        cp <- node[[5]][[3]]
      nl <- cutp(node[[8]])
      nr <- cutp(node[[9]])
      return(c(cp, nl, nr))
    }
    return(cutp(tree))
  }
  else {                            # tree grown by the randomForest-package
    # get the cutPoints
    
    ## --------------------------------------
    ## cutPoints <- tree$nodeInfo[tree$nodeInfo[,3] == variableID, 4]     # for old tree object
    ## --------------------------------------
    
    cutPoints <- tree$forest$xbestsplit[tree$forest$bestvar == variableID]
    
    # Check if variable is categorical
    
    ## --------------------------------------
    ## if(tree$nrCatPerVar[variableID] > 1) {        # for old tree object
    ##  cutPoints <- getCatLeft(cutPoints, tree$nrCatPerVar[variableID])
    ## }     
    ## --------------------------------------
    
    nCat <- tree$forest$ncat[variableID]
    if(nCat > 1) {
      cutPoints <- getCatLeft(cutPoints, nCat)
    }
    return(cutPoints)
  }
}

## create_cond_list function
## partly based on party, but different functionality
## create the list of variables to condition on:
create_cond_list <- function(binnedVars, threshold, input, varsInTree, asParty)
{
   if (threshold >= 0 & threshold <= 1) {
      if(length(varsInTree) <= 1) cond_list <- list(integer(0))
      else {
         if(asParty){
            ctrl <- party::ctree_control(teststat = "quad", testtype = "Univariate", stump = TRUE)
            xnames <- colnames(input)
            cond_list <- lapply(varsInTree, function(varID){
               otherIDs <- varsInTree[varsInTree != varID]
               ct <- party::ctree(stats::as.formula(paste(xnames[varID], "~", 
                                                          paste(xnames[otherIDs], 
                                                                collapse = "+"), 
                                            collapse = "")), 
                           data = input, controls = ctrl)
               crit <- ct@tree$criterion[[2]]
               crit[which(is.na(crit))] <- 0
               varsToCondOn <- otherIDs[crit > threshold]
               return(varsToCondOn)
            })
         }
         else {
            nVars <- length(varsInTree)
            testStats <- matrix(0, ncol = nVars, nrow = nVars, 
                                dimnames = list(varsInTree, varsInTree))
            dfs <- testStats
            
            for(k in seq_len(nVars)){
               l <- k + 1
               while(l <= nVars){
                  tableBinnedVars <- table(binnedVars[c(k,l)])
                  testStats[k, l] <- quickChisqTest(tableBinnedVars)
                  dfs[k, l] <- (nrow(tableBinnedVars) - 1L) * 
                     (ncol(tableBinnedVars) - 1L)
                  l <- l + 1
               }
            }
            testStats <- testStats + t(testStats)
            dfs <- dfs + t(dfs)
            pValues <- pchisq(testStats, dfs, lower.tail = FALSE)
            
            cond_list <- lapply(seq_along(varsInTree), function(IDnr) {
               varsToCondOn <- varsInTree[(1 - pValues[,IDnr]) > threshold]
               varsToCondOn <- varsToCondOn[order(varsToCondOn)]
               return(varsToCondOn)
            })  
      }
      
      }
      names(cond_list) <- varsInTree
      return(cond_list)
   }
   else stop("The threshold should be a value between 0 and 1.")
}

## conditional_perm function
## BASED ON PARTY, but different functionality
## function that permutes the values of a specific variable in a ctree conditionally 
## on (a set of) the other variables in the ctree according to the partitions based 
## on the splitpoints in the tree

conditional_perm <- function(varID, varsToCondOn, binnedVars, oob, asParty) 
{
   ## same results as party
   if(asParty){
      CondPartitions <- interaction(binnedVars[as.character(varsToCondOn)], drop = TRUE, sep = "")
      levels(CondPartitions) <- 1:nlevels(CondPartitions)
      parts <- listParts(CondPartitions, oob)
   } 
   else {
      CondPartitions <- fastInteraction(binnedVars[as.character(varsToCondOn)])
      levels(CondPartitions) <- 1:nlevels(CondPartitions)
      ## not exactly the same results as party
      ## make partitions (including variable of interest)
      AllPartitions <- fastInteraction(list(CondPartitions, binnedVars[names(binnedVars) == varID][[1]]))
      
      ## check whether conditional permutation is useful
      if(nlevels(CondPartitions) == nlevels(AllPartitions)) return(NULL)
      
      ## select only the unique partitions. Permuting the partitions
      ## that are the same after including the variable of interest 
      ## cannot change the prediction, and therefore is redundant.
      PartsWithout <- listParts(CondPartitions, oob)
      PartsWith <- listParts(AllPartitions, oob)
      parts <- GetUniqueParts(PartsWithout, PartsWith)
   }
   perm <- (seq_along(oob))
   for(part in parts){
      if (length(part) > 1)
         perm[part] <- .resample(part)
   }
   return(perm[oob])
} 


## function that categorizes the OOB values of a predictor using the
## splitpoints in a tree
binVar <- function(varID, tree, x){
   cl <- getCutPoints(tree, varID)
   xclass <- class(x)[1]
   if (xclass == "integer") xclass <- "numeric"
   
   block <- switch(xclass, "numeric" = factor(cut(x, breaks = c(-Inf, sort(unique(cl)), Inf))),
                   "ordered" = factor(cut(as.numeric(x), breaks =  c(-Inf, sort(unique(cl)), Inf))),
                   "factor" = {
                      CL <- matrix(as.logical(cl), nrow = nlevels(x))                            
                      rs <- rowSums(CL)
                      dlev <- (seq_len(nrow(CL)))[rs %in% rs[duplicated(rs)]]
                      fuse <- c()
                      for (ii in dlev) {
                         for (j in dlev[dlev > ii]) {
                            if (all(CL[ii,] == CL[j,])) fuse <- rbind(fuse, c(ii, j))
                         }
                      }
                      xlev <- 1:nlevels(x)
                      newl <- nlevels(x) + 1
                      block <- as.integer(x)
                      for (l in xlev) {
                         if (NROW(fuse) == 0) break
                         if (any(fuse[, 1] == l)) {
                            f <- c(l, fuse[fuse[, 1] == l, 2])
                            fuse <- fuse[!fuse[,1] %in% f, , drop = FALSE]
                            block[block %in% f] <- newl
                            newl <- newl + 1
                         }
                      }
                      as.factor(block)
                   })
   levels(block) <- seq_len(nlevels(block))
   return(block)
}


## function that categorizes the predictors using the splitpoints in the 
## tree, and collects them in a list
makeBinnedVars <- function(varsInTree, tree, oob, input){
   out <- lapply(varsInTree, function(varInTree){
      oobVar <- input[oob, varInTree]
      binVar(varID = varInTree, tree, x = oobVar)
   })
   names(out) <- varsInTree
   return(out)
}


## function that makes a list of sets of observations per partition
listParts <- function(partitions, oob){
   OOB <- which(oob)
   return(lapply(levels(partitions), function(part) {
      OOB[!is.na(match(partitions, part))]}))
}


## Function that returns the observations (grouped per partition) that are not 
## in the same set of observations across two lists of partitioned observations
## Simple set operation (lessPartsSets 'without' allPartsSets).
GetUniqueParts <- function(PartsWithout, allParts){
   return(setdiff(PartsWithout, allParts))
}

