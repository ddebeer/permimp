# function to get splits out of a tree (tree-style)
getSplits <- function(bestVar, bestSplit, nCat)
{
  # for non-categorical variables
  splits <- cbind(leftcut = paste0("<", bestSplit),
                  rightcut = paste0(">", bestSplit))
  
  # for end nodes
  splits[bestVar == 0, ] <- ""
  
  # for categorical variables
  if(any(isCat <- (nCat > 1))){
    for(varID in which(isCat)){
      whichNodes <- which(bestVar == varID)      # for which nodes?
      catSeq <- seq_len(nCat[varID])             # seq of number of categories
      catLetters <- letters[catSeq]              # letters for category levels
      
      for(node in whichNodes){
        leftCat <- as.logical(intToBits(bestSplit[node]))[catSeq]
        splits[node, ] <- paste0(":", c(paste0(catLetters[leftCat], collapse = ""),
                                        paste0(catLetters[!leftCat], collapse = ""))
                          )
      }
    }
  }
  return(splits)
}


# function to get a tree-style tree out of a randomForest object
makeTree <- function(object, treeNr){
  nodeSeq <- seq_len(object$forest$ndbigtree[treeNr])
  bestVar <- object$forest$bestvar[nodeSeq, treeNr]
  nCat <- object$forest$ncat
  
  # get splits
  splits <- getSplits(bestVar, 
                      bestSplit = object$forest$xbestsplit[nodeSeq, treeNr],
                      nCat)
  
  frame <- data.frame(
    var = bestVar + 1,
    n = 1,
    dev = 1,
    yval = object$forest$nodepred[nodeSeq, treeNr],
    splits = splits,
    row.names = nodeSeq
  )
  
  xlevels <- object$forest$xlevels
  xlevels[names(xlevels)[nCat ==0]] <- NULL
  
  tree <- list(frame = frame)
  attr(tree, "xlevels") <- xlevels
  
  
  
  return(tree)
}
