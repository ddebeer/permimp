## functions for predicting with only one tree in the RF.




# get predictions based on one tree in a randomForest-object
getPredsRF <- function(tree, newData)
{
  vapply(seq_len(dim(newData)[1]), FUN = function(obsNr){
    getPredRF(tree, newData[obsNr,])
  }, FUN.VALUE = double(1))
}

# recursive function for obtaining the prediction for one observation
getPredRF <- function(tree, obs, node = 1)
{
  # get info for the current node
  nodeInfo <- tree$nodeInfo[node,]
  
  # get variableID splitvar
  varID <- nodeInfo[3]
  
  # if node is endnode, return prediction
  if(varID == 0) return(nodeInfo[5])
  
  # if split variable is categorical
  if(tree$nrCatPerVar[varID] > 1){
    # if observation is one of the categories defined by the splitvalue,
    # than take left node
    catLeft <- getCatLeft(nodeInfo[4], tree$nrCatPerVar[varID])
    if(obs[varID] %in% tree$catLevels[[varID]][catLeft]) 
      getPredRF(tree, obs, node = nodeInfo[1]) 
    else
      getPredRF(tree, obs, node = nodeInfo[2])
  }
  else {
    # if observation is smaller than splitpoint, take left node
    if(obs[varID] <= nodeInfo[4]) 
      getPredRF(tree, obs, node = nodeInfo[1]) 
    else 
      getPredRF(tree, obs, node = nodeInfo[2])
  }
}



## --------------------------------------------------------------------------
## --------------------------------------------------------------------------




# function to go from the splitpoint-value for categorical predictors to
# an indicator vector, indicating "left daughter node for this category"
getCatLeft <- function(integer, nrCat)
{
  unlist(lapply(integer, FUN = function(x){
    as.logical(intToBits(x))[1:nrCat]
  }))
}

