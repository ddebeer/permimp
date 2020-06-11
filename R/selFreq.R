## selection frequency: counts hom many times each predictor 
## variable was selected for splitting
selFreq <- function(object, whichxnames = NULL)
{
   stopifnot(all(class(object) == "RandomForest", isS4(object)))
   input <- object@data@get("input")
   xnames <- colnames(input)
   if(is.null(whichxnames)) {
      whichxnames <- xnames
      whichVarIDs <- seq_along(xnames)
   }
   else {
      whichVarIDs <- match(whichxnames, table = xnames)
      if(all(is.na(whichVarIDs))) stop("Error: whichxnames is not a subset of the predictor variable names in the forest.")
      whichVarIDs <- whichVarIDs[order(whichVarIDs)]
   } 
   
   perTree <- t(vapply(object@ensemble, function(tree){
      Splitvars <- getSplitVars(tree)
      countSplits(Splitvars, whichVarIDs)
   }, FUN.VALUE = vector("integer", length(whichVarIDs))))
   
   colnames(perTree) <- whichxnames
   out <- as.VarImp(perTree, 
                    FUN = mean,
                    type = "Selection Frequency",
                    info = NULL)
   return(out)
}

