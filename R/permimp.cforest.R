## permimp for cforest: permimp.cforest.R
permimp.RandomForest <- function (object, nperm = 1, OOB = TRUE, scaled = FALSE,
                                  conditional = FALSE, threshold = .95, whichxnames = NULL,   
                                  thresholdDiagnostics = FALSE, progressBar = TRUE, 
                                  pre1.0_0 = conditional, AUC = FALSE, asParty = FALSE, 
                                  mincriterion = 0, ...)
{
  # select input and responses (y)
  input <- object@data@get("input")
  y <- object@responses@variables[[1]]
  
  # does not work for multivariate outcomes
  if (length(object@responses@variables) != 1) 
    stop("cannot compute permutation importance measure for multivariate outcomes")
  
  
  # check weights
  w <- object@initweights
  if (max(abs(w - 1)) > sqrt(.Machine$double.eps))
    warning(sQuote("permimp"), " with non-unity weights might give misleading results",
            immediate. = TRUE)
  
  # if asParty == TRUE, set back to old default threshold value
  if (asParty && missing(threshold))
    threshold <- 0.2
  
  out <- doPermimp(object, input, 
                   inp = party::initVariableFrame(input, trafo = NULL),
                   y, OOB, threshold, conditional, 
                   whichxnames, ntree = length(object@ensemble), nperm, scaled,
                   progressBar, thresholdDiagnostics, 
                   w, AUC, pre1.0_0, mincriterion, asParty)
  
  return(out)
  
}