## permimp for randomForest: permimp.randomForest.R
permimp.randomForest <- function (object, nperm = 1, OOB = TRUE, scaled = FALSE,
                                  conditional = FALSE, threshold = .95, whichxnames = NULL,   
                                  thresholdDiagnostics = FALSE, progressBar = TRUE, 
                                  do_check = TRUE, ...)
{
  # get randomForest call
  rfCall <- match.call(randomForest, object$call, expand.dots = TRUE)
  
  # make sure all required information is available.
  if(any(is.null(rfCall$keep.forest), is.null(rfCall$keep.inbag), 
     rfCall$keep.forest == FALSE, rfCall$keep.inbag == FALSE)) {
    if(do_check){
      if(menu(c("Yes", "No"), title = paste0("The ", sQuote("permimp"), " computations require forest and inbag information. \n", "A new random forest needs to be fit with ", sQuote("keep.forest = TRUE"), " and ", sQuote("keep.inbag = TRUE"), ". \n", "Due to random variation, the new and original forest are not identical. \n \n", "Should a new random forest be fitted?")) != 1)
        stop(sQuote("permimp"), " computations canceled.")
    } else {
      warning(sQuote("permimp"), " requires forest and inbag information. \n", "A new random forest is fit with ", sQuote("keep.forest = TRUE"), " and ", sQuote("keep.inbag = TRUE"), ". \n", "Due to random variation, the new and original forest are not identical.",
              immediate. = TRUE)}
    rfCall$keep.forest <- rfCall$keep.inbag <- TRUE
    object <- eval(rfCall)
    rfCall <- match.call(randomForest, object$call, expand.dots = TRUE)
  } 
  else {
    if(do_check) {
      if(menu(c("Yes", "No"), title = paste0("The ", sQuote("permimp"), " computations depend on the data-objects that were used to fit the random Forest. \n \n", "Are you sure that the training data have NOT been changed since fitting?")) != 1)
      stop(sQuote("permimp"), " computations canceled.")}
  }
  
  # check if formula was used
  if(inherits(object, "randomForest.formula")){
    
    # select input and responses (y) - using the randomForest.fromula-call
    dfCall <- rfCall
    dfCall[[1L]] <- quote(stats::model.frame)
    dfCall <- dfCall[c(1, match(c("formula", "data", "subset", "weights", "na.action", 
                                  "offset"), names(dfCall), 0L))]
    
    # make a model frame
    mf <- eval(dfCall, parent.frame())   # model frame  
    y <- model.response(mf)       # extract response
    mt <- attr(mf, "terms")              # model terms
    attr(mt, "intercept") <- 0           # intercept should not be included
    input <- model.frame(terms(reformulate(attributes(mt)$term.labels)), data.frame(mf))
    rm("mf", "mt")
  
  } 
  else {
    
    # select input and responses (y) - using the randomForest-call
    y <- eval(rfCall$y, parent.frame())       # extract response
    input <- eval(rfCall$x, parent.frame())   # extract input
    
  }
  # weights etc. are not included in the current computation
  if(object$type == "classification" & !is.null(rfCall$classwt)) 
    warning(sQuote("permimp"), " does not take ", sQuote("classwt"), " into account during permuation and prediction. \n", "All observations are automatically weighed equally.",
            immediate. = TRUE)
  
  if(object$type == "classification" & !is.null(rfCall$cutoff)) 
    warning(sQuote("permimp"), " does not take ", sQuote("cutoff"), " into account during prediction. \n", "The default cut-off is automatically applied.",
            immediate. = TRUE)
  
  if(!is.null(rfCall$sampsize) & length(eval(rfCall$sampsize, parent.frame())) > 1) 
    warning(sQuote("permimp"), " is based on the OOB values, using stratification to sample the IB values \n", "may have an undesired impact.",
            immediate. = TRUE)
  
  out <- doPermimp(object, input, inp = NULL, y, OOB, threshold, conditional, 
                   whichxnames, ntree = object$ntree, nperm, scaled,
                   progressBar, thresholdDiagnostics, 
                   w = NULL, AUC = FALSE, 
                   pre1.0_0 = TRUE, mincriterion = NULL, asParty = FALSE)

  return(out)
  
}

