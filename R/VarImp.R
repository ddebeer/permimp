## Object of class 'VarImp'


## as.VarImp
as.VarImp <- function(object, ...)
   UseMethod("as.VarImp")

as.VarImp.data.frame <- function(object, FUN = mean,
                             type = c("Permutation", "Conditional Permutation", "Selection Frequency", "See Info"), 
                             info = NULL, ...)
{
   match.fun(FUN)
   out <- list(values = apply(object, 2, FUN, ...),
               perTree = object,
               type = match.arg(type),
               info = info)
   class(out) <- "VarImp"
   return(out)
}

as.VarImp.matrix <- function(object, FUN = mean,
                                 type = c("Permutation", "Conditional Permutation", "Selection Frequency", "See Info"), 
                                 info = NULL, ...)
{
   object <- as.data.frame(object)
   as.VarImp(object, FUN, type, info, ...)
}

as.VarImp.numeric <- function(object, perTree = NULL,
                              type = c("Permutation", "Conditional Permutation", "Selection Frequency", "See Info"), 
                              info = NULL, ...)
{
   out <- list(values = object,
               perTree = perTree,
               type = match.arg(type),
               info = info)
   class(out) <- "VarImp"
   return(out)
}

## is.VarImp
is.VarImp <- function(VarImp)
{
   names <- all(c("values", "perTree", "type", "info") %in% names(VarImp))
   all(names, class(VarImp) == "VarImp")
}

