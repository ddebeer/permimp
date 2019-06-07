## ranks Method
ranks <- function(x, note = TRUE, ...) UseMethod("ranks")

## ranks.default
ranks.default <- function(x, note = TRUE, ...){
   out <- length(x) - rank(x, ...) + 1L
   names(out) <- names(x)
   if(note) cat("Reversed ranking: 1 denotes the highest value. \n ")
   return(out)
}

## ranks for class VarImp
ranks.VarImp <- function(x, note = TRUE, ...){
   return(ranks(x$values, note = note, ...))
} 