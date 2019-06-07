\name{VarImp}
\alias{VarImp}
\alias{as.VarImp}
\alias{as.VarImp.data.frame}
\alias{as.VarImp.matrix}
\alias{as.VarImp.numeric}
\alias{is.VarImp}
\title{ VarImp Objects }
\description{
    A class for random forest variable importance measures \code{\link{VarImp}} objects.
}
\usage{
as.VarImp(object, ...)

## S3 method for class 'data.frame'
\method{as.VarImp}{data.frame}(object, FUN = mean,
             type = c("Permutation", "Conditional Permutation", 
                      "Selection Frequency", "See Info"), 
             info = NULL, ...)

## S3 method for class 'matrix'
\method{as.VarImp}{matrix}(object, FUN = mean,
             type = c("Permutation", "Conditional Permutation", 
                      "Selection Frequency", "See Info"), 
             info = NULL, ...)
              
## S3 method for class 'numeric'
\method{as.VarImp}{numeric}(object, perTree = NULL, 
              type = c("Permutation", "Conditional Permutation", 
                       "Selection Frequency", "See Info"), 
              info = NULL, ...)
              
is.VarImp(VarImp)
}
\arguments{
  \item{object}{ an R object.}
  \item{perTree}{ a matrix or data frame of size \code{ntree x p} containing 
              the variable importance measures for each tree in the random forest.}
  \item{type}{ a character indicating the type of variable importance measure.}
  \item{info}{ a list with additional information about the variable 
              importance measure.}
  \item{FUN}{ a function to compute the variable importance. See section 'Details'.}
  \item{VarImp}{ an object of the class \code{\link{VarImp}}.}
  \item{\dots}{ additional arguments.}
}
\details{
  \code{as.VarImp} creates an object of class 'VarImp'. When \code{object} is a 
              \code{\link{matrix}} or a \code{\link{data.frame}}, the final values 
              are computed by applying \code{FUN} to its columns. \code{is.VarImp} 
              returns a logical indicating whether the evaluated object is of class
              'VarImp'.
}
\seealso{\code{\link{VarImp-methods}}
}
\examples{
  ## Matrix of fake importance measures per Tree  
  set.seed(290875)
  ntree <- 500
  p <- 15
  fakeVIM <- matrix(rnorm(ntree * p), nrow = ntree, ncol = p,
                    dimnames = list(paste0("pred", seq_len(ntree)), paste0("pred", seq_len(p))))
  is.VarImp(fakeVIM)
  
  ## make a 'VarImp' object
  fakeVarImp <- as.VarImp(fakeVIM, type = "See Info", 
                    info = list("The Vims are based on fake data.", 
                    "The mean was used to aggregate across the trees")) 
  is.VarImp(fakeVarImp)
}
\keyword{tree}
