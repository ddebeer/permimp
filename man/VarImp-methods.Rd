\name{VarImp-Methods}
\alias{VarImp-methods}
\alias{plot.VarImp}
\alias{print.VarImp}
\alias{subset.VarImp}
\title{ Methods for VarImp Objects }
\description{
    Methods for computing on \code{\link{VarImp}} objects..
}
\usage{
\method{plot}{VarImp}(x, nVar = length(x$values), type = c("bar", "box", "dot", "rank"),
                      sort = TRUE, interval = c( "no", "quantile", "sd"), 
                      intervalProbs = c(.25, .75), intervalColor = NULL, 
                      horizontal = FALSE, col = NULL, pch = NULL, 
                      main = NULL, margin = NULL, ...)
\method{print}{VarImp}(x, ranks = FALSE, ...)
\method{subset}{VarImp}(x, subset, ...)
}
\arguments{
  \item{x}{ an object of the class \code{\link{VarImp}}.}
  \item{nVar}{ an integer specifying the number of predictor variables that should be 
               included in the plot. The \code{nVar} predictor variables with the 
               highest variable importance measure are retained.}
  \item{type}{ a character string that indicates the type of plot. Must be one of the
               following: "bar", "box", "dot" or "rank" (see Details). }
  \item{sort}{ a logical that specifies whether the predictors should be ranked according 
               to the importance measures.}
  \item{interval}{ a character string that indicates if, and which type of intervals
                   should be added to the plot. Must be one of the following: \code{"no"}, 
                   \code{"quantile"}, or \code{"sd"} (see Details).}
  \item{intervalProbs}{ a numerical vector of the form c(bottom, top), specifying 
                        the two quantiles that should be used for the interval. 
                        Only meaningful when \code{interval = "quantile"}.}
  \item{intervalColor}{ a color code or name, see \code{\link{par}}. }
  \item{horizontal}{ a logical that specifies whether the plot should be horizontal 
                     (= importance values on the x-axis. The default is \code{FALSE}. }
  \item{col}{ a color code or name, see \code{\link{par}}. }
  \item{pch}{ Either a single character or an integer code specifying the plotting 
              'character', see \code{\link{par}}. }
  \item{main}{ an overall title for the plot: see \code{\link{title}}.}
  \item{margin}{ a numerical vector of the form \code{c(bottom, left, top, right)}, 
                 which gives the number of lines of margin to be specified on the 
                 four sides of the plot. See \code{\link{par}}.} 
  \item{ranks}{ a logical specifying whether the (reversed) rankings should be 
                printed instead of the importance values.}
  \item{subset}{ a character, integer or logical vector, specifying the subset of 
                  predictor variables.}		  
  \item{\dots}{ additional arguments.}
}
\details{

  \code{plot} gives visualization of the variable importance values. \code{print} prints the importance values, or their (reversed) rankings if \code{ranks = TRUE}. \code{ranks} returns the reversed rankings of the variable importance values. The \code{subset} method for VarImp objects returns a VarImp object for only a subset of the original predictors in the random forest.
  
  In \code{plot}, the \code{type = "bar"} results in a barplot, \code{type = "dot"} in a point-plot, \code{type = "rank"} in a point-plot with the importance rankings as the plotting 'characters', see \code{\link{ranks}}. In each of these three options an interval (based on either two quantiles or on the standard deviation of the \code{perTree} values) can be added to the plot. \code{type = "box"} results in boxplots, and is only meaningful when \code{perTree} values are available.
}
\seealso{\code{\link{VarImp}}
}
\examples{
  ## Fit a random forest (using cforest)   
  set.seed(290875)
  readingSkills.cf <- party::cforest(score ~ ., data = party::readingSkills, 
                              control = party::cforest_unbiased(mtry = 2, ntree = 50))
  
  ## compute permutation variable importance:
  set.seed(290875)
  permVIM <- permimp(readingSkills.cf)
  
  ## print the variable importance values
  permVIM
  print(permVIM, ranks = TRUE)
  ranks(permVIM)
  
  ## Visualize the variable importance values
  plot(permVIM, type = "bar", margin = c(6,3,3,1))
  plot(permVIM, nVar = 2, type = "box", horizontal = TRUE)
  
  ## note the rankings
  plot(subset(permVIM, c("age", "nativeSpeaker")), intervalColor = "pink")
  plot(subset(permVIM, c("shoeSize", "nativeSpeaker")), intervalColor = "pink")
}
\keyword{tree}
