\name{selFreq}
\alias{selFreq}
\title{ Predictor Selection Frequency in Random Forests }
\description{
    counts how many times each predictor variable was selected for splitting
    in a random forest. Only implemented for \code{\link{cforest}} form the \pkg{party}
    package.
}
\usage{ 
selFreq(object, whichxnames = NULL)
}
\arguments{
  \item{object}{ an object as returned by \code{cforest}.}
  \item{whichxnames}{ a character vector containing the predictor variable names that for 
                  which the permutation importance should be computed. See section 'Details'.} 
}
\details{

  Function \code{selFreq} counts how many times each predictor variable was 
  selected for splitting in a random forest. In the current implementation this 
  \code{selFreq} can only be applied to random forests as returned by 
  \code{cforest}.

}
\value{
  An object of class \code{\link{VarImp}}, with as \code{$values} the mean of
  the sum of the selection frequencies across the trees. 
}
\seealso{\code{\link{VarImp}},
}
\examples{
    
  set.seed(290875)
  readingSkills.cf <- party::cforest(score ~ ., data = party::readingSkills, 
                              control = party::cforest_unbiased(mtry = 2, ntree = 100))
  
  ## Selection Frequency
  selFreq(readingSkills.cf)

}
\keyword{tree}
