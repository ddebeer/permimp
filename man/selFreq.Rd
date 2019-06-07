\name{selFreq}
\alias{selFreq}
\title{ Predictor Selection Frequency in Random Forests }
\description{
    counts hom many times each predictor variable was selected for splitting
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

  Function \code{varimpAsParty} does exactly the same as \code{\link{varimp}} in \pkg{party},
  but the partial/conditional variable importance has a more efficient implementation. 
  Compared to the orignal \code{\link{varimp}} in \pkg{party}, \code{varimpNew2}
  applies a different strategy to select the predictors to condition on (ADD
  REFERENCE TO PAPER). In addition using the \code{wichxnames} argument, the
  computation can be selected to a limited number of predictor variables. When
  \code{wichxnames} is not \code{NULL}, the predictor variables to condition on,
  are also limited to this selection.

  If \code{conditional = TRUE}, the importance of each variable is computed by permuting 
  within a grid defined by the covariates that are associated  (with 1 - p-value 
  greater than \code{threshold}) to the variable of interest.
  The resulting variable importance score is partial/conditional in the sense of beta
  coefficients in regression models, but represents the effect of a variable in both main 
  effects and interactions. See Strobl et al. (2008) for details.

  For further deatils, please refer to the documentation of \code{\link{varimp}}.
}
\value{
  An object of class \code{\link{VarImp}}, with as \code{$values} the sum of the sum of 
  the selection frequencies across the trees. 
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
