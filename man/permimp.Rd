\name{permimp}
\alias{permimp}
\alias{permimp.randomForest}
\alias{permimp.RandomForest}
\title{ Random Forest Permutation Importance for random forests}
\description{
    Standard and partial/conditional permutation importance for 
    random forest-objects fit using the \pkg{party} or \pkg{randomForest} 
    packages, following the permutation principle of the `mean decrease in 
    accuracy' importance in \pkg{randomForest} . The partial/conditional permutation
    importance is implemented differently, selecting the predictions to condition 
    on in each tree using Pearson Chi-squared tests applied to the 
    by-split point-categorized predictors. In general the new implementation has
    similar results as the original \code{\link{varimp}} function. With 
    \code{asParty = TRUE}, the partial/conditional permutation importance is 
    fully backward-compatible but faster than the original \code{\link{varimp}}
    function in \pkg{party}.
}
\usage{ 
permimp(object, ...)
\method{permimp}{randomForest}(object, nperm = 1, OOB = TRUE, scaled = FALSE,
     conditional = FALSE, threshold = .95, whichxnames = NULL,   
     thresholdDiagnostics = FALSE, progressBar = TRUE,  do_check = TRUE, ...)
\method{permimp}{RandomForest}(object, nperm = 1, OOB = TRUE, scaled = FALSE,
     conditional = FALSE, threshold = .95, whichxnames = NULL,   
     thresholdDiagnostics = FALSE, progressBar = TRUE, 
     pre1.0_0 = conditional, AUC = FALSE, asParty = FALSE, mincriterion = 0, ...)
}
\arguments{
  \item{object}{ an object as returned by \code{cforest} or \code{randomForest}.}
  \item{mincriterion}{ the value of the test statistic or 1 - p-value that
                       must be exceeded in order to include a split in the 
                       computation of the importance. The default 
                       \code{mincriterion = 0} guarantees that all splits 
                       are included.}
  \item{conditional}{ a logical that determines whether unconditional or conditional 
                      permutation is performed. }
  \item{threshold}{ the threshold value for (1 - p-value) of the association 
                    between the predictor of interest and another predictor, which 
                    must be exceeded in order to include the other predictor in 
                    the conditioning scheme for the predictor of interest (only
                    relevant if \code{conditional = TRUE}). A threshold value of 
                    zero includes all other predictors.}
  \item{nperm}{ the number of permutations performed.}
  \item{OOB}{ a logical that determines whether the importance is computed from 
              the out-of-bag sample or the learning sample (not suggested).}
  \item{pre1.0_0}{ Prior to \pkg{party} version 1.0-0, the actual data values
                   were permuted according to the original permutation
                   importance suggested by Breiman (2001). Now the assignments
                   to child nodes of splits in the variable of interest
                   are permuted as described by Hapfelmeier et al. (2012),
                   which allows for missing values in the predictors and is
                   more efficient with respect to memory consumption and 
                   computing time. This method does not apply to the conditional
                   permutation importance, nor to random forests that were not
                   fit using the \pkg{party} package.} 
  \item{scaled}{ a logical that determines whether the differences in prediction
                 accuracy should be scaled by the total (null-model) error.}
  \item{AUC}{ a logical that determines whether the Area Under the Curve (AUC)
              instead of the accuracy is used to compute the permutation 
			  importance (cf. Janitza et al., 2012). The AUC-based permutation 
			  importance is more robust towards class imbalance, but it is 
			  only applicable to binary classification.}
  \item{asParty}{ a logical that determines whether or not exactly the same 
                  values as the original \code{\link{varimp}} function in 
				  \pkg{party} should be obtained.}
  \item{whichxnames}{ a character vector containing the predictor variable 
                      names for which the permutation importance should be 
					  computed. Only use when aware of the implications, see 
					  section 'Details'.}  
  \item{thresholdDiagnostics}{ a logical that specifies whether diagnostics 
                               with respect to the threshold-value should be 
							   prompted as warnings.}
  \item{progressBar}{ a logical that determines whether a progress bar should 
                      be displayed.}
  \item{do_check}{ a logical that determines whether a check requiring user 
                   input should be included.}                    
  \item{...}{ additional arguments to be passed to the Methods}
}
\details{

  Function \code{permimp} is highly comparable to \code{\link{varimp}} in \pkg{party},
  but the partial/conditional variable importance has a different, more efficient
  implementation. Compared to the original \code{\link{varimp}} in \pkg{party}, 
  \code{permimp} applies a different strategy to select the predictors to condition 
  on (ADD REFERENCE TO PAPER). 
  
  With \code{asParty = TRUE}, permimp returns exactly the same values as 
  \code{\link{varimp}} in \pkg{party}, but the computation is done more efficiently.
  
  If \code{conditional = TRUE}, the importance of each variable is computed by
  permuting within a grid defined by the predictors that are associated  
  (with 1 - p-value greater than \code{threshold}) to the variable of interest. 
  The \code{threshold} can be interpreted as a parameter that moves the permutation 
  importance across a dimension from fully conditional (\code{threshold = 0}) to
  completely unconditional (\code{threshold = 1}), see Debeer and Strobl (2020). 
  
  Using the \code{wichxnames} argument, the computation of the permutation importance
  can be limited to a smaller number of specified predictors. Note, however, that when
  \code{conditional = TRUE}, the (other) predictors to condition on are also 
  limited to this selection of predictors. Only use when fully aware of the 
  implications.
  
  For further details, please refer to the documentation of \code{\link{varimp}}.
}
\value{
    An object of class \code{\link{VarImp}}, with the mean decrease in accuracy 
    as its \code{$values}. 
}
\references{ 

    Leo Breiman (2001). Random Forests. \emph{Machine Learning}, 45(1), 5--32.

    Alexander Hapfelmeier, Torsten Hothorn, Kurt Ulm, and Carolin Strobl (2012).
    A New Variable Importance Measure for Random Forests with Missing Data.
    \emph{Statistics and Computing}, \url{https://link.springer.com/article/10.1007/s11222-012-9349-1}

    Torsten Hothorn, Kurt Hornik, and Achim Zeileis (2006b). Unbiased
    Recursive Partitioning: A Conditional Inference Framework.
    \emph{Journal of Computational and Graphical Statistics}, \bold{15} (3),
    651-674.  Preprint available from 
    \url{https://www.zeileis.org/papers/Hothorn+Hornik+Zeileis-2006.pdf}

    Silke Janitza, Carolin Strobl and Anne-Laure Boulesteix (2013). An AUC-based
    Permutation Variable Importance Measure for Random Forests. 
    BMC Bioinformatics.2013, \bold{14} 119.
    \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-119}

    Carolin Strobl, Anne-Laure Boulesteix, Thomas Kneib, Thomas Augustin, and 
    Achim Zeileis (2008). Conditional Variable Importance for Random Forests. 
    \emph{BMC Bioinformatics}, \bold{9}, 307. 
    \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-9-307}
    
    Debeer Dries and Carolin Strobl (2020). Conditional Permutation Importance 
    Revisited. \emph{BMC Bioinformatics}, \bold{21}, 307. 
    \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-020-03622-2}
}
\seealso{\code{\link{varimp}}, \code{\link{VarImp}}
}
\examples{
  
  ### for RandomForest-objects, by party::cforest()  
  set.seed(290875)
  readingSkills.cf <- party::cforest(score ~ ., data = party::readingSkills, 
                              control = party::cforest_unbiased(mtry = 2, ntree = 25))
  
  ### conditional importance, may take a while...
  # party implementation:
  set.seed(290875)
  party::varimp(readingSkills.cf, conditional = TRUE)
  # faster implementation but same results
  set.seed(290875)
  permimp(readingSkills.cf, conditional = TRUE, asParty = TRUE)
  
  # different implementation with similar results
  set.seed(290875)
  permimp(readingSkills.cf, conditional = TRUE, asParty = FALSE)
  
  ### standard (unconditional) importance is unchanged
  set.seed(290875)
  party::varimp(readingSkills.cf)
  set.seed(290875)
  permimp(readingSkills.cf)
  
  
  ###
  set.seed(290875)
  readingSkills.rf <- randomForest::randomForest(score ~ ., data = party::readingSkills, 
                              mtry = 2, ntree = 25, importance = TRUE, 
                              keep.forest = TRUE, keep.inbag = TRUE)
                              
    
  ### (unconditional) Permutation Importance
  set.seed(290875)
  permimp(readingSkills.rf, do_check = FALSE)
  
  # very close to
  readingSkills.rf$importance[,1]
  
  ### Conditional Permutation Importance
  set.seed(290875)
  permimp(readingSkills.rf, conditional = TRUE, threshold = .8, do_check = FALSE)
                              
  
}
\keyword{tree}
