\name{ranks}
\alias{ranks}
\alias{ranks.default}
\alias{ranks.VarImp}
\title{ Reversed Rankings }
\description{
    Method for giving the reversed rankings of the numerical values of a vector or VarImp object.
}
\usage{ 
   ranks(x, note = TRUE, ...)
\method{ranks}{default}(x, note = TRUE, ...)
\method{ranks}{VarImp}(x, note = TRUE, ...)
}
\arguments{
  \item{x}{ an object to be reverse ranked.}
  \item{note}{ a logical specifying whether the (reversed) rankings should be 
               printed instead of the importance values.}
  \item{\dots}{ additional arguments to be passed to \code{\link{rank}}.}
}

\details{
  The \code{ranks} function is nothing more than 
  \code{(length(x) - rank(x, ...) + 1L)}. But it also works for objects of class
  \code{\link{VarImp}}.
}
\value{
  A vector containing the reversed rankings.
}
\examples{
  ## High Jump data
  HighJumps <- c(Anna = 1.45, Betty = 1.53, Cara = 1.37, Debby = 1.61, 
                 Emma = 1.29, Hanna = 1.44, Juno = 1.71)
  HighJumps
  ## ranking of high jump data
  ranks(HighJumps)
}
