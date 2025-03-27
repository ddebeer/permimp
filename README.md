<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/permimp)](https://CRAN.R-project.org/package=permimp)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ddebeer/permimp?branch=master&svg=true)](https://ci.appveyor.com/project/ddebeer/permimp)
[![Build Status](https://app.travis-ci.com/ddebeer/permimp.svg?branch=master)](https://app.travis-ci.com/ddebeer/permimp) [![codecov](https://codecov.io/gh/ddebeer/permimp/branch/master/graph/badge.svg)](https://codecov.io/gh/ddebeer/permimp)
[![](http://cranlogs.r-pkg.org/badges/grand-total/permimp?color=blue)](https://cran.r-project.org/package=permimp)
[![Codecov test coverage](https://codecov.io/gh/ddebeer/permimp/graph/badge.svg)](https://app.codecov.io/gh/ddebeer/permimp)
[![R-CMD-check](https://github.com/ddebeer/permimp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddebeer/permimp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

### (Conditional) Permutation Importance for Random Forests. 



This R-package computes the Conditional Permutation Importance (CPI; Strobl, 2008) 
using an alternative implementation that is both faster and more 
stable ([Debeer & Strobl 2020](https://rdcu.be/b5CrH)). The (C)PI can 
be computed for random forest fit using (a) the original impurity 
reduction method ( `randomForest`-package), and (b) using the Conditional 
Inference framework (`party`-package). In addition, a plotting method for 
the resulting `VarImp`-object is included.

For forests with large trees parallel processing may speed up the computations. 
Parallel processing is possible via the `cl` argument. Under the hood, the 
`pblapply` function from the [pbapply-package](https://peter.solymos.org/pbapply/). Tip: 
when using parallel processing set `progressBar = FALSE`. The additional communication
between the nodes for updating the progress bar will slow down the computations.


## Installation


The package can be installed using using the `devtools`-package:

```
install.packages("devtools")
devtools::install_github("ddebeer/permimp")
```


## Documentation

The workhorse is the `permimp`-function. 

```
?permimp
```

For documentation about the plotting function:

```
?plot.VarImp
```

## Example

```
library(party)
library(randomForest)
library(permimp)




### set seed
set.seed(542863)

### get example data
airq <- subset(airquality, !(is.na(Ozone) | is.na(Solar.R)))

### fit a random forest 
### ... using the party package
cfAirq5 <- cforest(Ozone ~ ., data = airq,
                    control = cforest_unbiased(mtry = 3, ntree = 1000,
                                               minbucket = 5, 
                                               minsplit = 10))
                                               
### compute the conditional permutation importance
permimp_cf <- permimp(cfAirq5, conditional = TRUE)
plot(permimp_cf, type = "box", interval = "quantile")


### fit a random forest ...
### ... using the randomForest package         
rfAirq5 <- randomForest(Ozone ~ ., data = airq, 
                        mtry = 3, ntree = 1000, importance = TRUE, 
                        keep.forest = TRUE, keep.inbag = TRUE)
                        
### compute the conditional permutation importance                      
permimp_rf <- permimp(rfAirq5, conditional = TRUE)
plot(permimp_rf, horizontal = TRUE)
```
