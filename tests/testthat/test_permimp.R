context("CPI for cforest and randomForest")
library(party)
library(randomForest)
library(survival)
library(testthat)
library(permimp)




### set seed
set.seed(542863)

### get example data
airq <- subset(airquality, !(is.na(Ozone) | is.na(Solar.R)))

### fit very small forest
cfAirq5 <- cforest(Ozone ~ ., data = airq,
                    control = cforest_unbiased(mtry = 3, ntree = 5,
                                               minbucket = 5, 
                                               minsplit = 10))
rfAirq5 <- randomForest(Ozone ~ ., data = airq, maxnodes = 5,
                        mtry = 3, ntree = 5, importance = TRUE, 
                        keep.forest = TRUE, keep.inbag = TRUE)


permimp_cf <- permimp(cfAirq5, conditional = TRUE, progressBar = FALSE)
permimp_rf <- permimp(rfAirq5, conditional = TRUE, do_check = FALSE, 
                      whichxnames = c("Solar.R", "Wind", "Temp", "Month"),
                      progressBar = FALSE)


### Varimp object
test_that("permimp result is a VarImp object", {
  expect_output(str(permimp_cf), "List of 4")
  expect_is(permimp_cf, "VarImp")
  expect_identical(is.VarImp(permimp_cf), TRUE)
})


### ranks function
test_that("ranks works properly", {
  expect_is(ranks(permimp_cf, note = FALSE), "numeric")
  expect_identical(ranks(permimp_cf, note = FALSE), ranks(permimp_cf$values, note = FALSE))
})


### permimp asParty == varimp
test_that("permimp asParty is equal to varimp", {
  ### get CPI using permip
  set.seed(542863)
  permimp_asParty <- permimp(cfAirq5, conditional = TRUE, asParty = TRUE,
                             progressBar = FALSE)
  
  ### get CPI using varimp
  set.seed(542863)
  varimp <- varimp(cfAirq5, conditional = TRUE)
  
  ### test if equal
  expect_equal(varimp, permimp_asParty$values)
  expect_equal(varimp, print(permimp_asParty))
  
})


### selection frequency
test_that("selection frequency works", {
  
  selFreq_cf <- selFreq(cfAirq5)
  expect_output(str(selFreq_cf ), "List of 4")
  expect_is(subset(selFreq_cf, 1:4), "VarImp")
  expect_identical(is.VarImp(selFreq_cf ), TRUE)
  expect_is(ranks(selFreq_cf ), "numeric")
  
  selFreq_cf <- selFreq(cfAirq5, c("Temp", "Month", "Day"))
  expect_is(selFreq_cf , "VarImp")
  
  expect_error(selFreq(rfAirq5))
  
  expect_error(selFreq(cfAirq5, "5"))
  
})


### test plots
test_that("plot gives no errors", {
  expect_silent(plot(permimp_cf, type = "box", interval = "quantile"))
  expect_message(plot(permimp_cf, type = "dot", interval = "sd"))
  expect_message(plot(permimp_cf, horizontal = TRUE, interval = "sd"))
  expect_silent(plot(permimp_cf, type = "rank", 
                      horizontal = TRUE, interval = "quantile"))
})


### errors, warnings and messages for permimp
test_that("permimp returns errors and warnings", {
  expect_error(permimp(2), "The permimp functions only works for random forest objects from the party- and randomForest-packages.")
  expect_warning(permimp(cfAirq5, conditional = TRUE, progressBar = FALSE, 
                         scaled = TRUE,
                         thresholdDiagnostics = TRUE))
  
  expect_warning(permimp(
    randomForest(as.factor(Month) ~ ., data = airq, maxnodes = 5,
                 mtry = 3, ntree = 3, importance = TRUE,
                 classwt = c(1:5), keep.forest = TRUE, keep.inbag = TRUE),
    do_check = FALSE, 
    progressBar = FALSE))
  
  expect_warning(permimp(
    randomForest(factor(Ozone < 31) ~ ., data = airq[,-5], maxnodes = 5, 
                 strata = factor(airq$Month), sampsize = rep(5, 5),
                 mtry = 3, ntree = 3, importance = TRUE,
                 keep.forest = TRUE, keep.inbag = TRUE),
    do_check = FALSE, 
    progressBar = FALSE))
  
  expect_error(permimp(
    randomForest(factor(Ozone < 31) ~ ., data = airq[,-5], maxnodes = 5, 
                 mtry = 3, ntree = 3, importance = TRUE),
    do_check = FALSE, 
    progressBar = FALSE))
})

### unconditional permimp
test_that("unconditional permimp is the same as unconditional varimp", {
  ### get CPI using permip
  set.seed(542863)
  permimp <- permimp(cfAirq5, 
                     progressBar = FALSE)
  
  ### get CPI using varimp
  set.seed(542863)
  varimp <- varimp(cfAirq5)
  expect_equal(varimp, permimp$values)
  expect_equal(varimp, print(permimp))
  
  ### get CPI using permimp
  permimp <- permimp(
    cforest(factor(Ozone > 31) ~ ., data = airq,
            control = cforest_unbiased(mtry = 3, ntree = 5,
                                       minbucket = 5, 
                                       minsplit = 10)), 
    AUC = TRUE, pre1.0_0 = TRUE, 
    progressBar = FALSE)
  expect_is(permimp, "VarImp")
  
  
  ### Survivial data 
  airq <- subset(airquality, !(is.na(Ozone) | is.na(Solar.R)))
  airq$test <- with(airq, Surv(Ozone, floor((Wind - 1) / 10)))
  
  set.seed(54283)
  cf_surv <- cforest(test ~ ., data = airq,
                     control = cforest_unbiased(mtry = 3, ntree = 5,
                                                minbucket = 5, 
                                                minsplit = 10))
  
  
  set.seed(54283)
  varimp_s <- varimp(cf_surv)
  
  set.seed(54283)
  varimp_s <- varimp(cf_surv)
  
  set.seed(54283)
  permimp_s <- permimp(cf_surv, 
                       progressBar = FALSE)

  
  set.seed(542863)
  varimp_sc <- varimp(cf_surv, conditional = TRUE)
  set.seed(542863)
  permimp_sc <- permimp(cf_surv, conditional = TRUE, asParty = TRUE, 
                        progressBar = FALSE)


  expect_equal(varimp_s, permimp_s$values)
  

  expect_equal(varimp_sc, permimp_sc$values)
  
})

