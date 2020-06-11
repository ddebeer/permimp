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


permimp_cf <- permimp(cfAirq5, conditional = TRUE)
permimp_rf <- permimp(rfAirq5, conditional = TRUE, do_check = FALSE, 
                      whichxnames = c("Solar.R", "Wind", "Temp", "Month"))


### Varimp object
test_that("permimp result is a VarImp object", {
  expect_output(str(permimp_cf), "List of 4")
  expect_is(permimp_cf, "VarImp")
  expect_identical(is.VarImp(permimp_cf), TRUE)
})


### ranks function
test_that("ranks works properly", {
  expect_is(ranks(permimp_cf), "numeric")
  expect_identical(ranks(permimp_cf), ranks(permimp_cf$values))
})


### permimp asParty == varimp
test_that("permimp asParty is equal to varimp", {
  ### get CPI using permip
  set.seed(542863)
  permimp_asParty <- permimp(cfAirq5, conditional = TRUE, asParty = TRUE)
  
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
    do_check = FALSE))
  
  expect_warning(permimp(
    randomForest(factor(Ozone < 31) ~ ., data = airq[,-5], maxnodes = 5, 
                 strata = factor(airq$Month), sampsize = rep(5, 5),
                 mtry = 3, ntree = 3, importance = TRUE,
                 keep.forest = TRUE, keep.inbag = TRUE),
    do_check = FALSE))
  
  expect_error(permimp(
    randomForest(factor(Ozone < 31) ~ ., data = airq[,-5], maxnodes = 5, 
                 mtry = 3, ntree = 3, importance = TRUE),
    do_check = FALSE))
})

### unconditional permimp
test_that("unconditional permimp is the same as unconditional varimp", {
  ### get CPI using permip
  set.seed(542863)
  permimp <- permimp(cfAirq5)
  
  ### get CPI using varimp
  set.seed(542863)
  varimp <- varimp(cfAirq5)
  expect_equal(varimp, permimp$values)
  expect_equal(varimp, print(permimp))
  
  ### get CPI using permip
  permimp <- permimp(
    cforest(factor(Ozone > 31) ~ ., data = airq,
            control = cforest_unbiased(mtry = 3, ntree = 2,
                                       minbucket = 5, 
                                       minsplit = 10)), 
    AUC = TRUE, pre1.0_0 = TRUE)
  expect_is(permimp, "VarImp")
  
  
  ### Survivial data 
  airq <- subset(airquality, !(is.na(Ozone) | is.na(Solar.R)))
  airq$test <- with(airq, Surv(Ozone, floor((Wind - 1) / 10)))
  
  cf_surv <- cforest(test ~ ., data = airq,
                     control = cforest_unbiased(mtry = 3, ntree = 2,
                                                minbucket = 5, 
                                                minsplit = 10))
  
  set.seed(1)
  varimp <- varimp(cf_surv)
  set.seed(1)
  # Awaiting change in party package (party:::.R_get_nodeID)
  #permimp <- permimp(cf_surv)
  #expect_equal(varimp, permimp$values)
  
  set.seed(1)
  varimp <- varimp(cf_surv, conditional = TRUE)
  set.seed(1)
  #permimp <- permimp(cf_surv, conditional = TRUE, asParty = TRUE)
  #expect_equal(varimp, permimp$values)
  
})

