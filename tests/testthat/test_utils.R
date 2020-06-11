context("utils and other helper function")
library(party)
library(randomForest)
library(testthat)
library(permimp)

ntree <- 2
maxdepth <- 1


### make data and randomForest object
set.seed(542863)

### get example data
airq <- subset(airquality, !(is.na(Ozone) | is.na(Solar.R)))

### fit very small forest
cfAirq5 <- cforest(Ozone ~ ., data = airq,
                   control = cforest_unbiased(mtry = 3, ntree = ntree,
                                              maxdepth = maxdepth))
rfAirq5 <- randomForest(Ozone ~ ., data = airq, 
                        mtry = 3, ntree = ntree, importance = TRUE, 
                        keep.forest = TRUE, keep.inbag = TRUE)

cfAirq5_2 <- cforest(factor(Month)  ~ ., data = airq,
                   control = cforest_unbiased(mtry = 3, ntree = ntree,
                                              maxdepth = maxdepth))
rfAirq5_2 <- randomForest(factor(Month) ~ ., data = airq, 
                        mtry = 3, ntree = ntree, importance = TRUE, 
                        keep.forest = TRUE, keep.inbag = TRUE)

cfAirq5_3 <- cforest(ordered(Month)  ~ ., data = airq,
                     control = cforest_unbiased(mtry = 3, ntree = ntree,
                                                maxdepth = maxdepth))

cfAirq5_4 <- cforest(factor(Month < 6)  ~ ., data = airq,
                     control = cforest_unbiased(mtry = 3, ntree = ntree,
                                                maxdepth = maxdepth))


### test maxDepth
test_that("maxDepth works", {
  expect_equal(permimp:::getDepth(permimp:::getTree(cfAirq5, 1)), 1)
})


### test getOutcomeType
test_that("getOutcomeType works", {
  expect_equal(permimp:::getOutcomeType(cfAirq5), "regression")
  expect_equal(permimp:::getOutcomeType(rfAirq5), "regression")
  
  expect_equal(permimp:::getOutcomeType(cfAirq5_2), "nominal")
  expect_equal(permimp:::getOutcomeType(rfAirq5_2), "classification")
  
  expect_equal(permimp:::getOutcomeType(cfAirq5_3), "ordinal")
  
  expect_equal(permimp:::getOutcomeType(cfAirq5_4), "nominal2")
})


### test selectPred
test_that("selectPred works", {
  expect_is(permimp:::selectPred(cfAirq5, "regression"), "function")
  expect_is(permimp:::selectPred(rfAirq5, "regression"), "function")
})


