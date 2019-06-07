context("quickChisqTest")
library(permimp)

test_that("quickChisqTest gives same results as chisq.test", {
  A <- factor(sample(1:3, size = 50, replace = TRUE))
  B <- factor(sample(1:5, size = 50, replace = TRUE))
  table <- table(A, B)
  
  expect_equal(quickChisqTest(table), unname(chisq.test(table)$statistic))
})
