context("Fast implementations")
library(permimp)

A <- factor(sample(1:3, size = 50, replace = TRUE))
B <- factor(sample(1:5, size = 50, replace = TRUE))


### fastInteraction
test_that("fastInteraction gives same results as interaction", {
  interaction <- droplevels(interaction(list(A, B), lex.order = TRUE))
  levels(interaction) <- seq_len(nlevels(interaction)) 
  
  expect_equal(fastInteraction(list(A, B)), interaction)
})


### quickChisqTest
test_that("quickChisqTest gives same results as chisq.test", {
  table <- table(A, B)
  expect_warning(expect_equal(quickChisqTest(table), unname(chisq.test(table)$statistic)))
})


### test countSplits 
test_that("countSplits", {
  expect_equal(permimp:::countSplits(B, 1:5), as.vector(table(B)))
  expect_equal(permimp:::countSplits(A, c(3, 2)), as.vector(table(A)[c(3, 2)]))
})


### test Mode
test_that("Mode works", {
  expect_equal(permimp:::Mode(factor(c(1, 1, 2, 3))), "1")
  expect_equal(permimp:::Mode(factor(c(1, 2, 2, 3))), "2")
  expect_equal(permimp:::Mode(factor(c(1, 2, 3, 3))), "3")
  expect_equal(permimp:::Mode(factor(c(1, 2, 3))), "1")
})

