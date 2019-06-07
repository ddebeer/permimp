context("fastInteraction")
library(permimp)

test_that("fastInteraction gives same results as interaction", {
  A <- factor(sample(1:3, size = 50, replace = TRUE))
  B <- factor(sample(1:5, size = 50, replace = TRUE))
  interaction <- droplevels(interaction(list(A, B), lex.order = TRUE))
  levels(interaction) <- seq_len(nlevels(interaction)) 
  
  expect_equal(fastInteraction(list(A, B)), interaction)
})
