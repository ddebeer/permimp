context("GetUniqueParts")
library(permimp)

test_that("GetUniqueParts selects the correct partitions", {
  partsA <- list(1:3, 4:10)
  partsB <- list(1:3, 4:6, 7:10)
  partsC <- list(1:3, 4:6, 7:8, 9:10)
  partsD <- list(1, 2:3, 4:6, 7:8, 9:10)
  
  expect_equal(GetUniqueParts(PartsWithout = partsA, allParts = partsB), 
               list(4:10))
  expect_equal(GetUniqueParts(PartsWithout = partsA, allParts = partsC), 
               list(4:10))
  expect_equal(GetUniqueParts(PartsWithout = partsA, allParts = partsD), 
               list(1:3, 4:10))
  
  expect_equal(GetUniqueParts(PartsWithout = partsB, allParts = partsC), 
               list(7:10))
  expect_equal(GetUniqueParts(PartsWithout = partsB, allParts = partsD), 
               list(1:3, 7:10))
  
  expect_equal(GetUniqueParts(PartsWithout = partsC, allParts = partsD), 
               list(1:3))
})
