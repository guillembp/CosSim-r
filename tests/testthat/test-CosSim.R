context("test-CosSim.R")

test_that("multiplication works", {
  expect_equal(CosSim(matrix(data = c(1,1,1,0,1,1,0,1), ncol = 2)), matrix(data = c(1,0.66666667,0.66666667,1), ncol = 2))
})
