library(testthat)
library(tempdisagg)

context("output functions")

m <- td(ts(c(1, 1, 1, 1)) ~ 1, to = 4, method = "fast")

test_that("plot works", {
  expect_null(plot(m))
})

test_that("print works", {
  expect_output(print(m))
})
