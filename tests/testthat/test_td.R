library(testthat)
library(tempdisagg)
library(tsbox)

context("td")

test_that("td works with various to specifications", {
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "quarterly", method = "fast"), "td")
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "quarter", method = "fast"), "td")
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "monthly", method = "fast"), "td")
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "month", method = "fast"), "td")
})

