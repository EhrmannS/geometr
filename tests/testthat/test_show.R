library(checkmate)
library(testthat)
context("show")


test_that("show works", {
  expect_class(x = gtTheme, classes = "gtTheme")
})
