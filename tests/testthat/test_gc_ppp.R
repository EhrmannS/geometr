library(checkmate)
library(testthat)
context("gc_geom")


test_that("transform from ppp to geom", {
  ppp <- gc_ppp(input = gtGeoms$point)
  expect_class(ppp, "ppp")
})
