library(checkmate)
library(testthat)
context("gc_ppp")


test_that("transform from ppp to geom", {
  ppp <- gc_ppp(input = gt_subset(obj = gtGeoms$point, fid == 1))
  expect_class(ppp, "ppp")
})
