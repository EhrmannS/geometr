library(checkmate)
library(testthat)
context("gc_sf")


test_that("transform from geom to sf", {
  output <- gc_sf(input = gtGeoms$point)
  expect_class(output, "sfc")
  expect_list(x = output, types = "numeric", len = 3)

  output <- gc_sf(input = gtGeoms$line)
  expect_class(output, "sfc")
  expect_list(x = output, len = 3)

  output <- gc_sf(input = gtGeoms$polygon)
  expect_class(output, "sfc")
  expect_list(x = output, len = 2)
})

test_that("Error if arguments have wrong value", {
  #   notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
  #                          y = c(15, 25, 20, 40, 45))
  #
  #   expect_error(gt_as_sp(geom = "bla"))
  #   expect_error(gt_as_sp(geom = notAGeom))
})
