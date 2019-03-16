library(checkmate)
library(testthat)
context("gt_as_sp")


test_that("output has class Spatial*", {
  spPoints <- gt_as_sp(geom = gtGeoms$point)
  expect_class(spPoints, "SpatialPoints")

  spPolygon <- gt_as_sp(geom = gtGeoms$line)
  expect_class(spPolygon, "SpatialLines")

  spPolygon <- gt_as_sp(geom = gtGeoms$polygon)
  expect_class(spPolygon, "SpatialPolygons")
})

test_that("output has correct length", {
  spPolygon <- gt_as_sp(geom = gtGeoms$point)
  expect_equal(length(spPolygon), 12)
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))

  expect_error(gt_as_sp(geom = "bla"))
  expect_error(gt_as_sp(geom = notAGeom))
})
