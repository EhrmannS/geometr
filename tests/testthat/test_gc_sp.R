library(checkmate)
library(testthat)
context("gc_sp")


test_that("transform from geom to sp", {
  spPoints <- gc_sp(input = gtGeoms$point)
  expect_class(spPoints, "SpatialPoints")

  spPolygon <- gc_sp(input = gtGeoms$line)
  expect_class(spPolygon, "SpatialLines")

  spPolygon <- gc_sp(input = gtGeoms$polygon)
  expect_class(spPolygon, "SpatialPolygons")
})

test_that("output has correct length", {
  spPolygon <- gc_sp(input = gtGeoms$point)
  expect_equal(length(spPolygon), 12)
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))

  expect_error(gc_sp(input = "bla"))
  expect_error(gc_sp(input = notAGeom))
})