library(checkmate)
library(testthat)
context("getType")


test_that("getType of a 'geom'", {
  output <- getType(x = gtGeoms$polygon)

  expect_character(x = output, len = 2)
  expect_true(all(output %in% c("polygon", "polygon")))
})

test_that("getType of a Spatial* object", {
  output <- getType(x = gtSP$SpatialPolygons)

  expect_character(x = output, len = 2)
  expect_true(all(output %in% c("polygon", "SpatialPolygons")))
})

test_that("getType of an sf object", {
  output <- getType(x = gtSF$polygon)

  expect_character(x = output, len = 2)
  expect_true(all(output %in% c("polygon", "POLYGON")))
})

test_that("getType of a RasterLayer", {
  output <- getType(x = gtRasters$continuous)

  expect_character(x = output, len = 2)
  expect_true(all(output %in% c("grid", "RasterLayer")))
})

test_that("getType of any other object", {
  output <- getType("bla")
  expect_null(object = output)
})