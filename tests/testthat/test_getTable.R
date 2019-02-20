library(checkmate)
library(testthat)
library(raster)
context("getTable")


test_that("getTable of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)
  output <- getTable(aGeom)

  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "n"))
})

test_that("getTable of a Spatial* object", {
  input <- gtSP$SpatialPolygons

  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "n"))
})

test_that("getTable of an sf object", {
  input <- gtSF$polygon

  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 3)
  expect_names(names(output), identical.to = c("fid", "n", "a"))
})
