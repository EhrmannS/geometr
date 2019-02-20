library(checkmate)
library(testthat)
library(raster)
library(sp)
context("getExtent")


test_that("getExtent of a geom", {
  input <- gtGeoms$locations

  output <- getExtent(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of a Spatial* object", {
  input <- gtSP$SpatialPoints

  output <- getExtent(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of an sf object", {
  input <- gtSF$point

  output <- getExtent(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

