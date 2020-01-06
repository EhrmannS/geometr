library(checkmate)
library(testthat)
library(raster)
context("getRes")


test_that("getRes of a grid geom", {
  output <- getRes(gtGeoms$grid$categorical)
  expect_data_frame(x = output, any.missing = FALSE, types = "double", nrows = 1, ncols = 2)
})

test_that("getRes of a Raster*", {
  # RasterLayer
  output <- getRes(gtRasters$categorical)
  expect_data_frame(x = output, any.missing = FALSE, types = "double", nrows = 1, ncols = 2)

  # RasterStack
  output <- getRes(gtRasters)
  expect_data_frame(x = output, any.missing = FALSE, types = "double", nrows = 1, ncols = 2)
})

test_that("getRes of a matrix", {
  output <- getRes(raster::as.matrix(gtRasters$categorical))
  expect_data_frame(x = output, any.missing = FALSE, types = "double", nrows = 1, ncols = 2)
})

test_that("getRes of any other object", {
  output <- getRes("bla")
  expect_null(object = output)
})
