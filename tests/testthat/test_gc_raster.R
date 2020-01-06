library(checkmate)
library(testthat)
library(raster)
context("gc_raster")


test_that("transform geom to raster", {
  # test a single RasterLayer
  input <- gc_geom(input = gtRasters$categorical)

  output <- gc_raster(input = input)
  expect_class(x = output, classes = "RasterLayer")
  expect_data_frame(x = output@data@attributes[[1]], any.missing = FALSE, nrows = 9, ncols = 2)

  # test a RasterStack
  input <- gc_geom(input = gtRasters)

  output <- gc_raster(input = input)
  expect_class(x = output, classes = "RasterStack")
  expect_true(object = dim(output)[3] == 2)
  expect_data_frame(x = output[[1]]@data@attributes[[1]], any.missing = FALSE, nrows = 9, ncols = 2)
})

test_that("errors when transforming a geom not of type 'grid'", {

  expect_error(object = gc_raster(gtGeoms$polygon))
})