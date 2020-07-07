library(checkmate)
library(testthat)
library(raster)
context("gc_raster")


test_that("transform geom to raster", {
  # test a single RasterLayer
  output <- gc_raster(input = gtGeoms$grid$categorical)
  expect_class(x = output, classes = "RasterLayer")
  expect_data_frame(x = output@data@attributes[[1]], any.missing = FALSE, nrows = 9, ncols = 2)
})

test_that("errors when transforming a geom not of type 'grid'", {

  expect_error(object = gc_raster(gtGeoms$polygon))
})
