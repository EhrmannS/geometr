library(checkmate)
library(testthat)
library(raster)
context("gc_terra")


test_that("transform geom to SpatRaster", {
  # test a single RasterLayer
  output <- gc_terra(input = gtGeoms$grid$categorical)
  expect_class(x = output, classes = "SpatRaster")
  expect_list(x = levels(output), any.missing = FALSE)
  expect_data_frame(x = levels(output)[[1]], nrows = 9, ncols = 2)
})

test_that("errors when transforming a geom not of type 'grid'", {

  expect_error(object = gc_terra(gtGeoms$polygon))
})
