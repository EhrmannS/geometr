library(checkmate)
library(testthat)
context("getValue")


test_that("getValues of a 'geom'", {
  input <- gc_geom(input = gtRasters$continuous)
  output <- getValues(x = input)

  expect_integer(x = output, lower = 0, upper = 100, len = 3360)
})

test_that("getValues of a RasterLayer", {
  output <- getValues(x = gtRasters$continuous)

  expect_integer(x = output, lower = 0, upper = 100, len = 3360)
})

test_that("getValues of a matrix", {
  output <- getValues(x = raster::as.matrix(gtRasters$continuous))

  expect_integer(x = output, lower = 0, upper = 100, len = 3360)
})

test_that("getValues of any other object", {
  output <- getValues("bla")
  expect_null(object = output)
})