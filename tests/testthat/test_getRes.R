library(checkmate)
library(testthat)
library(raster)
context("getRes")


test_that("getRes of a grid geom", {

  output <- getRes(gtGeoms$grid$categorical)
  expect_numeric(x = output, len = 2, any.missing = FALSE)
})

test_that("getRes of a Raster*", {
  # RasterLayer
  input <- gc_raster(gtGeoms$grid$categorical)
  output <- getRes(input)
  expect_numeric(x = output, len = 2, any.missing = FALSE)

  # RasterStack
  output <- getRes(raster::stack(list(gc_raster(gtGeoms$grid$categorical),
                                      gc_raster(gtGeoms$grid$continuous))))
  expect_numeric(x = output, len = 2, any.missing = FALSE)
})

test_that("getRes of a matrix", {
  input <- gc_raster(gtGeoms$grid$categorical)
  output <- getRes(raster::as.matrix(input))
  expect_numeric(x = output, len = 2, any.missing = FALSE)
})

test_that("getRes of any other object", {
  output <- getRes("bla")
  expect_null(object = output)
})
