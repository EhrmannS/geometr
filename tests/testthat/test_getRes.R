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
  output <- getRes(gtRasters$categorical)
  expect_numeric(x = output, len = 2, any.missing = FALSE)

  # RasterStack
  output <- getRes(gtRasters)
  expect_numeric(x = output, len = 2, any.missing = FALSE)
})

test_that("getRes of a matrix", {
  output <- getRes(raster::as.matrix(gtRasters$categorical))
  expect_numeric(x = output, len = 2, any.missing = FALSE)
})

test_that("getRes of any other object", {
  output <- getRes("bla")
  expect_null(object = output)
})
