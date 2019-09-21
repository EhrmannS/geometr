library(checkmate)
library(testthat)
library(raster)
library(sp)
context("getExtent")


test_that("getExtent of a geom", {
  input <- gtGeoms$point

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

test_that("getExtent of a ppp object", {

  output <- getExtent(gtPPP)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of a Raster", {
  aRaster <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

  output <- getExtent(aRaster)
  expect_tibble(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of a matrix", {
  aMatrix <- matrix(ncol = 100, nrow = 100, data = 5)

  output <- getExtent(aMatrix)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of any other object", {
  output <- getExtent("bla")
  expect_null(object = output)
})

