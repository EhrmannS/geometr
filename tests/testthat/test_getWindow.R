library(checkmate)
library(testthat)
library(raster)
library(spatstat)
context("getWindow")


test_that("getWindow of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 2, 1, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_point(anchor = coords, window = window)
  output <- getWindow(aGeom)

  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getWindow of a Spatial* object", {
  input <- gtSP$SpatialPoints

  output <- getWindow(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getWindow of a Raster", {
  aRaster <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

  output <- getWindow(aRaster)
  expect_tibble(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getWindow of a matrix", {
  aMatrix <- matrix(ncol = 100, nrow = 100, data = 5)

  output <- getWindow(aMatrix)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getWindow of an sf object", {
  input <- gtSF$point

  output <- getWindow(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getWindow of a ppp object", {
  x <- runif(20)
  y <- runif(20)
  input <- ppp(x, y, poly=list(x=c(0,10,0), y=c(0,0,10)))

  output <- getWindow(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 4, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))

  output <- getWindow(gtPPP)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getWindow of any other object", {
  output <- getWindow("bla")
  expect_null(object = output)
})
