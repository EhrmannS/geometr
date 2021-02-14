library(checkmate)
library(testthat)
library(raster)
library(spatstat)
context("getFeatures")


test_that("getFeatures of a 'geom'", {
  # test non-grid geom
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)
  output <- getFeatures(aGeom)

  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))

  # test grid geom
  # ... several layers
  input <- gc_geom(gtRasters)
  output <- getFeatures(input)
  expect_list(x = output, any.missing = FALSE, len = 2)
  expect_names(x = names(output), permutation.of = c("categorical", "continuous"))

  # ... layer with attribute table
  input <- gc_geom(gtRasters$categorical)
  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 3360)
  expect_names(x = names(output), permutation.of = c("fid", "gid", "values"))

  # ... layer without attribute table
  input <- gc_geom(gtRasters$continuous)
  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 3360)
  expect_names(x = names(output), permutation.of = c("fid", "gid", "values"))
})

test_that("getFeatures of a Spatial* object", {
  input <- gtSP$SpatialPolygons

  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))
})

test_that("getFeatures of a sf object", {
  input <- gtSF$polygon

  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 3)
  expect_names(names(output), identical.to = c("fid", "gid", "a"))
})

test_that("getFeatures of a ppp object", {
  x <- runif(20)
  y <- runif(20)
  m <- sample(1:2, 20, replace=TRUE)
  m <- factor(m, levels=1:2)
  input <- ppp(x, y, c(0,1), c(0,1), marks=m)

  output <- getFeatures(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 20, ncols = 3)
  expect_names(names(output), identical.to = c("fid", "gid", "values"))
})

test_that("getFeatures of any other object", {
  output <- getFeatures("bla")
  expect_null(object = output)
})

