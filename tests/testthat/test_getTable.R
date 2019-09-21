library(checkmate)
library(testthat)
library(raster)
library(spatstat)
context("getTable")


test_that("getTable of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)
  output <- getTable(aGeom)

  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))
})

test_that("getTable of a Spatial* object", {
  input <- gtSP$SpatialPolygons

  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "gid"))
})

test_that("getTable of an sf object", {
  input <- gtSF$polygon

  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 3)
  expect_names(names(output), identical.to = c("fid", "gid", "a"))
})

test_that("getTable of a ppp object", {
  x <- runif(20)
  y <- runif(20)
  m <- sample(1:2, 20, replace=TRUE)
  m <- factor(m, levels=1:2)
  input <- ppp(x, y, c(0,1), c(0,1), marks=m)

  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 20, ncols = 3)
  expect_names(names(output), identical.to = c("fid", "gid", "value"))
})

test_that("getTable returns an empty tibble when no attributes are given", {
  input <- gtRasters$continuous

  # test RasterLayer without attribute table
  output <- getTable(input)
  expect_tibble(output, any.missing = FALSE, nrows = 91, ncols = 2)
})

test_that("getTable returns a given attribute table", {
  input <- gtRasters$categorical

  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 9, ncols = 2)
  expect_names(names(output), identical.to = c("fid", "cover"))
})

test_that("getTable of any other object", {
  output <- getTable("bla")
  expect_null(object = output)
})

