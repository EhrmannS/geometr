library(checkmate)
library(testthat)
library(raster)
context("setWindow")


test_that("setWindow based on data.frame", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords)

  output <- setWindow(input, window)
  expect_class(output, "geom")
  expect_data_frame(getWindow(output))
  expect_equal(dim(getWindow(output)),  c(4, 2))
})

test_that("setWindow based on Extent", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  window <- extent(window)
  input <- gs_polygon(anchor = coords)

  output <- setWindow(input, window)
  expect_class(output, "geom")
  expect_data_frame(getWindow(output))
  expect_equal(dim(getWindow(output)),  c(4, 2))
})

test_that("setWindow based on bbox", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- st_bbox(gtSF$linestring)
  input <- gs_polygon(anchor = coords)

  output <- setWindow(input, window)
  expect_class(output, "geom")
  expect_data_frame(getWindow(output))
  expect_equal(dim(getWindow(output)),  c(4, 2))
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  input <- gs_polygon(anchor = coords)

  expect_error(setWindow(x = input, to = "bla"))
})

