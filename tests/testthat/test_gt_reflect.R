library(testthat)
library(checkmate)
context("gt_reflect")


test_that("output is valid geometry", {
  # geom with one feature
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_reflect(geom = input, angle = 30)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 5, ncols = 3)

  # reflect one out of two features
  coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                       y = c(40, 40, 60, 70, 10, 20, 40),
                       fid = c(1, 1, 1, 1, 2, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_reflect(geom = input, angle = -45, fid = 1)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)

  # reflect two out of two features
  coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                       y = c(40, 40, 60, 70, 10, 20, 40),
                       fid = c(1, 1, 1, 1, 2, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_reflect(geom = input, angle = list(30, -45))
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)
})

test_that("output has different coordinates than input", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_reflect(geom = input, angle = list(30, -45))

  expect_false(all(getPoints(input)[c(1, 2)] == getPoints(output)[c(1, 2)]))
})

test_that("Error if arguments have wrong value", {
  notinput <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)

  expect_error(gt_reflect(geom = "bla"))
  expect_error(gt_reflect(geom = input, angle = "bla"))
  expect_error(gt_reflect(geom = input, fid = "bla"))
  expect_error(gt_reflect(geom = notinput))
})