library(testthat)
library(checkmate)
context("gs_point")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_point(anchor = coords, window = window)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 2, ncols = 3)
})

test_that("casting to 'point' works", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70))

  # from polygon to point
  input <- gs_polygon(anchor = coords)
  output <- gs_point(anchor = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 4, ncols = 3)

  # from line to point
  input <- gs_line(anchor = coords)
  output <- gs_point(anchor = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 4, ncols = 3)
})

