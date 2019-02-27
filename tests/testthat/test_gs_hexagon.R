library(checkmate)
library(testthat)
context("gs_hexagon")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_hexagon(anchor = coords, window = window)
  expect_class(output, classes = "geom")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_hexagon(anchor = coords, window = window)
  expect_data_frame(output@coords, any.missing = FALSE, nrows = 6, ncols = 4)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)

  expect_error(gs_hexagon(anchor = "bla"))
  expect_error(gs_hexagon(anchor = coords, window = "bla"))
  expect_error(gs_hexagon(anchor = coords, features = "bla"))
  expect_error(gs_hexagon(template = "bla"))
})
