library(checkmate)
library(testthat)
context("gs_voronoi")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_point(anchor = coords, window = window)

  # create voronoi from data.frame
  output <- gs_voronoi(anchor = coords, window = window)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@feature, any.missing = FALSE, nrows = 4, ncols = 2)

  # create voronoi from geom
  output <- gs_voronoi(anchor = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@feature, any.missing = FALSE, nrows = 4, ncols = 2)
})


