library(checkmate)
library(testthat)
context("gs_polygon")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70))

  output <- gs_polygon(anchor = coords)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 5, ncols = 3)
})

test_that("casting to 'polygon' works", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)

  # from point to polygon
  input <- gs_point(anchor = coords)
  output <- gs_polygon(anchor = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 5, ncols = 3)

  # from line to polygon
  input <- gs_line(anchor = coords)
  output <- gs_polygon(anchor = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 5, ncols = 3)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70))
  input <- gs_point(anchor = coords)

  expect_error(gs_polygon(window = coords), regexp = "please provide anchor values.")
  expect_error(gs_polygon(anchor = "bla"))
  expect_error(gs_polygon(anchor = input))
  expect_error(gs_polygon(anchor = coords, vertices = "bla"))
  expect_error(gs_polygon(anchor = coords, regular = "bla"))
  expect_error(gs_polygon(anchor = coords, vertices = 4, regular = "bla"))
  expect_error(gs_polygon(vertices = 4, regular = TRUE))

  # function stops when trying to cast from line/point with less than 3 vertices

})

