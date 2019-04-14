library(checkmate)
library(testthat)
context("gs_tiles")


test_that("output is valid geometry", {
  aWindow <- data.frame(x = c(-180, 180),
                        y = c(-60, 80))

  # squared tiles
  output <- gs_tiles(window = aWindow, width = 10)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  # expect_data_frame(output@vert, any.missing = FALSE, nrows = 2520, ncols = 4)

  # hexagonal tiles
  output <- gs_tiles(window = aWindow, width = 10, pattern = "hexagonal")
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 4403, ncols = 4)

  # only censtroids
  output <- gs_tiles(window = aWindow, width = 10, centroids = TRUE)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 504, ncols = 4)

  # rotated tiles
  output <- gs_tiles(window = aWindow, width = 10, rotation = 45)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 2905, ncols = 4)
})

test_that("Error if arguments have wrong value", {
  aWindow <- data.frame(x = c(-180, 180),
                        y = c(-60, 80))

  expect_error(gs_tiles(anchor = "bla"))
  expect_error(gs_tiles(window = "bla"))
  expect_error(gs_tiles(window = aWindow, width = "bla"))
  expect_error(gs_tiles(window = window, width = 10, pattern = "bla"))
  expect_error(gs_tiles(window = window, width = 10, pattern = "triangular"))
  expect_error(gs_tiles(window = window,  width = 10, rotation = "bla"))
  expect_error(gs_tiles(window = window,  width = 10, centroid = "bla"))
})

