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

test_that("output has the correct number of vertices and polygons", {
  # window <- data.frame(x = c(-40, 40),
  #                      y = c(-20, 20))
  #
  # output <- gs_tiles(window = window, cells = c(8, 4))
  # expect_true(length(output@vert$fid) == 128)
  #
  # output <- gs_tiles(window = window, cells = c(8, 4), tiling = "hexagonal")
  # expect_true(length(output@vert$fid) == 270)
  #
  # output <- gs_tiles(window = window, cells = c(8, 4), centroids = TRUE)
  # expect_true(length(output@vert$fid) == 32)
})

test_that("Error if arguments have wrong value", {
  aWindow <- data.frame(x = c(-180, 180),
                        y = c(-60, 80))

  expect_error(gs_tiles(window = "bla"))
  # expect_error(gs_tiles(window = window))
  # expect_error(gs_tiles(window = window, cells = "bla"))
  # expect_error(gs_tiles(window = window, cells = c(1, 1)))
  # expect_error(gs_tiles(window = wrongWindow, cells = c(8, 4)))
  # expect_error(gs_tiles(window = window, cells = c(8, 4), tiling = "bla"))
  # expect_error(gs_tiles(window = window, cells = c(8, 4), centroids = "bla"))
})

