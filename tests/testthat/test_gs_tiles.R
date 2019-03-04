library(checkmate)
library(testthat)
context("gs_tiles")


test_that("output is valid geometry", {
  window <- data.frame(x = c(-180, 180),
                       y = c(-80, 80))

  output <- gs_tiles(window = window, cells = c(8, 4), crs = projs$longlat)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")

  output <- gs_tiles(window = window, cells = c(8, 4), crs = projs$longlat, tiling = "hexagonal")
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")

  output <- gs_tiles(window = window, cells = c(8, 4), crs = projs$longlat, centroids = TRUE)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
})

test_that("output has the correct number of vertices and polygons", {
  window <- data.frame(x = c(-40, 40),
                       y = c(-20, 20))

  output <- gs_tiles(window = window, cells = c(8, 4), crs = projs$longlat)
  expect_true(length(output@coords$fid) == 128)

  output <- gs_tiles(window = window, cells = c(8, 4), crs = projs$longlat, tiling = "hexagonal")
  expect_true(length(output@coords$fid) == 270)

  output <- gs_tiles(window = window, cells = c(8, 4), crs = projs$longlat, centroids = TRUE)
  expect_true(length(output@coords$fid) == 32)
})

test_that("Error if arguments have wrong value", {
  window <- data.frame(x = c(-40, 40),
                       y = c(-20, 20))

  expect_error(gs_tiles(window = "bla"))
  expect_error(gs_tiles(window = window))
  expect_error(gs_tiles(window = window, cells = "bla"))
  expect_error(gs_tiles(window = window, cells = c(1, 1)))
  expect_error(gs_tiles(window = window, cells = c(8, 4), tiling = "bla"))
  expect_error(gs_tiles(window = window, cells = c(8, 4), centroids = "bla"))
})

