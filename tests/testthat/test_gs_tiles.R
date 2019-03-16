library(checkmate)
library(testthat)
context("gs_tiles")


test_that("output is valid geometry", {
  window <- data.frame(x = c(-40, 40),
                       y = c(-20, 20))

  output <- gs_tiles(window = window, cells = c(8, 4), crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")

  output <- gs_tiles(window = window, cells = c(8, 4), crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs", tiling = "hexagonal")
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")

  output <- gs_tiles(window = window, cells = c(8, 4), crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs", centroids = TRUE)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
})

test_that("output has the correct number of vertices and polygons", {
  window <- data.frame(x = c(-40, 40),
                       y = c(-20, 20))

  output <- gs_tiles(window = window, cells = c(8, 4), crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  expect_true(length(output@vert$fid) == 128)

  output <- gs_tiles(window = window, cells = c(8, 4), crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs", tiling = "hexagonal")
  expect_true(length(output@vert$fid) == 270)

  output <- gs_tiles(window = window, cells = c(8, 4), crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs", centroids = TRUE)
  expect_true(length(output@vert$fid) == 32)
})

test_that("Error if arguments have wrong value", {
  window <- data.frame(x = c(-40, 40),
                       y = c(-20, 20))
  wrongWindow <- data.frame(x = c(-180, 180),
                            y = c(-80, 80))

  expect_error(gs_tiles(window = "bla"))
  expect_error(gs_tiles(window = window))
  expect_error(gs_tiles(window = window, cells = "bla"))
  expect_error(gs_tiles(window = window, cells = c(1, 1)))
  expect_error(gs_tiles(window = wrongWindow, cells = c(8, 4)))
  expect_error(gs_tiles(window = window, cells = c(8, 4), tiling = "bla"))
  expect_error(gs_tiles(window = window, cells = c(8, 4), centroids = "bla"))
})

