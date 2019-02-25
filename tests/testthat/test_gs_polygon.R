library(checkmate)
library(testthat)
context("gs_polygon")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_polygon(anchor = coords, window = window)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
})

test_that("template instead of anchor", {
  # input <- rtRasters$continuous
  #
  # output <- gs_point(template = input, vertices = 5, show = TRUE, new = FALSE, col = "deeppink")
  # expect_class(output, classes = "geom")
  # expect_true(output@type == "point")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70),
                       fid = c(1))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_polygon(anchor = coords, window = window, regular = TRUE, vertices = 6)
  expect_true(length(output@coords$fid) == 6)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70),
                       fid = c(1))

  expect_error(gs_polygon(anchor = "bla"))
  expect_error(gs_polygon(anchor = coords, window = "bla"))
  expect_error(gs_polygon(anchor = coords, vertices = "bla"))
  expect_error(gs_polygon(anchor = coords, regular = "bla"))
  expect_error(gs_polygon(anchor = coords, vertices = 4, regular = "bla"))
  expect_error(gs_polygon(vertices = 4, regular = TRUE))
  expect_error(gs_polygon(template = "bla", vertices = 4))
})

