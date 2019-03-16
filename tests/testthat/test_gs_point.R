library(checkmate)
library(testthat)
context("gs_point")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_point(anchor = coords, window = window)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 2, ncols = 4)
})

test_that("template instead of anchor", {
  # input <- gtRasters$continuous
  #
  # output <- gs_point(template = input, vertices = 5, show = TRUE, col = "deeppink")
  # expect_class(output, classes = "geom")
  # expect_true(output@type == "point")
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)

  expect_error(gs_point(vertices = 4))
  expect_error(gs_point(anchor = "bla"))
  expect_error(gs_point(anchor = coords, window = "bla"))
  expect_error(gs_point(anchor = coords, vertices = "bla"))
  expect_error(gs_point(template = "bla", vertices = 4))
})
