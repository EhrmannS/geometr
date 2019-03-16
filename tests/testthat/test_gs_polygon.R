library(checkmate)
library(testthat)
context("gs_polygon")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70))

  output <- gs_polygon(anchor = coords)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 4, ncols = 4)
})

test_that("template instead of anchor", {
  # input <- gtRasters$continuous
  #
  # output <- gs_polygon(template = input, vertices = 5, show = TRUE, col = "deeppink")
  # expect_class(output, classes = "geom")
  # expect_true(output@type == "polygon")
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

