library(checkmate)
library(testthat)
context("gs_rectangle")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_rectangle(anchor = coords, extent = extent)
  expect_class(output, classes = "geom")
})

test_that("template instead of anchor", {
  # input <- rtRasters$continuous
  #
  # output <- gs_point(template = input, vertices = 5, show = TRUE, new = FALSE, col = "deeppink")
  # expect_class(output, classes = "geom")
  # expect_true(output@type == "point")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- gs_rectangle(anchor = coords, extent = extent)
  expect_data_frame(output@coords, any.missing = FALSE, nrows = 4, ncols = 4)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  expect_error(gs_rectangle(anchor = "bla"))
  expect_error(gs_rectangle(anchor = coords, window = "bla"))
  expect_error(gs_rectangle(anchor = coords, features = "bla"))
  expect_error(gs_rectangle(template = "bla"))
})
