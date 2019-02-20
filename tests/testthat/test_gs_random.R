library(checkmate)
library(testthat)
context("gs_random")


test_that("output is valid geometry", {
  output <- gs_random(type = "polygon", vertices = 4)
  expect_class(output, classes = "geom")
})

test_that("template instead of anchor", {
  # input <- matrix(nrow = 100, ncol = 100, data = 0)
  #
  # output <- gs_random(template = input, vertices = 5)
  # expect_class(output, classes = "geom")
})

test_that("output has the correct number of vertices", {
  output <- gs_random(type = "polygon", vertices = 4)
  expect_data_frame(output@coords, any.missing = FALSE, nrows = 4, ncols = 4)
})

test_that("Error if arguments have wrong value", {
  input <- matrix(nrow = 100, ncol = 100, data = 0)

  expect_error(gs_random(type = "bla"))
  expect_error(gs_random(template = "bla"))
  expect_error(gs_random(template = input, vertices = "bla"))
  expect_error(gs_random(template = input, show = "bla"))
})
