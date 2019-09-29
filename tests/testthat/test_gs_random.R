library(checkmate)
library(testthat)
context("gs_random")


test_that("output is valid geometry", {
  # test 'point'
  output <- gs_random(type = "point")
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 1, ncols = 3)

  # test 'line'
  output <- gs_random(type = "line")
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 2, ncols = 3)

  # test 'polygon'
  output <- gs_random(type = "polygon")
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 4, ncols = 3)
})

test_that("Error if arguments have wrong value", {
  input <- matrix(nrow = 100, ncol = 100, data = 0)

  expect_error(gs_random(type = "bla"))
  expect_error(gs_random(type = "line", vertices = 1))
  expect_error(gs_random(type = "polygon", vertices = 2))
  expect_error(gs_random(sketch = input, vertices = "bla"))
})
