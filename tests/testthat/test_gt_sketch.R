library(testthat)
library(checkmate)
context("gt_sketch")


test_that("sketch a point geometry", {
  output <- gt_sketch(template = gtRasters$categorical, shape = "point")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "point")
})

test_that("sketch a line geometry", {
  output <- gt_sketch(template = gtRasters$categorical, shape = "line")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "line")
})

test_that("sketch a polygon geometry", {
  output <- gt_sketch(template = gtRasters$categorical, shape = "polygon")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "polygon")

  # test regular polygons
  output <- gt_sketch(template = gtRasters$categorical, shape = "polygon", regular = TRUE, fixed = FALSE)

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "polygon")
})