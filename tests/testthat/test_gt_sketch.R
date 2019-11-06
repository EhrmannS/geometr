library(testthat)
library(checkmate)
library(raster)
context("gt_sketch")


test_that("sketch a point geometry", {
  output <- gt_sketch(template = gtRasters$categorical, shape = "point")

  expect_class(x = output, classes = "geom")
  expect_true(object = output@type == "point")
})

test_that("sketch a point geometry from a matrix", {
  output <- gt_sketch(template = raster::as.matrix(gtRasters$categorical), shape = "point")

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

test_that("sketch several polygon geometries", {
  output <- gt_sketch(template = gtRasters$categorical, shape = "polygon", features = 2, vertices = 5)

  expect_class(x = output, classes = "geom")
})