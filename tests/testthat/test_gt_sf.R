library(checkmate)
library(testthat)
context("gt_sf")


test_that("transform from geom to sf", {
  output <- gt_sf(input = gtGeoms$point)
  expect_class(output, "sfc")
  expect_list(x = output, types = "numeric", len = 3)

  output <- gt_sf(input = gtGeoms$line)
  expect_class(output, "sfc")
  expect_list(x = output, len = 3)

  output <- gt_sf(input = gtGeoms$polygon)
  expect_class(output, "sfc")
  expect_list(x = output, len = 2)
})

test_that("transform from sf to geom", {
  # test POINT
  input <- gtSF$point

  output <- gt_sf(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 2, ncols = 3)

  # test MULTIPOINT
  input <- gtSF$multipoint

  output <- gt_sf(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 8, ncols = 3)

  # test LINESTRING
  input <- gtSF$linestring

  output <- gt_sf(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 8, ncols = 3)

  # test MULTILINESTRING
  input <- gtSF$multilinestring

  output <- gt_sf(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 12, ncols = 3)

  # test POLYGON
  input <- gtSF$polygon

  output <- gt_sf(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 15, ncols = 3)

  # test MULTIPOLYGON
  input <- gtSF$multipolygon

  output <- gt_sf(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 25, ncols = 3)
})

test_that("Error if arguments have wrong value", {
  #   notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
  #                          y = c(15, 25, 20, 40, 45))
  #
  #   expect_error(gt_as_sp(geom = "bla"))
  #   expect_error(gt_as_sp(geom = notAGeom))
})
