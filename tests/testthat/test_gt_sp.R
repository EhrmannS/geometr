library(checkmate)
library(testthat)
context("gt_sp")


test_that("output has class Spatial*", {
  spPoints <- gt_sp(input = gtGeoms$point)
  expect_class(spPoints, "SpatialPoints")

  spPolygon <- gt_sp(input = gtGeoms$line)
  expect_class(spPolygon, "SpatialLines")

  spPolygon <- gt_sp(input = gtGeoms$polygon)
  expect_class(spPolygon, "SpatialPolygons")
})

test_that("output has correct length", {
  spPolygon <- gt_sp(input = gtGeoms$point)
  expect_equal(length(spPolygon), 12)
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))

  expect_error(gt_sp(input = "bla"))
  expect_error(gt_sp(input = notAGeom))
})


test_that("something", {

  # test 'SpatialPoints'
  input <- gtSP$SpatialPoints

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialPointsDataFrame'
  input <- SpatialPointsDataFrame(input, data.frame(a = 1:4), match.ID = TRUE)

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialMultiPoints'
  input <- gtSP$SpatialMultiPoints

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  expect_true(length(unique(output@vert$fid)) == 8)

  # test 'SpatialMultiPointsDataFrame'
  input <- SpatialMultiPointsDataFrame(input, data = data.frame(a = 1:2))

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  expect_data_frame(getTable(output), nrows = 8, ncols = 3)

  # test 'SpatialLines'
  input <- gtSP$SpatialLines

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_true(length(unique(output@vert$fid)) == 2)

  # test 'SpatialLinesDataFrame'
  input <- SpatialLinesDataFrame(input, data = data.frame(a = 1:2), match.ID = FALSE)

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_data_frame(getTable(output), nrows = 2, ncols = 3)

  # test 'SpatialPolygons'
  input = gtSP$SpatialPolygons

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialPolygonsDataFrame'
  input <- SpatialPolygonsDataFrame(input, data = data.frame(a = 1:2), match.ID = FALSE)

  output <- gt_sp(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(getTable(output), nrows = 2, ncols = 3)

  # test 'SpatialGrid'

  # output <- gt_sp(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")

  # test 'SpatialGridDataFrame'

  # output <- gt_sp(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")

  # test 'SpatialPixels'

  # output <- gt_sp(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")

  # test 'SpatialPixelsDataFrame'

  # output <- gt_sp(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")

})