library(checkmate)
library(testthat)
library(sp)
context("gc_geom")


test_that("transform from sf to geom", {
  # test POINT
  input <- gtSF$point

  output <- gc_geom(input = input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 2, ncols = 3)

  # test MULTIPOINT
  input <- gtSF$multipoint

  output <- gc_geom(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 8, ncols = 3)

  # test LINESTRING
  input <- gtSF$linestring

  output <- gc_geom(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 8, ncols = 3)

  # test MULTILINESTRING
  input <- gtSF$multilinestring

  output <- gc_geom(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "line")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 12, ncols = 3)

  # test POLYGON
  input <- gtSF$polygon

  output <- gc_geom(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 15, ncols = 3)

  # test MULTIPOLYGON
  input <- gtSF$multipolygon

  output <- gc_geom(input)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 25, ncols = 3)
})

test_that("transform from sp to geom", {
  # test 'SpatialPoints'
  input <- gtSP$SpatialPoints

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialPointsDataFrame'
  input <- SpatialPointsDataFrame(input, data.frame(data = 1:4), match.ID = TRUE)

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialMultiPoints'
  input <- gtSP$SpatialMultiPoints

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  expect_true(length(unique(output@vert$fid)) == 8)

  # test 'SpatialMultiPointsDataFrame'
  input <- SpatialMultiPointsDataFrame(input, data = data.frame(data = 1:2))

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  expect_data_frame(getTable(output), nrows = 8, ncols = 3)

  # test 'SpatialLines'
  input <- gtSP$SpatialLines

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_true(length(unique(output@vert$fid)) == 2)

  # test 'SpatialLinesDataFrame'
  input <- SpatialLinesDataFrame(input, data = data.frame(data = 1:2), match.ID = FALSE)

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_data_frame(getTable(output), nrows = 2, ncols = 3)

  # test 'SpatialPolygons'
  input = gtSP$SpatialPolygons

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialPolygonsDataFrame'
  input <- SpatialPolygonsDataFrame(input, data = data.frame(data = 1:2), match.ID = FALSE)

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(getTable(output), nrows = 2, ncols = 3)

  # test 'SpatialGrid'
  x = GridTopology(c(0,0), c(1,1), c(5,5))
  input = SpatialGrid(grid = x)

  output <- gc_geom(input = input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialGridDataFrame'
  input <- SpatialGridDataFrame(grid = input, data = data.frame(data = letters[1:25]))

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialPixels'
  data(meuse.grid)
  pts = meuse.grid[c("x", "y")]
  input = SpatialPixels(SpatialPoints(pts))

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")

  # test 'SpatialPixelsDataFrame'
  input <- SpatialPixelsDataFrame(points = input, data = meuse.grid)

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
})

test_that("transform from ppp to geom", {
  # test 'SpatialPoints'
  input <- gtPPP

  output <- gc_geom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
})
