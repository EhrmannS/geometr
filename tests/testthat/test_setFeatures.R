library(checkmate)
library(testthat)
library(raster)
context("setFeatures")


test_that("setFeatures of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  attributes <- data.frame(fid = 1, data = "A")

  # set table with a known variable
  output <- setFeatures(x = input, table = attributes)
  expect_class(output, "geom")
  expect_list(output@feature, len = 1)
  expect_names(names(output@feature$geometry), must.include = c("fid", "gid", "data"))

  # set table with only unknown variables
  output <- setFeatures(x = input, table = data.frame(fid = 1, data = "B"))
  expect_class(output, "geom")
  expect_list(output@feature, len = 1)
  expect_names(names(output@feature$geometry), must.include = c("fid", "gid", "data"))
})

test_that("setFeatures of a Spatial*DataFrame object", {
  input <- gtSP$SpatialPolygonsDataFrame
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(a = 1:2, x = c("a", "b"))

  # without matcing columns in x
  output <- setFeatures(x = input, table = newData1)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 2)
  expect_names(names(output@data), must.include = c("a", "x"))

  # with matcing columns in x
  output <- setFeatures(x = input, table = newData2)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 2)
  expect_names(names(output@data), must.include = c("a", "x"))
})

test_that("setFeatures of a Spatial* objects", {
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(x = 1:4)

  # SpatialPoints
  output <- setFeatures(x = gtSP$SpatialPoints, table = newData2)
  expect_class(output, "SpatialPointsDataFrame")
  expect_data_frame(output@data, nrows = 4, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialPixel
  data(meuse.grid)
  pts = meuse.grid[c("x", "y")]
  input = SpatialPixels(SpatialPoints(pts))
  output <- setFeatures(x = input, table = data.frame(data = seq_along(input@coords[,1])))
  expect_class(output, "SpatialPixelsDataFrame")
  expect_data_frame(output@data, nrows = 3103, ncols = 1)
  expect_names(names(output@data), must.include = c("data"))

  # SpatialMultiPoints
  output <- setFeatures(x = gtSP$SpatialMultiPoints, table = newData1)
  expect_class(output, "SpatialMultiPointsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialLines
  output <- setFeatures(x = gtSP$SpatialLines, table = newData1)
  expect_class(output, "SpatialLinesDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialPolygons
  output <- setFeatures(x = gtSP$SpatialPolygons, table = newData1)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))
})

test_that("setFeatures of an sf object", {
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(a = c(1, 2), x = c("a", "b"))

  # test POINT
  input <- gtSF$point
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test MULITPOINT
  input <- gtSF$multipoint
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test LINESTRING
  sfObj <- gtSF$linestring
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test MULTILINESTRING
  sfObj <- gtSF$multilinestring
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test POLYGON
  sfObj <- gtSF$polygon
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test MULTIPOLYGON
  sfObj <- gtSF$multipolygon
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)
})

test_that("setFeatures of an sfc object", {
  newData <- data.frame(x = c("a", "b"))

  input <- st_geometry(gtSF$point)
  output <- setFeatures(x = input, newData)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 2)
})

test_that("setFeatures of a ppp object", {
  newData <- data.frame(attr = LETTERS[1:15],
                        colour = topo.colors(15))

  input <- gtPPP
  output <- setFeatures(x = input, newData)
  expect_class(output, classes = c("ppp"))
})


