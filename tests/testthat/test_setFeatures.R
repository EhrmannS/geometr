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
  expect_names(names(output@feature), must.include = c("fid", "gid", "data"))

  # set table with only unknown variables
  output <- setFeatures(x = input, table = data.frame(fid = 1, data = "B"))
  expect_class(output, "geom")
  expect_names(names(output@feature), must.include = c("fid", "gid", "data"))
})

test_that("setFeatures of a Spatial*DataFrame object", {
  input <- gc_sp(input = gtGeoms$polygon)
  input <- SpatialPolygonsDataFrame(input, data = data.frame(data = 1:2), match.ID = FALSE)
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(a = 1:2, x = c("a", "b"))

  # without matcing columns in x
  output <- setFeatures(x = input, table = newData1)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 2)
  expect_names(names(output@data), must.include = c("data", "x"))

  # with matcing columns in x
  output <- setFeatures(x = input, table = newData2)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 3)
  expect_names(names(output@data), must.include = c("a", "x"))
})

test_that("setFeatures of a Spatial* objects", {
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(x = 9:1)

  # SpatialPoints
  input <- gc_sp(input = gtGeoms$point)
  output <- setFeatures(x = input, table = newData2)
  expect_class(output, "SpatialPointsDataFrame")
  expect_data_frame(output@data, nrows = 9, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialPixel
  data(meuse.grid)
  pts = meuse.grid[c("x", "y")]
  input = SpatialPixels(SpatialPoints(pts))
  output <- setFeatures(x = input, table = data.frame(data = seq_along(input@coords[,1])))
  expect_class(output, "SpatialPixelsDataFrame")
  expect_data_frame(output@data, nrows = 3103, ncols = 1)
  expect_names(names(output@data), must.include = c("data"))

  # # SpatialMultiPoints
  # output <- setFeatures(x = gtSP$SpatialMultiPoints, table = newData1)
  # expect_class(output, "SpatialMultiPointsDataFrame")
  # expect_data_frame(output@data, nrows = 2, ncols = 1)
  # expect_names(names(output@data), must.include = c("x"))

  # SpatialLines
  input <- gc_sp(input = gtGeoms$line)
  output <- setFeatures(x = input, table = newData1)
  expect_class(output, "SpatialLinesDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialPolygons
  input <- gc_sp(input = gtGeoms$polygon)
  output <- setFeatures(x = input, table = newData1)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))
})

test_that("setFeatures of an sf object", {
  newData1 <- data.frame(x = c(letters[1:9]))
  newData2 <- data.frame(a = c(1, 2), x = c("a", "b"))

  # test POINT
  temp <- gtGeoms$point
  temp@feature$gid <- temp@feature$fid
  input <- gc_sf(temp)
  output <- setFeatures(x = input, newData1)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 9, ncols = 2)

  # test MULITPOINT
  input <- gc_sf(gtGeoms$point)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test LINESTRING
  input <- gc_sf(gtGeoms$line)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test MULTILINESTRING
  temp <- gtGeoms$line
  temp@feature$gid <- 1
  temp@group$gid <- 1
  input <- gc_sf(temp)
  output <- setFeatures(x = input, newData2[1,])
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 1, ncols = 3)

  # test POLYGON
  input <- gc_sf(gtGeoms$polygon)
  output <- setFeatures(x = input, newData2)
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 2, ncols = 3)

  # test MULTIPOLYGON
  temp <- gtGeoms$polygon
  temp@feature$gid <- 1
  temp@group$gid <- 1
  input <- gc_sf(temp)

  output <- setFeatures(x = input, newData2[1,])
  expect_class(output, classes = c("sf", "data.frame"))
  expect_data_frame(x = output, nrows = 1, ncols = 3)
})


