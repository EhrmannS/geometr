library(checkmate)
library(testthat)
library(raster)
context("setTable")


test_that("setTable of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  attributes <- data.frame(fid = 1, variable = "A")

  output <- setTable(input, attributes)
  expect_class(output, "geom")
  expect_data_frame(output@attr, ncols = 3)
  expect_names(names(output@attr), must.include = c("fid", "gid", "variable"))
})

test_that("setTable of a Spatial*DataFrame object", {
  input <- gtSP$SpatialPolygonsDataFrame
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(a = 1:2, x = c("a", "b"))

  # without matcing columns in x
  output <- setTable(x = input, table = newData1)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 2)
  expect_names(names(output@data), must.include = c("a", "x"))

  # with matcing columns in x
  output <- setTable(x = input, table = newData2)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 2)
  expect_names(names(output@data), must.include = c("a", "x"))
})

test_that("setTable of a Spatial* objects", {
  newData1 <- data.frame(x = c("a", "b"))
  newData2 <- data.frame(x = 1:4)

  # SpatialPoints
  output <- setTable(x = gtSP$SpatialPoints, table = newData2)
  expect_class(output, "SpatialPointsDataFrame")
  expect_data_frame(output@data, nrows = 4, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialMultiPoints
  output <- setTable(x = gtSP$SpatialMultiPoints, table = newData1)
  expect_class(output, "SpatialMultiPointsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialLines
  output <- setTable(x = gtSP$SpatialLines, table = newData1)
  expect_class(output, "SpatialLinesDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))

  # SpatialPolygons
  output <- setTable(x = gtSP$SpatialPolygons, table = newData1)
  expect_class(output, "SpatialPolygonsDataFrame")
  expect_data_frame(output@data, nrows = 2, ncols = 1)
  expect_names(names(output@data), must.include = c("x"))
})

test_that("setTable of an sf object", {

})

test_that("setTable of a 'RasterLayer'", {
  input <- raster(system.file("external/rlogo.grd", package="raster"))
  attributes <- data.frame(id = 1:256, variable = sample(x = LETTERS, size = 256, replace = TRUE))

  # test RasterLayer without attribute table
  output <- setTable(input, attributes)
  expect_class(output, "RasterLayer")
  expect_true(output@data@isfactor)
})
