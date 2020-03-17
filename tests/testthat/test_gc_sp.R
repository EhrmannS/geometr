library(checkmate)
library(testthat)
library(sp)
context("gc_sp")


test_that("transform from geom to sp", {
  spPoints <- gc_sp(input = gtGeoms$point)
  expect_class(spPoints, "SpatialPoints")

  spPolygon <- gc_sp(input = gtGeoms$line)
  expect_class(spPolygon, "SpatialLines")

  spPolygon <- gc_sp(input = gtGeoms$polygon)
  expect_class(spPolygon, "SpatialPolygons")
})

test_that("make a Spatial*DataFrame object", {
  # test type == 'point'
  input <- setFeatures(x = gtGeoms$point, table = data.frame(fid = c(1:12), attr = letters[1:12]))
  spPoints <- gc_sp(input)
  expect_class(spPoints, "SpatialPointsDataFrame")
  expect_names(x = names(spPoints), identical.to = c("attr"))

  input <- setGroups(x = gtGeoms$point, table = data.frame(gid = c(1:3), attr = letters[1:3]))
  spPoints <- gc_sp(input)
  expect_class(spPoints, "SpatialPointsDataFrame")
  expect_names(x = names(spPoints), identical.to = c("attr"))

  # test type == 'line'
  input <- setFeatures(x = gtGeoms$line, table = data.frame(fid = c(1:3), attr = letters[1:3]))
  spPoints <- gc_sp(input)
  expect_class(spPoints, "SpatialLinesDataFrame")
  expect_names(x = names(spPoints), identical.to = c("attr"))

  input <- setGroups(gtGeoms$line, table = data.frame(gid = c(1:3), attr = letters[1:3]))
  spLines <- gc_sp(input)
  expect_class(spLines, "SpatialLinesDataFrame")
  expect_names(x = names(spLines), identical.to = c("attr"))

  # test type == 'polygon'
  input <- setFeatures(gtGeoms$polygon, table = data.frame(fid = c(1:2), attr = letters[1:2]))
  spPolygon <- gc_sp(input)
  expect_class(spPolygon, "SpatialPolygonsDataFrame")
  expect_names(x = names(spPolygon), identical.to = c("attr"))

  input <- setGroups(gtGeoms$polygon, table = data.frame(gid = c(1:2), attr = letters[1:2]))
  spLines <- gc_sp(input)
  expect_class(spLines, "SpatialPolygonsDataFrame")
  expect_names(x = names(spLines), identical.to = c("attr"))
})

test_that("output has correct length", {
  spPolygon <- gc_sp(input = gtGeoms$point)
  expect_equal(length(spPolygon), 12)
})

test_that("also objects with CRS are properly handled", {
  input <- setCRS(x = gtGeoms$point, crs = "+proj=longlat +ellps=sphere +no_defs")

  output <- gc_sp(input = input)
  expect_equal(length(output), 12)
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))

  expect_error(gc_sp(input = "bla"))
  expect_error(gc_sp(input = notAGeom))
})
