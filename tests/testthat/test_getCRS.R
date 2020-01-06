library(checkmate)
library(testthat)
library(raster)
library(sf)
context("getCRS")


test_that("getCRS of a geom", {
  input <- gtGeoms$polygon
  input <- setCRS(x = input, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

  output <- getCRS(input)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of a Spatial object", {
  input <- gtSP$SpatialPoints
  input <- setCRS(x = input, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

  output <- getCRS(input)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of an sf object", {
  input <- gtSF$point
  input <- setCRS(x = input, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

  output <- getCRS(input)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of a ppp object", {
  output <- getCRS(gtPPP)
  expect_character(output, pattern = "NA", len = 1)
})

test_that("getCRS of a Raster", {
  aRaster <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

  output <- getCRS(aRaster)
  expect_character(output, any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})

test_that("getCRS of any other object", {
  output <- getCRS("bla")
  expect_true(object = is.na(output))
})