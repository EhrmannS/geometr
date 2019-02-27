library(checkmate)
library(testthat)
library(raster)
library(sf)
context("getCRS")


test_that("getCRS of a geom", {
  input <- gtGeoms$locations

  output <- getCRS(input)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of a Spatial object", {
  input <- gtSP$SpatialPoints
  proj4string(input) <- projs$laea

  output <- getCRS(input)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of an sf object", {
  input <- gtSF$point
  input <- st_set_crs(x = input, value = projs$laea)

  output <- getCRS(input)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getCRS of a Raster", {
  aRaster <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

  output <- getCRS(aRaster)
  expect_character(output, any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})