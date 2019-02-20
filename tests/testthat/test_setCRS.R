library(checkmate)
library(testthat)
library(raster)
library(sp)
context("setCRS")


test_that("setCRS of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)

  # setting a CRS on a geom that hasn't had one before
  output <- setCRS(x = aGeom, crs = projs$laea)
  expect_class(output, classes = "geom")
  expect_character(getCRS(output), any.missing = FALSE, pattern = "+proj=laea", len = 1)

  # setting a CRS on a geom that had one before
  output <- setCRS(x = output, crs = projs$longlat)
  expect_class(output, classes = "geom")
  expect_character(getCRS(output), any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})

test_that("getExtent of a Spatial object", {
  x = c(1, 2, 3, 4, 5)
  y = c(3, 2, 5, 1, 4)
  aSpatial <- SpatialPoints(cbind(x, y))

  # setting a CRS on a Spatial* that hasn't had one before
  output <- setCRS(x = aSpatial, crs = projs$laea)
  expect_class(output, classes = "SpatialPoints")
  expect_character(proj4string(output), any.missing = FALSE, pattern = "+proj=laea", len = 1)

  # setting a CRS on a Spatial* that had one before
  output <- setCRS(x = output, crs = projs$longlat)
  expect_class(output, classes = "SpatialPoints")
  expect_character(proj4string(output), any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})

test_that("getExtent of an sf object", {
  input <- gtSF$polygon

  output <- setCRS(x = input, crs = projs$laea)
  expect_class(output, classes = "sf")
  expect_character(st_crs(output)$proj4string, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})
