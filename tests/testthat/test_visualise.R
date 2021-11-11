library(checkmate)
library(testthat)
library(raster)
context("visualise")


test_that("visualise a RasterLayer object", {
  output <- visualise(raster = gc_raster(gtGeoms$grid$continuous))
  expect_class(output, "recordedplot")
})

test_that("visualise a RasterBrick object", {
  input <- brick(system.file("external/rlogo.grd", package="raster"))
  output <- visualise(raster = gc_geom(input = input, as_hex = TRUE))
  expect_class(output, "recordedplot")
})

test_that("visualise a matrix", {
  aMatrix <- raster::as.matrix(gc_raster(gtGeoms$grid$continuous))

  output <- visualise(`my matrix` = aMatrix)
  expect_class(output, "recordedplot")
})

test_that("visualise a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  input <- gs_polygon(anchor = coords)

  output <- visualise(geom = input, clip = FALSE)
  expect_class(output, "recordedplot")
})

test_that("quick options produce output", {

  aTheme <- setTheme(parameters = list(linewidth = c(1, 3),
                                       pointsize = c(1, 3),
                                       pointsymbol = c(0:12),
                                       linetype = c(1, 2)))

  # linecol
  output <- visualise(geom = gtGeoms$polygon, linecol = "fid")
  expect_class(output, "recordedplot")

  # fillcol
  output <- visualise(geom = gtGeoms$polygon, fillcol = "fid")
  expect_class(output, "recordedplot")

  # pointsize
  output <- visualise(geom = gtGeoms$point, pointsize = "fid", theme = aTheme)
  expect_class(output, "recordedplot")

  # linewidth
  output <- visualise(geom = gtGeoms$polygon, linewidth = "fid", theme = aTheme)
  expect_class(output, "recordedplot")

  # pointsymbol
  output <- visualise(geom = gtGeoms$point, pointsymbol = "fid", theme = aTheme)
  expect_class(output, "recordedplot")

  # linetype
  output <- visualise(geom = gtGeoms$polygon, linetype = "fid", theme = aTheme)
  expect_class(output, "recordedplot")
})

test_that("visualise a geom on top of an already plotted raster", {
  continuous <- gc_raster(gtGeoms$grid$continuous)
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  input <- gs_polygon(anchor = coords)

  visualise(raster = continuous)
  output <- visualise(geom = input, new = FALSE)
  expect_class(output, "recordedplot")
})

test_that("output the history of a plotted object", {
  # from a RasterLayer

  output <- capture_message(visualise(gc_raster(gtGeoms$grid$continuous), trace = TRUE))
  expect_class(output, "simpleMessage")

  # from a geom
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70))
  aGeom <- gs_polygon(anchor = coords)

  output <- capture_messages(visualise(bla = aGeom, trace = TRUE))
  expect_character(x = output, pattern = "this object has the following history:\n -> object was created as 'polygon' geom.\n")

  input <- brick(system.file("external/rlogo.grd", package="raster"))
  input@history <- list("bla")
  output <- capture_message(visualise(raster = input, trace = TRUE))
  expect_class(output, "simpleMessage")
})

test_that("Error/warning if arguments have wrong value", {
  continuous <- gc_raster(gtGeoms$grid$continuous)

  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords)

  expect_error(visualise(raster = continuous, theme = "bla"))
  expect_error(visualise(raster = continuous, trace = 1))
})

