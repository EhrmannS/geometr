library(checkmate)
library(testthat)
library(raster)
context("visualise")


test_that("visualise a Raster* object", {

  output <- visualise(raster = gtRasters$continuous)
  expect_class(output, "recordedplot")

  input <- brick(system.file("external/rlogo.grd", package="raster"))
  output <- visualise(raster = input)
  expect_class(output, "recordedplot")
})

test_that("visualise a matrix", {
  aMatrix <- raster::as.matrix(gtRasters$continuous)

  output <- visualise(`my matrix` = aMatrix)
  expect_class(output, "recordedplot")
})

test_that("visualise an image", {
  continuous <- gtRasters$categorical
  input <<- brick(system.file("external/rlogo.grd", package="raster"))

  output <- visualise(raster = input, image = TRUE)
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

  aTheme <- setTheme(vector = list(linewidth = c(1, 3),
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
  output <- visualise(geom = gtGeoms$polygon, pointsize = "fid", theme = aTheme)
  expect_class(output, "recordedplot")

  # pointsymbol
  output <- visualise(geom = gtGeoms$polygon, pointsymbol = "fid", theme = aTheme)
  expect_class(output, "recordedplot")

  # linewidth
  output <- visualise(geom = gtGeoms$polygon, linewidth = "fid", theme = aTheme)
  expect_class(output, "recordedplot")

  # linetype
  output <- visualise(geom = gtGeoms$polygon, linetype = "fid", theme = aTheme)
  expect_class(output, "recordedplot")
})

test_that("visualise a geom on top of an already plotted raster", {
  continuous <- gtRasters$continuous
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

  output <- capture_message(visualise(gtRasters$continuous, trace = TRUE))
  expect_class(output, "simpleMessage")

  # from a RasterBrick
  output <- capture_message(visualise(gtRasters, trace = TRUE))
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

test_that("Error if arguments have wrong value", {
  continuous <<- gtRasters$continuous

  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords)
  # anImage <- system.file()

  expect_error(visualise())
  expect_error(visualise(raster = "bla"))
  expect_error(visualise(raster = continuous, geom = "bla"))
  expect_error(visualise(raster = continuous, theme = "bla"))
  expect_error(visualise(raster = continuous, trace = 1))
  expect_error(visualise(raster = continuous, image = 0))
})
