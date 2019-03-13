library(checkmate)
library(testthat)
context("gt_as_sp")


test_that("output has class Spatial*", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                           5234735, 5281527, 5189955, 5041066),
                           Y = c(3977612, 3971119, 4028167, 3997230,
                           4060164, 4117856, 4118207, 4062838),
                           fid = c(1:8))

  pointsGeom <- gs_point(anchor = input)
  spPoints <- gt_as_sp(geom = pointsGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_class(spPoints, "SpatialPoints")

  # linesGeom <- geomCurve(anchor = somePoints, show = FALSE)
  # spLines <- gt_as_sp(geom = linesGeom, crs = LAEA)
  # expect_class(spPoints, "SpatialLines")

  pointsGeom <- gt_group(geom = pointsGeom, index = c(rep(1, 8)))
  polyGeom <- gs_polygon(anchor = pointsGeom)
  spPolygon <- gt_as_sp(geom = polyGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_class(spPolygon, "SpatialPolygons")
})

test_that("output has correct length", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  polyGeom <- gs_polygon(anchor = input)
  spPolygon <- gt_as_sp(geom = polyGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_equal(length(spPolygon), 2)
})

test_that("output has proper coordinate reference system", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  polyGeom <- gs_polygon(anchor = input, show = FALSE)
  spPolygon <- gt_as_sp(geom = polyGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  expect_equal(spPolygon@proj4string@projargs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

  spPolygon <- gt_as_sp(geom = polyGeom)
  expect_equal(spPolygon@proj4string@projargs, as.character(NA))

  polyGeom <- setCRS(x = polyGeom, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  spPolygon <- gt_as_sp(geom = polyGeom, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  expect_equal(spPolygon@proj4string@projargs, "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  aGeom <- gs_polygon(anchor = input, show = FALSE)

  expect_error(gt_as_sp(geom = "bla"))
  expect_error(gt_as_sp(geom = notAGeom))
  expect_error(gt_as_sp(geom = aGeom, crs = "LAEA"))
})
