library(checkmate)
library(testthat)
context("gt_scale")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  rectGeom <- gs_polygon(anchor = coords, extent = extent)
  spRectGeom <- setCRS(rectGeom, crs = projs$laea)
  rectGeomRel <- gt_scale(geom = rectGeom, to = "relative")
  spRectGeomRel <- gt_scale(geom = spRectGeom, to = "relative")
  rectGeomAbs <- gt_scale(geom = rectGeomRel, to = "absolute")
  rectRescaled <- gt_scale(geom = rectGeom, range = list(x = c(0, 100), y = c(10, 90)))

  expect_class(rectGeomRel, classes = "geom")
  expect_class(rectGeomAbs, classes = "geom")
  expect_class(rectGeom, classes = "geom")
  expect_class(rectRescaled, classes = "geom")
  expect_class(spRectGeomRel, classes = "geom")
})

test_that("output has correctly scaled values (only 'relative')", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  rectGeom <- gs_polygon(anchor = coords, extent = extent)
  rectGeomRel <- gt_scale(geom = rectGeom, to = "relative")

  expect_true(all(rectGeomRel@coords[c("x", "y")] <= 1))
  expect_true(rectGeomRel@scale == "relative")
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  rectGeom <- gs_polygon(anchor = coords, extent = extent)

  expect_error(gt_scale(geom = rectGeom, to = "bla"))
  expect_error(gt_scale(geom = rectGeom, range = c(0, 100)))
  expect_error(gt_scale(geom = "bla"))
})
