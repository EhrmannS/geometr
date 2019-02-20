library(checkmate)
context("gt_group")


test_that("output is valid geometry", {
  input <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                      y = c(40, 40, 60, 70, 10, 20, 40),
                      fid = 1)
  extent = data.frame(x = c(0, 80),
                      y = c(0, 80))
  aGeom <- gs_polygon(anchor = input, extent = extent)

  groupedGeom <- gt_group(geom = aGeom, distance = 40)
  expect_class(groupedGeom, classes = "geom")

  groupedGeom <- gt_group(geom = aGeom, index = c(1, 1, 1, 1, 2, 2, 2, 2))
  expect_class(groupedGeom, classes = "geom")

  groupedGeom <- gt_group(geom = aGeom, clusters = 2)
  expect_class(groupedGeom, classes = "geom")
})

test_that("output has different values (fid) than input", {
  input <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                      y = c(40, 40, 60, 70, 10, 20, 40),
                      fid = 1)
  extent = data.frame(x = c(0, 80),
                      y = c(0, 80))
  aGeom <- gs_polygon(anchor = input, extent = extent)

  groupedGeom <- gt_group(geom = aGeom, distance = 40)
  expect_false(length(aGeom@attr$fid) == length(groupedGeom@attr$fid))

  groupedGeom <- gt_group(geom = aGeom, index = c(1, 1, 1, 1, 2, 2, 2, 2))
  expect_false(length(aGeom@attr$fid) == length(groupedGeom@attr$fid))

  groupedGeom <- gt_group(geom = aGeom, clusters = 2)
  expect_false(length(aGeom@attr$fid) == length(groupedGeom@attr$fid))
})

test_that("Error if arguments have wrong value", {
  input <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                      y = c(40, 40, 60, 70, 10, 20, 40),
                      fid = 1)
  extent = data.frame(x = c(0, 80),
                      y = c(0, 80))
  aGeom <- gs_polygon(anchor = input, extent = extent)

  expect_error(gt_group(geom = "bla"))
  expect_error(gt_group(geom = aGeom))
  expect_error(gt_group(geom = aGeom, distance = "bla"))
  expect_error(gt_group(geom = aGeom, index = "bla"))
  expect_error(gt_group(geom = aGeom, clusters = "bla"))
  expect_error(gt_group(geom = aGeom, clusters = 2.3))
})
