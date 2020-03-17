library(checkmate)
library(testthat)
library(sf)
context("gc_sf")


test_that("transform geom to POINT sf", {
  # test mere sf
  pointGeom <- gs_point(anchor = data.frame(x = 5, y = 5),
                        window = data.frame(x = c(0, 10), y = c(0, 10)))

  output <- gc_sf(input = pointGeom)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = "geom")

 # test with feature attributes
  pointGeomF <- setFeatures(x = pointGeom, table = data.frame(fid = 1, feature = "a"))
  output <- gc_sf(input = pointGeomF)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("feature", "geom"))

  pointGeomVF <- setFeatures(x = pointGeom, table = data.frame(fid = 1, feature = "a"))
  output <- gc_sf(input = pointGeomVF)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("feature", "geom"))

  # test with group attributes
  pointGeomFG <- setGroups(x = pointGeomF, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomFG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("feature", "group", "geom"))

  pointGeomVFG <- setGroups(x = pointGeomVF, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomVFG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("feature", "group", "geom"))

  pointGeomG <- setGroups(x = pointGeom, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("group", "geom"))

  pointGeomVG <- setGroups(x = pointGeom, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomVG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("group", "geom"))
})

test_that("transform geom to MULTIPOINT sf", {
  output <- gc_sf(input = gtGeoms$point)
  expect_class(x = output, classes = "sf")
  expect_list(x = output$geom, types = "numeric", len = 3)
  expect_names(x = names(output), identical.to = c("geom"))
})

test_that("transform geom to LINESTRING sf", {
  # test mere sf
  lineGeom <- gs_line(anchor = data.frame(x = c(0, 0, 10, 10), y = c(0, 10, 10, 0)),
                      window = data.frame(x = c(0, 10), y = c(0, 10)))

  output <- gc_sf(input = lineGeom)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "LINESTRING")
  expect_names(x = names(output), identical.to = "geom")

  # test with feature attributes
  lineGeomF <- setFeatures(x = lineGeom, table = data.frame(fid = 1, feature = "a"))
  output <- gc_sf(input = lineGeomF)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "LINESTRING")
  expect_names(x = names(output), identical.to = c("feature", "geom"))

  # test with group attributes
  lineGeomG <- setGroups(x = lineGeom, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = lineGeomG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "LINESTRING")
  expect_names(x = names(output), identical.to = c("group", "geom"))

  lineGeomFG <- setGroups(x = lineGeomF, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = lineGeomFG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "LINESTRING")
  expect_names(x = names(output), identical.to = c("feature", "group", "geom"))
})

test_that("transform geom to MULTILINE sf", {
  input <- gtGeoms$line
  input@feature$geometry$gid <- c(1, 1, 2)
  input@group <- list(geometry = tibble(gid = c(1, 2)))
  lineGeomF <- setFeatures(x = input, table = data.frame(fid = 1, feature = LETTERS[1:3]))
  expect_warning(object = output <- gc_sf(input = lineGeomF))
  expect_class(x = output, classes = "sf")
  expect_list(x = output$geom, len = 2)
  expect_names(x = names(output), identical.to = c("geom"))
})

test_that("transform geom to POLYGON sf", {
  # test mere sf
  polyGeom <- gs_polygon(anchor = data.frame(x = c(0, 0, 10, 10, 0), y = c(0, 10, 10, 0, 0)),
                         window = data.frame(x = c(0, 10), y = c(0, 10)))

  output <- gc_sf(input = polyGeom)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POLYGON")
  expect_names(x = names(output), identical.to = "geom")

  # test with feature attributes
  polyGeomF <- setFeatures(x = polyGeom, table = data.frame(fid = 1, feature = "a"))
  output <- gc_sf(input = polyGeomF)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POLYGON")
  expect_names(x = names(output), identical.to = c("feature", "geom"))

  # test with group attributes
  polyGeomG <- setGroups(x = polyGeom, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = polyGeomG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POLYGON")
  expect_names(x = names(output), identical.to = c("group", "geom"))

  polyGeomFG <- setGroups(x = polyGeomF, table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = polyGeomFG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POLYGON")
  expect_names(x = names(output), identical.to = c("feature", "group", "geom"))
})

test_that("transform geom to MULTIPOLYGON sf", {
  polyGeom <- gs_polygon(anchor = data.frame(x = c(0, 0, 10, 10, 0, 9, 9, 10, 10, 9, 11, 11, 12, 12, 11),
                                             y = c(0, 10, 10, 0, 0, 11, 12, 12, 11, 11, 9, 10, 10, 9, 9),
                                             fid = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)),
                         window = data.frame(x = c(0, 12), y = c(0, 12)))
  polyGeom@feature$geometry$gid <- c(1, 1, 2)
  polyGeom@group <- list(geometry = tibble(gid = c(1, 2)))

  polyGeomF <- setFeatures(x = polyGeom, table = data.frame(fid = 1, feature = LETTERS[1:3]))
  expect_warning(object = output <- gc_sf(input = polyGeomF))
  expect_class(x = output, classes = "sf")
  expect_list(x = output$geom, len = 2)
  expect_names(x = names(output), identical.to = c("geom"))
})
