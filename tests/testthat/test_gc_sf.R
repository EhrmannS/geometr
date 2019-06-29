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

  # test with point attributes
  pointGeomV <- setTable(x = pointGeom, slot = "vert",
                         table = data.frame(vertex = "first"))

  output <- gc_sf(input = pointGeomV)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("vertex", "geom"))

  # test with feature attributes
  pointGeomF <- setTable(x = pointGeom, slot = "feat",
                         table = data.frame(gid = 1, feature = "a"))
  output <- gc_sf(input = pointGeomF)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("feature", "geom"))

  pointGeomVF <- setTable(x = pointGeomV, slot = "feat",
                          table = data.frame(gid = 1, feature = "a"))
  output <- gc_sf(input = pointGeomVF)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("vertex", "feature", "geom"))

  # test with group attributes
  pointGeomFG <- setTable(x = pointGeomF, slot = "group",
                          table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomFG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("feature", "group", "geom"))

  pointGeomVFG <- setTable(x = pointGeomVF, slot = "group",
                           table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomVFG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("vertex", "feature", "group", "geom"))

  pointGeomG <- setTable(x = pointGeom, slot = "group",
                         table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("group", "geom"))

  pointGeomVG <- setTable(x = pointGeomV, slot = "group",
                          table = data.frame(gid = 1, group = "red"))
  output <- gc_sf(input = pointGeomVG)
  expect_class(x = output, classes = "sf")
  expect_true(st_geometry_type(output) == "POINT")
  expect_names(x = names(output), identical.to = c("vertex", "group", "geom"))
})

test_that("transform geom to MULTIPOINT sf", {
  output <- gc_sf(input = gtGeoms$point)
  expect_class(x = output, classes = "sf")
  expect_list(x = output$geom, types = "numeric", len = 3)
  expect_names(x = names(output), identical.to = c("geom"))

  pointGeomV <- setTable(x = gtGeoms$point, slot = "vert",
                         table = data.frame(vertex = LETTERS[1:12]))
  expect_warning(object = output <- gc_sf(input = pointGeomV))
  expect_class(x = output, classes = "sf")
  expect_list(x = output$geom, types = "numeric", len = 3)
  expect_names(x = names(output), identical.to = c("geom"))
})

test_that("transform geom to LINE sf", {
  output <- gc_sf(input = gtGeoms$line)
  expect_class(output, "sf")
  expect_list(x = output$geom, len = 3)

})

test_that("transform geom to MULTILINE sf", {


})

test_that("transform geom to POLYGON sf", {
  output <- gc_sf(input = gtGeoms$polygon)
  expect_class(output, "sf")
  expect_list(x = output$geom, len = 2)

})

test_that("transform geom to MULTIPOLYGON sf", {

})
