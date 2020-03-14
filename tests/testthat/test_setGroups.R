library(checkmate)
library(testthat)
library(raster)
context("setGroups")


test_that("setGroups of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  attributes <- data.frame(gid = 1, data = "A")

  # set table with a known variable
  output <- setGroups(x = input, table = attributes)
  expect_class(output, "geom")
  expect_list(output@feature, len = 1)
  expect_names(names(output@group$geometry), must.include = c("gid", "data"))

  # set table with only unknown variables
  output <- setGroups(x = input, table = data.frame(gid = 1, data = "B"))
  expect_class(output, "geom")
  expect_list(output@feature, len = 1)
  expect_names(names(output@group$geometry), must.include = c("gid", "data"))
})

test_that("setGroups of a 'RasterLayer'", {
  input <- raster(system.file("external/rlogo.grd", package="raster"))
  attributes <- data.frame(id = 1:256, variable = sample(x = LETTERS, size = 256, replace = TRUE))

  # test RasterLayer without attribute table
  output <- setGroups(input, attributes)
  expect_class(output, "RasterLayer")
  expect_true(output@data@isfactor)
})
