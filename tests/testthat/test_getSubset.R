library(checkmate)
library(testthat)
library(raster)
context("getSubset")


test_that("getSubset of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)

  # get a subset of the coords
  output <- getSubset(x = input, coords = "fid == 2")
  expect_class(output, "geom")
  expect_true(dim(output@coords)[1] == 2)

  # get a subset of the attributes
  input <- setTable(x = input, table = tibble(fid = c(1, 2), a = c("a", "b")))
  output <- getSubset(x = input, attr = "a == 'b'")
  expect_class(output, "geom")
  expect_true(dim(output@coords)[1] == 2)
})

test_that("getSubset of a Spatial* object", {
  # input <- gtSP$SpatialPoints
  #
  # output <- getExtent(input)
  # expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  # expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getSubset of an sf object", {
  input <- gtSF$point

  output <- getSubset(x = input, attr = "a == 2")
  expect_class(output, "sf")
  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
})
