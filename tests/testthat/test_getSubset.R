library(testthat)
library(checkmate)
context("getSubset")


test_that("getSubset of a geom, based on a condition", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  input <- gs_polygon(anchor = coords)

  # get a subset of the vertices
  output <- getSubset(x = input, fid == 2, slot = "point")
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 3)
  expect_true(dim(output@point)[1] < dim(input@point)[1])

  # get a subset of the features
  input <- setTable(x = input,
                    table = data.frame(fid = c(1, 2), attr = c("a", "b")),
                    slot = "feature")
  output <- getSubset(x = input, attr == 'b', slot = "feature")
  expect_class(output, "geom")
  expect_true(dim(output@feature$geometry)[1] == 1)
  expect_true(dim(output@feature$geometry)[1] < dim(input@group$geometry)[1])

  # get a subset of the groups
  input <- setTable(x = input,
                    table = data.frame(gid = c(1, 2), attr = c("A", "B")),
                    slot = "group")
  output <- getSubset(x = input, attr == 'B', slot = "group")
  expect_class(output, "geom")
  expect_true(dim(output@group$geometry)[1] == 1)
  expect_true(dim(output@group$geometry)[1] < dim(input@group$geometry)[1])
})

test_that("getSubset of a geom, based on a logical", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  input <- gs_polygon(anchor = coords)

  # get a subset of the vertices
  subset <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  output <- getSubset(x = input, subset, slot = "point")
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 4)
  expect_true(dim(output@point)[1] < dim(input@point)[1])

  # get a subset of the features
  subset <- c(TRUE, FALSE)
  output <- getSubset(x = input, subset, slot = "feature")
  expect_class(output, "geom")
  expect_true(dim(output@feature$geometry)[1] == 1)
  expect_true(dim(output@feature$geometry)[1] < dim(input@group$geometry)[1])

  # get a subset of the groups
  subset <- c(TRUE, FALSE)
  output <- getSubset(x = input, subset, slot = "group")
  expect_class(output, "geom")
  expect_true(dim(output@group$geometry)[1] == 1)
  expect_true(dim(output@group$geometry)[1] < dim(input@group$geometry)[1])
})

test_that("getSubset of a Spatial* object", {
  input <- gtSP$SpatialPointsDataFrame

  output <- getSubset(x = input, a == 2)
  expect_class(output, "SpatialPointsDataFrame")
  expect_true(length(output) == 1)
  expect_true(length(output) < length(input))

  subset <- c(TRUE, FALSE, TRUE, FALSE)
  output <- getSubset(x = input, subset)
  expect_class(output, "SpatialPointsDataFrame")
  expect_true(length(output) == 2)
  expect_true(length(output) < length(input))
})

test_that("getSubset of an sf object", {
  input <- gtSF$point

  output <- getSubset(x = input, a == 2)
  expect_class(output, "sf")
  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)

  subset <- c(TRUE, FALSE)
  output <- getSubset(x = input, subset)
  expect_class(output, "sf")
  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
})

test_that("getSubset of any other object", {
  output <- getSubset("bla")
  expect_null(object = output)
})
