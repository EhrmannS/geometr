library(checkmate)
library(testthat)
context("gt_subset")


test_that("subset of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  input <- gs_polygon(anchor = coords)
  input <- setFeatures(x = input, table = data.frame(fid = c(1, 2), attr = c("a", "b")))

  # based on a condition
  output <- gt_subset(obj = input, fid == 2)
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 3)
  expect_true(dim(output@point)[1] < dim(input@point)[1])

  output <- gt_subset(obj = input, attr == 'b')
  expect_class(output, "geom")
  expect_true(dim(output@feature$geometry)[1] == 1)
  expect_true(dim(output@feature$geometry)[1] < dim(input@group$geometry)[1])
})

test_that("subset of a Spatial* object", {
  input <- gtSP$SpatialPointsDataFrame

  # based on a condition
  output <- gt_subset(obj = input, a == 2)
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 1)
  expect_true(dim(output@point)[1] < length(input))
})

test_that("subset of a sf object", {
  input <- gtSF$point

  # based on a condition
  output <- gt_subset(obj = input, a == 2)
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 1)
  expect_true(dim(output@point)[1] < length(input))
})
