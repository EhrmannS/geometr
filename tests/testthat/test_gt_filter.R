library(checkmate)
library(testthat)
context("gt_filter")


test_that("subset of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  input <- gs_polygon(anchor = coords)
  input <- setFeatures(x = input, table = data.frame(fid = c(1, 2), attr = c("a", "b")))

  # based on a condition
  output <- gt_filter(obj = input, fid == 2)
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 3)
  expect_true(dim(output@point)[1] < dim(input@point)[1])

  output <- gt_filter(obj = input, attr == 'b')
  expect_class(output, "geom")
  expect_true(dim(output@feature)[1] == 1)
  expect_true(dim(output@feature)[1] < dim(input@group)[1])
})

test_that("subset of a Spatial* object", {
  input <- gc_sp(input = gtGeoms$point)
  input <- SpatialPointsDataFrame(input, data.frame(data = 9:1), match.ID = TRUE)

  # based on a condition
  output <- gt_filter(obj = input, data == 2)
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 1)
  expect_true(dim(output@point)[1] < length(input))
})

test_that("subset of a sf object", {
  temp <- gtGeoms$point
  temp@feature$gid <- temp@feature$fid
  input <- gc_sf(temp)
  input <- cbind(input, data = c(1, 1, 1, 1, 2, 2, 2, 2, 2))

  # based on a condition
  output <- gt_filter(obj = input, data == 2)
  expect_class(output, "geom")
  expect_true(dim(output@point)[1] == 5)
  expect_true(dim(output@point)[1] < dim(input)[1])
})
