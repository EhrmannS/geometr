library(checkmate)
library(testthat)
library(raster)
library(spatstat)
context("getGroups")


test_that("getGroups of a 'geom'", {
  # test grid geom
  # ... several layers
  input <- gc_geom(gtRasters)
  output <- getGroups(input)
  expect_list(x = output, any.missing = FALSE, len = 2)
  expect_names(x = names(output), permutation.of = c("categorical", "continuous"))
  expect_data_frame(output$categorical, any.missing = FALSE, nrows = 9, ncols = 2)
  expect_data_frame(output$continuous, any.missing = FALSE, nrows = 0, ncols = 1)

  # ... layer with attribute table
  input <- gc_geom(gtRasters$categorical)
  output <- getGroups(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 9, ncols = 2)
  expect_names(x = names(output), permutation.of = c("gid", "cover"))

  # ... layer without attribute table
  input <- gc_geom(gtRasters$continuous)
  output <- getGroups(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 0, ncols = 1)
  expect_names(x = names(output), permutation.of = c("gid"))
})

test_that("getGroups with subset of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  input <- gs_polygon(anchor = coords)

  # based on a condition
  input <- setGroups(x = input, table = data.frame(gid = c(1, 2), attr = c("A", "B")))
  output <- getGroups(x = input, attr == 'B')
  expect_class(output, "geom")
  expect_true(dim(output@group$geometry)[1] == 1)
  expect_true(dim(output@group$geometry)[1] < dim(input@group$geometry)[1])

  # based on a logical
  subset <- c(TRUE, FALSE)
  output <- getGroups(x = input, subset)
  expect_class(output, "geom")
  expect_true(dim(output@group$geometry)[1] == 1)
  expect_true(dim(output@group$geometry)[1] < dim(input@group$geometry)[1])
})

test_that("getGroups of a Raster* object", {

  # test RasterLayer without attribute table
  input <- gtRasters$continuous
  output <- getGroups(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 0, ncols = 1)
  expect_names(x = names(output), permutation.of = c("gid"))

  # test RasterLayer with attribute table
  input <- gtRasters$categorical
  output <- getGroups(input)
  expect_tibble(output, any.missing = FALSE, nrows = 9, ncols = 2)
  expect_names(names(output), permutation.of = c("gid", "cover"))
})

test_that("getGroups returns a given raster attribute table", {
  input <- gtRasters$categorical

  output <- getGroups(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 9, ncols = 2)
  expect_names(names(output), identical.to = c("gid", "cover"))
})

test_that("getGroups of any other object", {
  output <- getGroups("bla")
  expect_null(object = output)
})

