library(testthat)
library(checkmate)
library(grid)
context("gt_locate")


test_that("locating in a geom works", {
  visualise(gtGeoms$polygon)

  output <- gt_locate()
  expect_data_frame(x = output)
  expect_names(names(output), identical.to = c("id", "x", "y"))
})

test_that("identifying in a geom works", {
  visualise(gtGeoms$polygon)

  output <- gt_locate(identify = TRUE)
  expect_data_frame(x = output)
  expect_names(names(output), identical.to = c("id", "x", "y", "geom"))
})

test_that("locating in a raster works", {
  visualise(gtRasters$continuous)

  output <- gt_locate()
  expect_data_frame(x = output)
  expect_names(names(output), identical.to = c("id", "x", "y"))
})

test_that("identifying in a raster works", {
  visualise(gtRasters$continuous)

  output <- gt_locate(identify = TRUE)
  expect_data_frame(x = output)
  expect_names(names(output), identical.to = c("id", "x", "y", "value", "colour"))
})