library(testthat)
library(checkmate)
library(grid)
context("gt_locate")


test_that("locating in a geom works", {
  visualise(gtGeoms$polygon)

  bla <- capture_output(gt_locate())
  expect_character(x = bla)
})

test_that("identifying in a geom works", {
  visualise(gtGeoms$polygon)

  bla <- gt_locate(identify = TRUE)
  expect_character(x = bla)
})

test_that("locating in a raster works", {
  visualise(gtRasters$continuous)

  bla <- capture_output(gt_locate())
  expect_character(x = bla)
})

test_that("identifying in a raster works", {
  visualise(gtRasters$continuous)

  bla <- gt_locate(identify = TRUE)
  expect_character(x = bla)
})