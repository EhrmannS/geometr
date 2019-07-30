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

test_that("the correct warnings are printed", {
  visualise(gtRasters$continuous)
  expect_warning(object = gt_locate(identify = TRUE, panel = "categorical"), regexp = "the specified panel did not match any of the existing panels, please select locations in the first panel.")

  visualise(gtRasters)
  expect_warning(object = gt_locate(), regexp = "please select locations in the first panel.")
})

test_that("the correct errors are printed", {
  dev.off()
  expect_error(object = gt_locate(), regexp = "please create a plot with geometr::visualise()")

  plot(c(1:10))
  expect_error(object = gt_locate(), regexp = "please create a plot with geometr::visualise()")
})