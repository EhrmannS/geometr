library(checkmate)
library(testthat)
library(raster)
context("makeObject")


test_that("make object from a geom", {
  output <- makeObject(x = gtGeoms$polygon, theme = gtTheme)
  expect_list(x = output, len = 6)
  expect_names(x = names(output), identical.to = c("type", "name", "out", "hasLegend", "uniqueValues", "legend"))
})

test_that("make object from a Raster", {
  output <- makeObject(x = gtRasters$categorical, theme = gtTheme)
  expect_list(x = output, len = 8)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "uniqueValues", "legend", "array"))

  input <- gtRasters$categorical
  input@legend@colortable <- list()
  output <- makeObject(x = input, theme = gtTheme)
  expect_list(x = output, len = 8)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "uniqueValues", "legend", "array"))

  output <- makeObject(x = gtRasters$continuous, theme = gtTheme)
  expect_list(x = output, len = 8)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "uniqueValues", "legend", "array"))

  input <- RGB(gtRasters$continuous)
  output <- makeObject(x = input, image = TRUE, theme = gtTheme)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "array"))

  expect_error(object = makeObject(x = input, theme = gtTheme))
})

test_that("make object from a matrix", {
  input1 <- matrix(c(rep('#000000', 4)), nrow = 2)
  output <- makeObject(x = input1, image = TRUE, theme = gtTheme)
  expect_list(x = output, len = 5)
  expect_names(x = names(output), identical.to = c("type", "rows", "cols", "hasLegend", "array"))

  input2 <- matrix(c(rep(1, 4)), nrow = 2)
  output <- makeObject(x = input2, theme = gtTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), identical.to = c("type", "rows", "cols", "hasLegend", "uniqueValues", "legend", "array"))

  expect_error(object = makeObject(x = input1, theme = gtTheme))
})

test_that("make object from a Spatial", {
  expect_error(object = makeObject(x = gtSP$SpatialPolygons))
})

test_that("make object from a sf", {
  expect_error(object = makeObject(x = gtSF$polygon))
})
