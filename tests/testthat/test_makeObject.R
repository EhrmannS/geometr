library(checkmate)
library(testthat)
library(raster)
library(sf)
context("makeObject")


test_that("make object from a geom", {
  output <- makeObject(x = gtGeoms$polygon, window = NULL, theme = gtTheme)
  expect_list(x = output, len = 6)
  expect_names(x = names(output), identical.to = c("type", "name", "out", "hasLegend", "params", "legend"))

  # with a modified theme
  myTheme <- gtTheme
  myTheme@legend$ascending <- FALSE
  output <- makeObject(x = gtGeoms$polygon, window = NULL, theme = myTheme)
  expect_list(x = output, len = 6)
  expect_names(x = names(output), identical.to = c("type", "name", "out", "hasLegend", "params", "legend"))

  # a more complex geom
  nc <- st_read(system.file("shape/nc.shp", package="sf"))
  output <- makeObject(x = gc_geom(input = nc), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 6)
  expect_names(x = names(output), identical.to = c("type", "name", "out", "hasLegend", "params", "legend"))

  # when a deviating window is used
  input <- setWindow(x = gtGeoms$polygon, to = data.frame(x = c(0, 11), y = c(0, 11)))
  expect_warning(object = output <- makeObject(x = input, window = NULL, theme = gtTheme), regexp = "some vertices are not within the plotting window.")
  expect_list(x = output, len = 6)
  expect_names(x = names(output), identical.to = c("type", "name", "out", "hasLegend", "params", "legend"))

  input <- setWindow(x = gtGeoms$polygon, to = data.frame(x = c(3, 5), y = c(3, 5)))
  expect_warning(object = output <- makeObject(x = input, window = NULL, theme = gtTheme), regexp = "no vertices are within the plotting window.")
  expect_list(x = output, len = 6)
  expect_names(x = names(output), identical.to = c("type", "name", "out", "hasLegend", "params", "legend"))
})

test_that("make object from a Raster", {
  # a categorical raster with colourtable
  output <- makeObject(x = gtRasters$categorical, theme = gtTheme)
  expect_list(x = output, len = 8)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "params", "legend", "array"))

  # a categorical raster without colourtable
  input <- gtRasters$categorical
  input@legend@colortable <- list()
  output <- makeObject(x = input, theme = gtTheme)
  expect_list(x = output, len = 8)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "params", "legend", "array"))

  # a continuous raster
  output <- makeObject(x = gtRasters$continuous, theme = gtTheme)
  expect_list(x = output, len = 8)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "params", "legend", "array"))

  # an image
  input <- RGB(gtRasters$continuous)
  output <- makeObject(x = input, image = TRUE, theme = gtTheme)
  expect_names(x = names(output), identical.to = c("type", "name", "rows", "cols", "hasLegend", "array"))

  # errors
  expect_error(object = makeObject(x = input, theme = gtTheme))
  expect_error(object = makeObject(x = gtRasters$categorical, image = TRUE, theme = gtTheme))
})

test_that("make object from a matrix", {
  # a matrix with hexadecimal colour values
  input1 <- matrix(c(rep('#000000', 4)), nrow = 2)
  output <- makeObject(x = input1, image = TRUE, theme = gtTheme)
  expect_list(x = output, len = 5)
  expect_names(x = names(output), identical.to = c("type", "rows", "cols", "hasLegend", "array"))

  # an integer value matrix
  input2 <- matrix(c(rep(1, 4)), nrow = 2)
  output <- makeObject(x = input2, theme = gtTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), identical.to = c("type", "rows", "cols", "hasLegend", "params", "legend", "array"))

  # with a modified theme
  myTheme <- gtTheme
  myTheme@legend$ascending <- FALSE
  output <- makeObject(x = input2, theme = myTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), identical.to = c("type", "rows", "cols", "hasLegend", "params", "legend", "array"))

  # errors
  expect_error(object = makeObject(x = input1, theme = gtTheme))
  expect_error(object = makeObject(x = input2, image = TRUE, theme = gtTheme))
})

test_that("make object from a Spatial", {
  # errors
  expect_error(object = makeObject(x = gtSP$SpatialPolygons))
})

test_that("make object from a sf", {
  # errors
  expect_error(object = makeObject(x = gtSF$polygon))
})
