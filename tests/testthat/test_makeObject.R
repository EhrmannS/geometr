library(checkmate)
library(testthat)
library(raster)
library(sf)
context("makeObject")


test_that("make object from a geom", {
  output <- makeObject(x = list(gtGeoms$polygon), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))

  # with a modified theme
  myTheme <- gtTheme
  myTheme@legend$ascending <- FALSE
  output <- makeObject(x = list(gtGeoms$polygon), window = NULL, theme = myTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))

  # a more complex geom
  nc <- st_read(system.file("shape/nc.shp", package="sf"))
  output <- makeObject(x = list(gc_geom(input = nc)), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))

  # when a deviating window is used
  input <- setWindow(x = gtGeoms$polygon, to = data.frame(x = c(0, 11), y = c(0, 11)))
  expect_warning(object = output <- makeObject(x = list(input), window = NULL, theme = gtTheme), regexp = "some vertices are not within the plotting window.")
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))

  input <- setWindow(x = gtGeoms$polygon, to = data.frame(x = c(3, 5), y = c(3, 5)))
  expect_warning(object = output <- makeObject(x = list(input), window = NULL, theme = gtTheme), regexp = "no vertices are within the plotting window.")
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))
})

test_that("make object from a Raster", {
  # a categorical raster with colourtable
  output <- makeObject(x = list(gtRasters$categorical), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 10)
  expect_names(x = names(output), permutation.of = c("type", "name", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # a categorical raster without colourtable
  input <- gtRasters$categorical
  input@legend@colortable <- list()
  output <- makeObject(x = input, window = NULL, theme = gtTheme)
  expect_list(x = output, len = 10)
  expect_names(x = names(output), permutation.of = c("type", "name", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # a continuous raster
  output <- makeObject(x = list(gtRasters$continuous), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 10)
  expect_names(x = names(output), permutation.of = c("type", "name", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # an image
  input <- RGB(gtRasters$continuous)
  output <- makeObject(x = list(input), image = TRUE, window = NULL, theme = gtTheme)
  expect_names(x = names(output), permutation.of = c("type", "name", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # errors
  # expect_error(object = makeObject(x = input, theme = gtTheme))
  expect_error(object = makeObject(x = list(gtRasters$categorical), window = NULL, image = TRUE, theme = gtTheme))
})

test_that("make object from a matrix", {
  # a matrix with hexadecimal colour values
  input1 <- matrix(c(rep('#000000', 4)), nrow = 2)
  output <- makeObject(x = list(input1), window = NULL, image = TRUE, theme = gtTheme)
  expect_names(x = names(output), permutation.of = c("type", "name", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # an integer value matrix
  input2 <- matrix(c(rep(1, 4)), nrow = 2)
  output <- makeObject(x = list(input2), window = NULL, theme = gtTheme)
  expect_names(x = names(output), permutation.of = c("type", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # with a modified theme
  myTheme <- gtTheme
  myTheme@legend$ascending <- FALSE
  output <- makeObject(x = list(input2), window = NULL, theme = myTheme)
  expect_names(x = names(output), permutation.of = c("type", "extent", "window", "rows", "cols", "hasLegend", "params", "legend", "values"))

  # errors
  expect_error(object = makeObject(x = list(input1), theme = gtTheme))
  expect_error(object = makeObject(x = llist(input2), window = NULL, image = TRUE, theme = gtTheme))
})

test_that("make object from a Spatial", {
  output <- makeObject(x = list(gtSP$SpatialPolygons), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))

  # errors
})

test_that("make object from a sf", {
  output <- makeObject(x = list(gtSF$polygon), window = NULL, theme = gtTheme)
  expect_list(x = output, len = 7)
  expect_names(x = names(output), permutation.of = c("type", "name", "window", "out", "hasLegend", "params", "legend"))

  # errors
})
