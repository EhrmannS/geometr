library(checkmate)
library(raster)
context("visualise")


test_that("visualise a Raster* object", {
  continuous <- gtRasters$continuous

  output <- visualise(raster = continuous)
  expect_class(output, "recordedplot")
})

test_that("visualise a matrix", {
  continuous <- raster::as.matrix(gtRasters$continuous)

  output <- visualise(raster = continuous)
  expect_class(output, "recordedplot")
})

test_that("visualise an image", {
  continuous <- gtRasters$continuous
  input <- RGB(continuous)

  output <- visualise(raster = input, image = TRUE)
  expect_class(output, "recordedplot")
})

test_that("visualise a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  input <- gs_polygon(anchor = coords)

  output <- visualise(geom = input)
  expect_class(output, "recordedplot")
})

test_that("visualise an object with NA values", {
  # continuous <- gtRasters$continuous
  # get_patches <- list(list(operator = "rBinarise", thresh = 30),
  #                     list(operator = "rPatches"))
  # myPatches <- modify(input = continuous, by = get_patches, sequential = TRUE)
  #
  # output <- visualise(raster = myPatches)
  # expect_class(output, "recordedplot")
})

test_that("visualise a geom on top of an already plotted raster", {
  continuous <- gtRasters$continuous
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  input <- gs_polygon(anchor = coords)
  visualise(raster = continuous)

  output <- visualise(geom = input, new = FALSE)
  expect_class(output, "recordedplot")
})

test_that("output the history of a plotted object", {
  # continuous <- gtRasters$continuous
  # getBGPatches <- list(background = list(operator = "rBinarise", thresh = 30),
  #                      background = list(operator = "rPatches"),
  #                      background = list(operator = "rSegregate", background = 0),
  #                      background = list(operator = "rBinarise", thresh = 1),
  #                      background = list(operator = "rPermute"),
  #                      background = list(operator = "rPatches"),
  #                      background = list(operator = "rReduce", fun = max),
  #                      background = list(operator = "rFillNA"))
  # backgroundPatches <- modify(input = continuous, by = getBGPatches)
  #
  # output <- capture_message(visualise(raster = backgroundPatches, trace = TRUE))
  # expect_class(output, "simpleMessage")
  #
  # anAlgo <- list(background = list(operator = "rBinarise", thresh = 30),
  #                background = list(operator = "rPatches"),
  #                background = list(operator = "rSegregate", background = 0),
  #                background = list(operator = "rBinarise", thresh = 1))
  # segregated <- modify(input = continuous, by = anAlgo)
  # output <- capture_message(visualise(raster = segregated, trace = TRUE))
  # expect_class(output, "simpleMessage")
})

test_that("Error if arguments have wrong value", {
  continuous <- gtRasters$continuous
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords)
  # anImage <- system.file()

  expect_error(visualise())
  expect_error(visualise(raster = "bla"))
  expect_error(visualise(raster = continuous, geom = "bla"))
  expect_error(visualise(raster = continuous, theme = "bla"))
  expect_error(visualise(raster = continuous, trace = 1))
  expect_error(visualise(raster = continuous, image = 0))
})
