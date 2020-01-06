library(checkmate)
library(testthat)
library(raster)
context("getHistory")


test_that("getHistory of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)
  output <- getHistory(aGeom)

  expect_list(output, any.missing = FALSE, types = "character")
  expect_true(output[[1]] == "object was created as 'polygon' geom.")
})

test_that("getHistory of a RasterLayer", {
  input <- gtRasters$categorical
  input@history <- list("bla")

  output <- getHistory(input)
  expect_list(output, len = 1, types = "character")
})

test_that("getHistory of a RasteBrick", {
  # seems like I don't have a brick within this package, so I create a random one
  input <- brick(gtRasters)
  input@history <- list("bla")

  output <- getHistory(input)
  expect_list(output, len = 1, types = "character")
})

test_that("getHistory of a RasteStack", {
  input <- stack(gtRasters)
  input@history <- list("bla")

  output <- getHistory(input)
  expect_list(output, len = 2, types = "character")
})

test_that("getHistory of any other object", {
  output <- getHistory("bla")
  expect_null(object = output)
})
