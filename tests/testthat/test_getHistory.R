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
  expect_true(output[[1]] == "geometry was created as 'polygon'")
})

test_that("getHistory of a RasterLayer", {
  input <- rPatches(rBinarise(rtRasters$continuous, thresh = 40))

  output <- getHistory(input)
  expect_list(output, len = 3, types = "character")
})

test_that("getHistory of a RasteBrick", {
  # seems like I don't have a brick within this package, so I create a random one
  aBrick <- brick(system.file("external/rlogo.grd", package="raster"))
  aBrick@history <- list("bla")

  output <- getHistory(aBrick)
  expect_list(output, len = 1, types = "character")
})

test_that("getHistory of a RasteStack", {
  continuous <- rtRasters$continuous
  patches <- rPatches(rBinarise(continuous, thresh = 40))
  anAlgo <- list(background = list(operator = "rBinarise", thresh = 30),
                 background = list(operator = "rPatches"),
                 background = list(operator = "rSegregate", background = 0),
                 background = list(operator = "rBinarise", thresh = 1))
  input <- modify(input = continuous, by = anAlgo)

  output <- getHistory(input)
  expect_list(output, len = 140)
})
