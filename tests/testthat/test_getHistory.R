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
