library(checkmate)
library(testthat)
library(raster)
context("getWindow")


test_that("getWindow of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 2, 1, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_point(anchor = coords, window = window)
  output <- getWindow(aGeom)

  expect_data_frame(output, any.missing = FALSE, nrows = 4, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})
