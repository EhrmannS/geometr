library(checkmate)
library(testthat)
context("length")


test_that("length of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- gs_polygon(anchor = coords, window = window)
  output <- length(x = aGeom)
  expect_integerish(output, len = 1, any.missing = FALSE)
})


