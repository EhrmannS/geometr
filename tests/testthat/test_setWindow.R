library(checkmate)
library(testthat)
library(raster)
context("setWindow")


test_that("setWindow of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  window2 <- data.frame(x = c(0, 0, 80, 80),
                        y = c(0, 80, 80, 0))
  input <- gs_polygon(anchor = coords)
  
  # test short notation
  output <- setWindow(input, window)
  expect_class(output, "geom")
  expect_data_frame(getWindow(output))
  expect_equal(dim(getWindow(output)),  c(4, 2))
  
  # test long notation
  output <- setWindow(input, window2)
  expect_class(output, "geom")
  expect_data_frame(getWindow(output))
  expect_equal(dim(getWindow(output)),  c(4, 2))
})

