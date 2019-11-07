library(checkmate)
library(testthat)
context("setPoints")


test_that("setPoints of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  attributes <- data.frame(fid = 1, data = "A")

  # set table with a known variable
  output <- setPoints(x = input, table = attributes)
  expect_class(output, "geom")
  expect_data_frame(output@point)
  expect_names(names(output@point), must.include = c("fid", "x", "y", "data"))

  # set table with only unknown variables
  output <- setPoints(x = input, table = data.frame(fid = 1, data = "B"))
  expect_class(output, "geom")
  expect_data_frame(output@point)
  expect_names(names(output@point), must.include = c("fid", "x", "y", "data"))
})

