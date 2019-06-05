library(checkmate)
library(sf)
context("getVertices")


test_that("getVertices of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)

  output <- getVertices(x = input)
  expect_data_frame(output, any.missing = FALSE, nrows = 5, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))
})

test_that("getVertices of a Spatial* object", {
  input1 <- gtSP$SpatialPoints
  input2 <- gtSP$SpatialPolygons

  # point should have as many coordinates as points
  output <- getVertices(x = input1)
  expect_data_frame(output, any.missing = FALSE, nrows = 4, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))

  # polygon should have one point duplicated, hence, 5 times as many points as features
  output <- getVertices(x = input2)
  expect_data_frame(output, any.missing = FALSE, nrows = 15, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))
})

test_that("getVertices of an sf object", {
  input <- gtSF$polygon

  output <- getVertices(x = input)
  expect_data_frame(output, any.missing = FALSE, nrows = 15, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "fid"))
})

test_that("Error if arguments have wrong value", {
  input <- st_sf(st_sfc(st_geometrycollection(list(st_point(1:2))),
                        st_geometrycollection(list(st_linestring(matrix(1:4,2))))))

  expect_error(getVertices(x = input))
})

