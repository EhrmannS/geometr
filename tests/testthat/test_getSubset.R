context("getSubset")


test_that("getSubset of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)

  # get a subset of the coords
  output <- getSubset(x = input, fid == 2, slot = "vert")
  expect_class(output, "geom")
  expect_true(dim(output@vert)[1] == 3)
  expect_true(dim(output@vert)[1] < dim(input@vert)[1])

  # get a subset of the attributes
  input <- setTable(x = input, table = data.frame(fid = c(1, 2), a = c("a", "b")))
  output <- getSubset(x = input, a == 'b', slot = "table")
  expect_class(output, "geom")
  expect_true(dim(output@vert)[1] == 3)
  expect_true(dim(output@vert)[1] < dim(input@vert)[1])
})

test_that("getSubset of a Spatial* object", {
  input <- gtSP$SpatialPointsDataFrame

  output <- getSubset(x = input, a == 2)
  expect_class(output, "SpatialPointsDataFrame")
  expect_true(length(output) == 1)
  expect_true(length(output) < length(input))
})

test_that("getSubset of an sf object", {
  input <- gtSF$point

  output <- getSubset(x = input, attr = "a == 2")
  expect_class(output, "sf")
  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
})
