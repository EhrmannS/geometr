library(checkmate)
context("gt_rotate")


test_that("output is valid geometry", {
  # geom with one features
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_rotate(geom = input, angle = 45, about = c(50, 30))

  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 5, ncols = 3)

  # rotate one out of two features
  coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                       y = c(40, 40, 60, 70, 10, 20, 40),
                       fid = c(1, 1, 1, 1, 2, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_rotate(geom = input,
                      angle = 90,
                      about = c(40, 40),
                      fid = 2)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)

  # rotate two out of two features
  coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                       y = c(40, 40, 60, 70, 10, 20, 40),
                       fid = c(1, 1, 1, 1, 2, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_rotate(geom = input,
                      angle = list(90, -180),
                      about = list(c(40, 40), c(30, 40)))
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@point, any.missing = FALSE, nrows = 9, ncols = 3)
})

test_that("output has different coordinates than input", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  rotGeom <- gt_rotate(geom = input, angle = 45, about = c(50, 30))

  expect_false(all(getPoints(input)[c(1, 2)] == getPoints(rotGeom)[c(1, 2)]))
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)

  expect_error(gt_rotate(geom = "bla"))
  expect_error(gt_rotate(geom = input))
  expect_error(gt_rotate(geom = input, angle = "bla"))
  expect_error(gt_rotate(geom = notAGeom, angle = 45))
  expect_error(gt_rotate(geom = input, angle = 45, about = "bla"))
})
