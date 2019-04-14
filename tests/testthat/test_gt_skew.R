context("gt_skew")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_skew(geom = input, x = list(0.5), y = list(0, 0.2))

  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(output@vert, any.missing = FALSE, nrows = 5, ncols = 4)
})

test_that("output has different coordinates than input", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)
  output <- gt_skew(geom = input, x = list(0.5), y = list(0, 0.2))

  expect_false(all(getVertices(input)[c(3, 4)] == getVertices(output)[c(3, 4)]))
})

test_that("Error if arguments have wrong value", {
  notinput <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- gs_polygon(anchor = coords, window = window)

  expect_error(gt_skew(geom = "bla"))
  expect_error(gt_skew(geom = input, x = "bla"))
  expect_error(gt_skew(geom = input, y = "bla"))
  expect_error(gt_skew(geom = input, fid = "bla"))
  expect_error(gt_skew(geom = notAGeom))
})
