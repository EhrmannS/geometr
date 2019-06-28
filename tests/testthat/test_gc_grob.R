library(checkmate)
library(testthat)
context("gc_grob")


test_that("transform from geom to grob", {
  # test type == 'point'
  pointGrob <- gc_grob(input = gtGeoms$point, theme = gtTheme)

  expect_list(pointGrob)
  expect_names(names(pointGrob), permutation.of = c("x", "y", "pch", "size", "name", "gp", "vp"))
  expect_class(pointGrob, classes = c("points", "grob", "gDesc"))

  # test type == 'line'
  linesGrob <- gc_grob(input = gtGeoms$line, theme = gtTheme)

  expect_list(linesGrob)
  expect_names(names(linesGrob), permutation.of = c("x", "y", "id", "id.lengths", "arrow", "name", "gp", "vp"))
  expect_class(linesGrob, classes = c("polyline", "grob", "gDesc"))

  # test type == 'polygon'
  polyGrob <- gc_grob(input = gtGeoms$polygon, theme = gtTheme)

  expect_list(polyGrob)
  expect_names(names(polyGrob[[1]]), permutation.of = c("x", "y", "id", "id.lengths", "pathId", "pathId.lengths", "rule", "name", "gp", "vp"))
  expect_class(polyGrob, classes = "gList")
  expect_class(polyGrob[[1]], classes = c("pathgrob", "grob", "gDesc"))
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))

  expect_error(gc_grob(input = notAGeom, theme = gtTheme))
  expect_error(gc_grob(input = gtGeoms$polygon, theme = "bla"))
  expect_error(gc_grob(input = gtSF$polygon, theme = gtTheme))
  expect_error(gc_grob(input = gtSP$SpatialPolygons, theme = gtTheme))
})
