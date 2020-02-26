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
  expect_names(names(polyGrob), permutation.of = c("x", "y", "id", "id.lengths", "pathId", "pathId.lengths", "rule", "name", "gp", "vp"))
  expect_class(polyGrob, classes = c("pathgrob", "grob", "gDesc"))
})

test_that("quick options work", {
  aTheme <- setTheme(vector = list(linewidth = c(1, 3),
                                   pointsize = c(1, 3),
                                   pointsymbol = c(0:12),
                                   linetype = c(1, 2)))

  # linecol
  polyGrob <- gc_grob(input = gtGeoms$polygon, theme = aTheme, linecol = "fid")

  expect_true(polyGrob$gp$col[[1]] == "#00204D")
  expect_true(is.na(polyGrob$gp$fill[[1]]))
  expect_true(polyGrob$gp$lty[[1]] == 1)
  expect_true(polyGrob$gp$lwd[[1]] == 1)

  # fillcol
  polyGrob <- gc_grob(input = gtGeoms$polygon, theme = aTheme, fillcol = "fid")

  expect_true(polyGrob$gp$col[[1]] == "#00204DFF")
  expect_true(polyGrob$gp$fill[[1]] == "#00204D")
  expect_true(polyGrob$gp$lty[[1]] == 1)
  expect_true(polyGrob$gp$lwd[[1]] == 1)

  # linewidth
  polyGrob <- gc_grob(input = gtGeoms$polygon, theme = aTheme, linewidth = "fid")

  expect_true(polyGrob$gp$col[[1]] == "#00204DFF")
  expect_true(is.na(polyGrob$gp$fill[[1]]))
  expect_true(polyGrob$gp$lty[[1]] == 1)
  expect_true(polyGrob$gp$lwd[[1]] == 1)

  # pointsize
  pointGrob <- gc_grob(input = gtGeoms$point, theme = aTheme, pointsize = "fid")

  expect_true(pointGrob$gp$col[1] == "#00204DFF")
  expect_true(is.na(pointGrob$gp$fill))
  expect_true(as.numeric(pointGrob$size[1]) == 1)
  expect_true(pointGrob$pch[1] == 0)

  # pointsymbol
  pointGrob <- gc_grob(input = gtGeoms$point, theme = aTheme, pointsymbol = "fid")

  expect_true(pointGrob$gp$col[1] == "#00204DFF")
  expect_true(is.na(pointGrob$gp$fill))
  expect_true(as.numeric(pointGrob$size[1]) == 1)
  expect_true(pointGrob$pch[1] == 0)

  # linetype
  polyGrob <- gc_grob(input = gtGeoms$polygon, theme = aTheme, linetype = "fid")

  expect_true(polyGrob$gp$col[[1]] == "#00204DFF")
  expect_true(is.na(polyGrob$gp$fill[[1]]))
  expect_true(polyGrob$gp$lty[[1]] == 1)
  expect_true(polyGrob$gp$lwd[[1]] == 1)

})

test_that("correct warnings are printed", {

  emptyTheme <- setTheme(vector = list(linecol = "#00204DFF", fillcol = "#00204DFF"))

  expect_warning(object = gc_grob(input = gtGeoms$polygon, theme = emptyTheme, linecol = "fid"),
                 regexp = "please provide a theme with at least two values for 'linecol' to make a color gradient between.")
  expect_warning(object = gc_grob(input = gtGeoms$polygon, theme = emptyTheme, fillcol = "fid"),
                 regexp = "please provide a theme with at least two values for 'fillcol' to make a color gradient between.")
  expect_warning(object = gc_grob(input = gtGeoms$polygon, theme = emptyTheme, linewidth = "fid"),
                 regexp = "please provide a theme with at least two values for 'linewidth' to scale between.")
  expect_warning(object = gc_grob(input = gtGeoms$polygon, theme = emptyTheme, pointsize = "fid"),
                 regexp = "please provide a theme with at least two values for 'pointsize' to scale between.")
  expect_warning(object = gc_grob(input = gtGeoms$polygon, theme = emptyTheme, pointsymbol = "fid"),
                 regexp = "please provide a theme with 2 values for the unique values of 'pointsymbol'.")
  expect_warning(object = gc_grob(input = gtGeoms$polygon, theme = emptyTheme, linetype = "fid"),
                 regexp = "please provide a theme with 2 values for the unique values of 'linetype'.")

})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))

  invalidTheme <- setTheme(scale = list(param = "linecol", to = NA))

  expect_error(gc_grob(input = notAGeom, theme = gtTheme))
  expect_error(gc_grob(input = gtGeoms$polygon, theme = invalidTheme))
  expect_error(gc_grob(input = gtGeoms$polygon, theme = "bla"))
  expect_error(gc_grob(input = gtSF$polygon, theme = gtTheme))
  expect_error(gc_grob(input = gtSP$SpatialPolygons, theme = gtTheme))
})
