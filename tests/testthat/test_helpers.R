library(checkmate)
library(testthat)
context("helpers")


test_that("makeLayout when 'window' is given", {
  input <- makeObject(x = list(gtGeoms$point), window = data.frame(x = c(3, 5), y = c(3, 5)), theme = gtTheme)
  output <- makeLayout(x = input, theme = gtTheme)
  expect_list(output, len = 22, any.missing = FALSE)
  expect_names(x = names(output), identical.to = c("minPlotX", "maxPlotX", "minPlotY", "maxPlotY", "xMajGrid", "xMinGrid", "yMajGrid", "yMinGrid", "xMargin", "yMargin", "xOffset", "yOffset", "xFactor", "yFactor", "gridH", "gridW", "titleH", "yAxisTicksW", "xAxisTitleH", "xWindowOffset", "yWindowOffset", "legendX"))
})

test_that("makeLayout for deviating theme options", {
  myTheme <- gtTheme
  myTheme@title$plot <- FALSE
  myTheme@legend$plot <- FALSE
  myTheme@yAxis$plot <- FALSE
  myTheme@xAxis$plot <- FALSE

  input <- makeObject(x = list(gtGeoms$point), window = NULL, theme = myTheme)
  output <- makeLayout(x = input, theme = myTheme)
  expect_list(output, len = 22, any.missing = FALSE)
  expect_names(x = names(output), identical.to = c("minPlotX", "maxPlotX", "minPlotY", "maxPlotY", "xMajGrid", "xMinGrid", "yMajGrid", "yMinGrid", "xMargin", "yMargin", "xOffset", "yOffset", "xFactor", "yFactor", "gridH", "gridW", "titleH", "yAxisTicksW", "xAxisTitleH", "xWindowOffset", "yWindowOffset", "legendX"))
})

test_that(".getDecimals works", {
  output <- .getDecimals(x = 1.52)
  expect_numeric(x = output, len = 1, any.missing = FALSE)
  expect_true(object = output == 2)

  output <- .getDecimals(x = 1)
  expect_numeric(x = output, len = 1, any.missing = FALSE)
  expect_true(object = output == 0)
})

test_that(".rad works", {
  output <- .rad(degree = 180)
  expect_numeric(x = output, len = 1, any.missing = FALSE)
  expect_true(output == pi)
})

test_that(".updateWindow works", {
  aWindow <- data.frame(x = c(-1, 15), y = c(-1, 15))
  output <- .updateWindow(input = gtGeoms$polygon@point,
                          window = aWindow)
  expect_data_frame(x = output, nrows = 5, ncols = 2)
  expect_true(max(gtGeoms$polygon@window$x) == max(output$x))
  expect_true(min(gtGeoms$polygon@window$x) == min(output$x))
  expect_true(max(gtGeoms$polygon@window$y) == max(output$y))
  expect_true(min(gtGeoms$polygon@window$y) == min(output$y))
})

test_that(".testAnchor works", {
  output <- capture_messages(.testAnchor(x = "bla", verbose = TRUE))
  expect_true(object = output == "'anchor' is neither a data.frame nor a geom.\n")
})

test_that(".testWindow works", {
  output <- capture_messages(.testWindow(x = "bla", verbose = TRUE))
  expect_true(object = output == "'window' is not a data.frame.\n")
})

test_that(".testTemplate works", {
  output <- capture_messages(.testTemplate(x = "bla", verbose = TRUE))
  expect_true(object = output == "'template' is neither a RasterLayer nor a matrix.\n")
})