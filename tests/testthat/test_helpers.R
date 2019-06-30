library(checkmate)
library(testthat)
context("helpers")


test_that("makeLayout for dimensionless point", {
  aPoint <- gs_point(anchor = data.frame(x = 5, y = 5))
  output <- makeLayout(x = aPoint, theme = gtTheme)
  expect_list(output, len = 23, any.missing = FALSE)
  expect_names(x = names(output), identical.to = c("minWinX", "maxWinX", "minWinY", "maxWinY", "xMajGrid", "xMinGrid", "yMajGrid", "yMinGrid", "xMargin", "yMargin", "xOffset", "yOffset", "xFactor", "yFactor", "gridH", "gridHr", "gridW", "gridWr", "titleH", "yAxisTicksW", "xAxisTitleH", "xWindowOffset", "yWindowOffset"))
})

test_that("makeLayout when 'window' is given", {
  aPoint <- gs_point(anchor = data.frame(x = 5, y = 5))
  output <- makeLayout(x = aPoint, window = data.frame(x = c(3, 5), y = c(3, 5)), theme = gtTheme)
  expect_list(output, len = 23, any.missing = FALSE)
  expect_names(x = names(output), identical.to = c("minWinX", "maxWinX", "minWinY", "maxWinY", "xMajGrid", "xMinGrid", "yMajGrid", "yMinGrid", "xMargin", "yMargin", "xOffset", "yOffset", "xFactor", "yFactor", "gridH", "gridHr", "gridW", "gridWr", "titleH", "yAxisTicksW", "xAxisTitleH", "xWindowOffset", "yWindowOffset"))
})

test_that("makeLayout for deviating theme options", {
  myTheme <- gtTheme
  myTheme@title$plot <- FALSE
  myTheme@legend$plot <- FALSE
  myTheme@yAxis$plot <- FALSE
  myTheme@xAxis$plot <- FALSE

  output <- makeLayout(x = gtGeoms$polygon, theme = myTheme)
  expect_list(output, len = 23, any.missing = FALSE)
  expect_names(x = names(output), identical.to = c("minWinX", "maxWinX", "minWinY", "maxWinY", "xMajGrid", "xMinGrid", "yMajGrid", "yMinGrid", "xMargin", "yMargin", "xOffset", "yOffset", "xFactor", "yFactor", "gridH", "gridHr", "gridW", "gridWr", "titleH", "yAxisTicksW", "xAxisTitleH", "xWindowOffset", "yWindowOffset"))
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
  aWindow <- data.frame(x = c(3, 5), y = c(3, 5))
  output <- .updateWindow(geom = gtGeoms$polygon@vert,
                          window = aWindow)
  expect_data_frame(x = output, nrows = 2, ncols = 2)
  expect_true(all(aWindow != output))
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