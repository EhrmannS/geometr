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
  aPoint <- setWindow(x = aPoint, to = data.frame(x = c(3, 5), y = c(3, 5)))
  output <- makeLayout(x = aPoint, theme = gtTheme)
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