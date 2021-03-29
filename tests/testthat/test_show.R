library(checkmate)
library(testthat)
context("show")


test_that("geom with less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 groups | 2 features | 11 points")
})

test_that("geom with all attribute tables", {
  input <- setFeatures(x = gtGeoms$polygon, table = data.frame(fid = c(1, 2), wat = c("bla", "blubb")))
  input <- setGroups(x = input, table = data.frame(gid = c(1, 2), blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 11)
  expect_true(output[2] == "            2 groups | 2 features | 11 points")
  expect_true(output[5] == "            (groups) blubb")
  expect_true(output[4] == "attributes  (features) wat")
})

test_that("geom with single attribute tables", {
  input <- setFeatures(x = gtGeoms$polygon, table = data.frame(fid = c(1, 2), bla = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 groups | 2 features | 11 points")

  input <- setFeatures(x = gtGeoms$polygon, table = data.frame(fid = c(1, 2), wat = c("bla", "blubb")))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 groups | 2 features | 11 points")

  input <- setGroups(x = gtGeoms$polygon, table = data.frame(gid = c(1, 2), blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 groups | 2 features | 11 points")
})

test_that("geom with crs", {
  input <- setCRS(x = gtGeoms$polygon, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[3] == "crs         +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
})

test_that("grid geom", {
  output <- capture.output(gtGeoms$grid$categorical)
  expect_character(x = output, len = 6)
  expect_true(output[2] == "            1 layer | 3360 cells")
  expect_true(output[6] == "extent      0 60 0 56 (xmin, xmax, ymin, ymax)")
})

test_that("more than 9 attributes", {
  attr <- getFeatures(gtGeoms$polygon)
  n <- dim(attr)[1]
  newAttributes <- data.frame(fid = 1:2,
                              af = sample(letters, n),
                              asd = sample(letters, n),
                              adf = sample(letters, n),
                              addsa = sample(letters, n),
                              aslk = sample(letters, n),
                              ial = sample(letters, n),
                              afasdsa = sample(letters, n))
  temp <- setFeatures(x = gtGeoms$polygon, newAttributes)
  output <- capture.output(temp)

  expect_character(x = output, len = 10)
  expect_true(output[4] == "attributes  (features) af, asd, adf, addsa, aslk, ial, afasdsa")
})
