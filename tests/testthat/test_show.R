library(checkmate)
library(testthat)
context("show")


test_that("geom with less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")
})

test_that("geom with all attribute tables", {
  input <- setFeatures(x = gtGeoms$polygon, table = data.frame(bla = c(1:2)))
  input <- setPoints(x = input, table = data.frame(wat = c(1:15)))
  input <- setGroups(x = input, table = data.frame(blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 12)
  expect_true(output[2] == "            2 features | 15 points")
})

test_that("geom with single attribute tables", {
  input <- setFeatures(x = gtGeoms$polygon, table = data.frame(bla = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")

  input <- setPoints(x = gtGeoms$polygon, table = data.frame(wat = c(1:15)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")

  input <- setGroups(x = gtGeoms$polygon, table = data.frame(blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")
})

test_that("geom with crs", {
  input <- setCRS(x = gtGeoms$polygon, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[3] == "crs         +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
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
