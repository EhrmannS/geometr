library(checkmate)
library(testthat)
context("show")


test_that("geom with less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 vertices")
})

test_that("geom with all attribute tables", {
  input <- setTable(x = gtGeoms$polygon,
                     slot = "feat",
                     table = data.frame(bla = c(1:2)))
  input <- setTable(x = input,
                     slot = "vert",
                     table = data.frame(wat = c(1:15)))
  input <- setTable(x = input,
                     slot = "group",
                     table = data.frame(blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 12)
  expect_true(output[2] == "            2 features | 15 vertices")
})

test_that("geom with single attribute tables", {
  input <- setTable(x = gtGeoms$polygon,
                    slot = "feat",
                    table = data.frame(bla = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 vertices")

  input <- setTable(x = gtGeoms$polygon,
                    slot = "vert",
                    table = data.frame(wat = c(1:15)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 vertices")

  input <- setTable(x = gtGeoms$polygon,
                    slot = "group",
                    table = data.frame(blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 vertices")
})

test_that("geom with crs", {
  input <- setCRS(x = gtGeoms$polygon, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[3] == "crs         +proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
})

test_that("more than 9 attributes", {
  newAttributes <- data.frame(fid = 1:2,
                              af = sample(letters, length(gtGeoms$polygon@feat$fid)),
                              asd = sample(letters, length(gtGeoms$polygon@feat$fid)),
                              adf = sample(letters, length(gtGeoms$polygon@feat$fid)),
                              addsa = sample(letters, length(gtGeoms$polygon@feat$fid)),
                              aslk = sample(letters, length(gtGeoms$polygon@feat$fid)),
                              ial = sample(letters, length(gtGeoms$polygon@feat$fid)),
                              afasdsa = sample(letters, length(gtGeoms$polygon@feat$fid)))
  temp <- setTable(x = gtGeoms$polygon, newAttributes)
  output <- capture.output(temp)

  expect_character(x = output, len = 10)
  expect_true(output[4] == "attributes  (features) af, asd, adf, addsa, aslk, ial, afasdsa")
})
