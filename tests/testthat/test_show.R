library(checkmate)
library(testthat)
context("show")


test_that("show works, less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 8)
  expect_true(output[8] == "attributes : 3  (fid, n, a)")
})

test_that("show works, more than 9 attributes", {
  newAttributes <- data.frame(fid = 1:15,
                              af = sample(letters, length(gtGeoms$polygon@coords$fid)),
                              asd = sample(letters, length(gtGeoms$polygon@coords$fid)),
                              adf = sample(letters, length(gtGeoms$polygon@coords$fid)),
                              addsa = sample(letters, length(gtGeoms$polygon@coords$fid)),
                              aslk = sample(letters, length(gtGeoms$polygon@coords$fid)),
                              ial = sample(letters, length(gtGeoms$polygon@coords$fid)),
                              afasdsa = sample(letters, length(gtGeoms$polygon@coords$fid)))
  temp <- setTable(x = gtGeoms$polygon, newAttributes)
  output <- capture.output(temp)

  expect_character(x = output, len = 8)
  expect_true(output[8] == "attributes : 10  (fid, n, a, af, asd, adf, addsa, aslk, ial, ...)")
})
