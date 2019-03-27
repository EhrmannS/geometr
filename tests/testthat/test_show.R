library(checkmate)
library(testthat)
context("show")


test_that("show works, less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 8)
  expect_true(output[8] == "attributes : 2  (fid, gid)")
})

test_that("show works, more than 9 attributes", {
  newAttributes <- data.frame(fid = 1:2,
                              af = sample(letters, length(gtGeoms$polygon@attr$fid)),
                              asd = sample(letters, length(gtGeoms$polygon@attr$fid)),
                              adf = sample(letters, length(gtGeoms$polygon@attr$fid)),
                              addsa = sample(letters, length(gtGeoms$polygon@attr$fid)),
                              aslk = sample(letters, length(gtGeoms$polygon@attr$fid)),
                              ial = sample(letters, length(gtGeoms$polygon@attr$fid)),
                              afasdsa = sample(letters, length(gtGeoms$polygon@attr$fid)))
  temp <- setTable(x = gtGeoms$polygon, newAttributes)
  output <- capture.output(temp)

  expect_character(x = output, len = 8)
  expect_true(output[8] == "attributes : 9  (fid, gid, af, asd, adf, addsa, aslk, ial, afasdsa, ...)")
})
