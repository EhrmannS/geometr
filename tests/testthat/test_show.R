library(checkmate)
library(testthat)
context("show")


test_that("show works, less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 vertices")
})

test_that("show works, more than 9 attributes", {
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
  expect_true(output[4] == "attributes  (features)   af, asd, adf, addsa, aslk, ial, afasdsa")
})
