library(checkmate)
library(testthat)
context("show")


test_that("geom with less than 9 attributes", {
  output <- capture.output(gtGeoms$polygon)

  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")
})

test_that("geom with all attribute tables", {
  input <- setTable(x = gtGeoms$polygon,
                     slot = "feature",
                     table = data.frame(bla = c(1:2)))
  input <- setTable(x = input,
                     slot = "point",
                     table = data.frame(wat = c(1:15)))
  input <- setTable(x = input,
                     slot = "group",
                     table = data.frame(blubb = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 12)
  expect_true(output[2] == "            2 features | 15 points")
})

test_that("geom with single attribute tables", {
  input <- setTable(x = gtGeoms$polygon,
                    slot = "feature",
                    table = data.frame(bla = c(1:2)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")

  input <- setTable(x = gtGeoms$polygon,
                    slot = "point",
                    table = data.frame(wat = c(1:15)))
  output <- capture.output(input)
  expect_character(x = output, len = 10)
  expect_true(output[2] == "            2 features | 15 points")

  input <- setTable(x = gtGeoms$polygon,
                    slot = "group",
                    table = data.frame(blubb = c(1:2)))
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
  newAttributes <- data.frame(fid = 1:2,
                              af = sample(letters, length(gtGeoms$polygon@feature$fid)),
                              asd = sample(letters, length(gtGeoms$polygon@feature$fid)),
                              adf = sample(letters, length(gtGeoms$polygon@feature$fid)),
                              addsa = sample(letters, length(gtGeoms$polygon@feature$fid)),
                              aslk = sample(letters, length(gtGeoms$polygon@feature$fid)),
                              ial = sample(letters, length(gtGeoms$polygon@feature$fid)),
                              afasdsa = sample(letters, length(gtGeoms$polygon@feature$fid)))
  temp <- setTable(x = gtGeoms$polygon, newAttributes)
  output <- capture.output(temp)

  expect_character(x = output, len = 10)
  expect_true(output[4] == "attributes  (features) af, asd, adf, addsa, aslk, ial, afasdsa")
})