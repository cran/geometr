library(checkmate)
library(testthat)
context("helpers")


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
  expect_data_frame(x = output, nrows = 2, ncols = 2)
  expect_true(max(gtGeoms$polygon@point$x) == max(output$x))
  expect_true(min(gtGeoms$polygon@point$x) == min(output$x))
  expect_true(max(gtGeoms$polygon@point$y) == max(output$y))
  expect_true(min(gtGeoms$polygon@point$y) == min(output$y))
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