context("onload")

test_that("onload for oxcAAR worked", {
  expect_true("oxcAAR.oxcal_path" %in% names(options()))
})
