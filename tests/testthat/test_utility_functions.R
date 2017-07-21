context("setOxcalExecutablePath")

test_that("setOxcalExecutablePath sets the oxcal path in the environment variable",{
  setOxcalExecutablePath("ox_output.js")
  expect_true(options("oxcAAR.oxcal_path")=="ox_output.js")
})
