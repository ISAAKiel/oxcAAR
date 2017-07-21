result <- readOxcalOutput("ox_output.js")
this_date_list <- parseOxcalOutput(result)

context("print.oxcAARCalibratedDatesList")

test_that("print is delegated to print.oxcAARCalibratedDatesList on a oxcAARCalibratedDatesList", {
  expect_output(print(this_date_list), "BP")
})

# Dummy test for plots

context("plot.oxcAARCalibratedDatesList")

test_that("plot produces no error", {
  expect_error(plot( this_date_list), NA)
})
