context("format.oxcAARCalibratedDate")

result <- readOxcalOutput("ox_output.js")
this_date <- parseOxcalOutput(result)[[1]]
this_date$name <- "my_date"

test_that("format is delegated to format.oxcAARCalibratedDate on a oxcAARCalibratedDate", {
  expect_match(format(this_date), "BP")
  expect_match(format(this_date), "std")
  expect_match(format(this_date), "sigma")
  expect_match(format(this_date), "Atmospheric")
})

test_that("format.oxcAARCalibratedDate returns a character string", {
  expect_is(format(this_date),"character")
})

test_that("format.oxcAARCalibratedDate returns no output", {
  expect_silent(format(this_date))
})

context("print.oxcAARCalibratedDate")

test_that("print is delegated to print.oxcAARCalibratedDate on a oxcAARCalibratedDate", {
  expect_output(print(this_date), "BP")
})

# Dummy test for plots

context("plot.oxcAARCalibratedDate")

test_that("plot produces no error", {
  expect_error(plot( this_date), NA)
})

context("is.oxcAARCalibratedDate")

test_that("is.oxcAARCalibratedDate distinguishes between objects", {
  expect_true(is.oxcAARCalibratedDate(this_date))
  expect_false(is.oxcAARCalibratedDate(data.frame(test=NA)))
})

context("get_name")

test_that("get_name returns name", {
  expect_equal(get_name(this_date), this_date[['name']])
})

context("get_bp")

test_that("get_bp returns BP", {
  expect_equal(get_bp(this_date), this_date[['bp']])
})

context("get_std")

test_that("get_std returns std", {
  expect_equal(get_std(this_date), this_date[['std']])
})

context("get_cal_curve")

test_that("get_cal_curve returns std", {
  expect_equal(get_cal_curve(this_date), this_date[['cal_curve']])
})

context("get_sigma_ranges")

test_that("get_sigma_ranges returns sigma_ranges", {
  expect_equal(get_sigma_ranges(this_date), this_date[['sigma_ranges']])
})

context("get_sigma_ranges")

test_that("get_raw_probabilities returns raw_probabilities", {
  expect_equal(get_raw_probabilities(this_date), this_date[['raw_probabilities']])
})
