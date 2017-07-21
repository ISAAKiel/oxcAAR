result <- readOxcalOutput("ox_output.js")
this_date_list <- parseOxcalOutput(result)
this_date_list[[1]]$name <- "date"

context("print.oxcAARCalibratedDatesList")

test_that("print is delegated to print.oxcAARCalibratedDatesList on a oxcAARCalibratedDatesList", {
  expect_output(print(this_date_list), "BP")
})

# Dummy test for plots

context("plot.oxcAARCalibratedDatesList")

this_multiple_dates_list <- this_date_list
this_multiple_dates_list[[2]] <- this_date_list[[1]]
names(this_multiple_dates_list) <- c("date1", "date2")

test_that("plot produces no error", {
  expect_error(plot( this_multiple_dates_list), NA)
})
