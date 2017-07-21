context("setOxcalExecutablePath")

test_that("setOxcalExecutablePath sets the oxcal path in the environment variable",{
  setOxcalExecutablePath("ox_output.js")
  expect_true(options("oxcAAR.oxcal_path")=="ox_output.js")
})

context("quickSetupOxcal")
test_that("quickSetupOxcal downloads oxcal and sets correct path",{
  expect_error(quickSetupOxcal(), NA)
  expect_true(dir.exists("OxCal/bin/"))
  expect_true(basename(options("oxcAAR.oxcal_path")[[1]]) %in% c("OxCalLinux",
                                                                 "OxCalWin.exe",
                                                                 "OxCalMac"))
})
