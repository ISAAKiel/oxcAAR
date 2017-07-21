with_mock(
  `executeOxcalScript`= function(...) {
    return("ox_output_wrong.js");
  }, {
    expect_error( suppressWarnings(oxcalCalibrate(5000,25,"KIA-12345")))
  })

with_mock(
  `executeOxcalScript`= function(...) {
    return("ox_output.js");
  }, {
    expect_error( suppressWarnings(oxcalCalibrate(5000,25,"KIA-12345")), NA)
  })
