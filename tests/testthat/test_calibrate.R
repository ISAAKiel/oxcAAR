with_mock(
  `executeOxcalScript`= function(...) {
    return("ox_output.js");
  }, {
    expect_error( suppressWarnings(oxcalCalibrate(5000,25,"KIA-12345")))
  })

