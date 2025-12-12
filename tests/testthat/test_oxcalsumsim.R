context("oxcalSumSim")

test_that("oxcalSumSim produces error given wrong oxcal result file", {
  testthat::with_mocked_bindings(
    {
      expect_error(
        suppressWarnings(
          oxcalSumSim(timeframe_begin = -1000, timeframe_end = -2000, n = 10, stds = 10)
        )
      )
    },
    executeOxcalScript = function(...) "ox_output_wrong.js",
    .package = "oxcAAR"
  )
})

testthat::with_mocked_bindings(
  {
    file.copy("ox_sum.js", "ox_sum_work.js", overwrite = TRUE)

    test_that("oxcalSumSim is processed without error given oxcal result file", {
      expect_error(
        suppressWarnings(
          oxcalSumSim(timeframe_begin = -1000, timeframe_end = -2000, n = 10, stds = 10)
        ),
        NA
      )
    })

    file.copy("ox_sum.js", "ox_sum_work.js", overwrite = TRUE)

    test_that("oxcalSumSim is processed without error given oxcal result file with uniform distribution", {
      expect_error(
        suppressWarnings(
          oxcalSumSim(
            timeframe_begin = -1000,
            timeframe_end   = -2000,
            n               = 10,
            stds            = 10,
            date_distribution = "uniform"
          )
        ),
        NA
      )
    })

    file.copy("ox_sum.js", "ox_sum_work.js", overwrite = TRUE)

    test_that("oxcalSumSim complains when given wrong amount of stds", {
      expect_error(
        oxcalSumSim(timeframe_begin = -1000, timeframe_end = -2000, n = 10, stds = c(10, 10)),
        "Please give either one stds"
      )
    })

    file.copy("ox_sum.js", "ox_sum_work.js", overwrite = TRUE)

    test_that("oxcalSumSim complains when given wrong distribution", {
      expect_error(
        oxcalSumSim(
          timeframe_begin = -1000,
          timeframe_end   = -2000,
          n               = 10,
          stds            = 10,
          date_distribution = "kaffeemühle"
        ),
        "should be one of"
      )
    })

    file.copy("ox_sum.js", "ox_sum_work.js", overwrite = TRUE)
    this_result <- oxcalSumSim(timeframe_begin = -1000, timeframe_end = -2000, n = 10, stds = 10)

    test_that("oxcalSumSim produces an oxcAARCalibratedDate", {
      expect_equal(class(this_result), "oxcAARCalibratedDate")
    })
  },
  executeOxcalScript = function(...) "ox_sum_work.js",
  .package = "oxcAAR"
)
