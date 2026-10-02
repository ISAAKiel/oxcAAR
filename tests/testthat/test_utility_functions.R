context("getOxcalExecutablePath")

test_that("getOxcalExecutablePath returns error if not set already", {
  old_path <- getOption("oxcAAR.oxcal_path")
  on.exit(options(oxcAAR.oxcal_path = old_path), add = TRUE)

  options(oxcAAR.oxcal_path = NULL)

  expect_error(
    oxcAAR:::getOxcalExecutablePath(),
    "Please set path to oxcal first"
  )
})


context("setOxcalExecutablePath")

test_that("setOxcalExecutablePath sets the OxCal path", {
  old_path <- getOption("oxcAAR.oxcal_path")
  on.exit(options(oxcAAR.oxcal_path = old_path), add = TRUE)

  setOxcalExecutablePath("ox_output.js")

  expect_equal(
    getOption("oxcAAR.oxcal_path"),
    "ox_output.js"
  )
})

test_that("setOxcalExecutablePath complains when file does not exist", {
  expect_error(
    setOxcalExecutablePath("i_am_not_here.file"),
    "No file at given location"
  )
})


context("getOxcalExecutablePath")

test_that("getOxcalExecutablePath returns a configured path", {
  old_path <- getOption("oxcAAR.oxcal_path")
  on.exit(options(oxcAAR.oxcal_path = old_path), add = TRUE)

  options(oxcAAR.oxcal_path = "ox_output.js")

  expect_error(
    oxcAAR:::getOxcalExecutablePath(),
    NA
  )

  expect_equal(
    oxcAAR:::getOxcalExecutablePath(),
    "ox_output.js"
  )
})


context("quickSetupOxcal")

test_that("OxCal download version is validated", {
  expect_error(
    oxcAAR:::.oxcal_download_url(""),
    "'version' must be a single non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    oxcAAR:::.oxcal_download_url(NA_character_),
    "'version' must be a single non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    oxcAAR:::.oxcal_download_url(c("4.4.4", "4.3.2")),
    "'version' must be a single non-empty character string.",
    fixed = TRUE
  )

  expect_error(
    oxcAAR:::.oxcal_download_url(4.4),
    "'version' must be a single non-empty character string.",
    fixed = TRUE
  )
})

test_that("latest OxCal download URL is constructed correctly", {
  expect_equal(
    oxcAAR:::.oxcal_download_url("latest"),
    "https://c14.arch.ox.ac.uk/OxCalDistribution.zip"
  )
})

test_that("fixed OxCal download URLs are constructed correctly", {
  expect_equal(
    oxcAAR:::.oxcal_download_url("4.4.4"),
    "https://c14.arch.ox.ac.uk/OxCal_4_4_4.zip"
  )

  expect_equal(
    oxcAAR:::.oxcal_download_url("4.3.1"),
    "https://c14.arch.ox.ac.uk/OxCal_4_3_1.zip"
  )
})

test_that("OxCal 4.3.2 uses its exceptional archive name", {
  expect_equal(
    oxcAAR:::.oxcal_download_url("4.3.2"),
    "https://c14.arch.ox.ac.uk/OxCal_4_3_2_orig.zip"
  )
})

test_that("tested OxCal versions are reported", {
  expect_equal(
    testedOxcalVersions(),
    c(
      "4.4.4",
      "4.4.3",
      "4.4.2",
      "4.4.1",
      "4.3.2",
      "4.3.1",
      "4.2.4"
    )
  )
})

test_that("quickSetupOxcal downloads OxCal and sets correct path", {
  skip_on_cran()

  old_path <- getOption("oxcAAR.oxcal_path")
  oxcal_dir <- file.path(tempdir(), "OxCal")

  on.exit(options(oxcAAR.oxcal_path = old_path), add = TRUE)
  on.exit(unlink(oxcal_dir, recursive = TRUE), add = TRUE)

  options(oxcAAR.oxcal_path = NULL)
  unlink(oxcal_dir, recursive = TRUE)

  expect_error(
    quickSetupOxcal(),
    NA
  )

  expect_true(
    dir.exists(file.path(oxcal_dir, "bin"))
  )

  expect_true(
    basename(getOption("oxcAAR.oxcal_path")) %in%
      c("OxCalLinux", "OxCalWin.exe", "OxCalMac")
  )
})


context("formatDateAdBc")

test_that("formatDateAdBc can handle NAs", {
  expect_error(
    oxcAAR:::formatDateAdBc(NA),
    NA
  )

  expect_equal(
    oxcAAR:::formatDateAdBc(NA),
    "NA"
  )
})


precise_sigma_range <- data.frame(
  start = -3956.5,
  end = -3643,
  probability = 99.73002
)


context("formatFullSigmaRange")

test_that("formatFullSigmaRange should have a precision of 2", {
  sigma_text <- oxcAAR:::formatFullSigmaRange(
    precise_sigma_range,
    "name"
  )

  sigma_precision <- stringr::str_extract_all(
    sigma_text,
    pattern = "(?<=\\().+?(?=%\\))"
  )[[1]]

  expect_equal(
    as.numeric(sigma_precision),
    round(as.numeric(sigma_precision), 2)
  )
})
