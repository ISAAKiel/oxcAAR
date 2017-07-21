context("R_Date")

test_that("R_Date produces correct oxcal code", {
  expect_equal(R_Date("date", 4000, 10), "R_Date(\"date\", 4000, 10);")
}
         )

context("R_Simulate")

test_that("R_Simulate produces correct oxcal code", {
  expect_equal(R_Simulate(4000, 10), "R_Simulate(\"1\",\n          4000, 10);")
})


context("oxcal_Sum")

test_that("oxcal_Sum produces correct oxcal code", {
  expect_equal(oxcal_Sum(R_Simulate(4000, 10)), "Sum(\" Sum \"){\n R_Simulate(\"1\",\n          4000, 10); \n};")
})

context("parseFullOxcalOutput")

test_that("parseFullOxcalOutput parses oxcal output file correct", {
  result <- readOxcalOutput("ox_output.js")
  RVA <- oxcAAR::parseFullOxcalOutput(result)
  expect_equal(length(RVA), 4)
  })
