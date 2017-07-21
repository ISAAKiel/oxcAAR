context("R_Date")

testthat("R_Date produces correct oxcal code", {
  expect_equal(R_Date("date", 4000, 10), "R_Date(\"date\", 4000, 10);")
}
         )

context("R_Simulate")

testthat("R_Simulate produces correct oxcal code", {
  expect_equal(R_Simulate(4000, 10), "R_Simulate(\"1\",\n          4000, 10);")
})


context("oxcal_Sum")

testthat("oxcal_Sum produces correct oxcal code", {
  expect_equal(oxcal_Sum(R_Simulate(4000, 10)), "Sum(\" Sum \"){\n R_Simulate(\"1\",\n          4000, 10); \n};")
})
