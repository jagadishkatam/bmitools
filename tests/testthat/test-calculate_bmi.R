

test_that("BMI is calculated correctly", {
  result <- calculate_bmi(70,175)
  expect_equal(result$bmi, 22.9)
  expect_equal(result$category, "Normal Weight")
})


test_that("invalid values and error", {
  expect_error(calculate_bmi(70,0))
})
