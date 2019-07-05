test_that("eval_phosphate_availability works", {
  expect_equal(
    eval_resistance(
      om = 2
    ),
    expected = 0.2662949,
    tolerance = 0.001
  )
  expect_equal(
    eval_resistance(
      om = 0:8
    ),
    expected = c(0.004491325, 0.049927928, 0.266294851, 0.620746792, 0.857766085, 0.953887800, 0.985788749, 0.995689634, 0.998698998),
    tolerance = 0.001
  )
})