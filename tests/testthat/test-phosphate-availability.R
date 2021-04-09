test_that("calc_phosphate_availability works", {
  expect_equal(
    calc_phosphate_availability(
      A_P_PAL = 25,
      A_P_PAE = 1.5,
      A_P_WA = 24,
      B_LU_BRP = 2014
    ),
    expected = 2.4,
    tolerance = 0.001
  )
  expect_equal(
    calc_phosphate_availability(
      A_P_PAL = c(25, 20),
      A_P_PAE = c(1.5, 3),
      A_P_WA = c(24,20),
      B_LU_BRP = c(2014, 265)
    ),
    expected = c(2.4, 4.5713),
    tolerance = 0.001
  )
})

test_that("eval_phosphate_availability works", {
  expect_equal(
    ind_phosphate_availability(
      D_PBI = 2
    ),
    expected = 0.3804136,
    tolerance = 0.001
  )
  expect_equal(
    ind_phosphate_availability(
      D_PBI = 0:7
    ),
    expected = c(0.004930119, 0.074904299, 0.380413642, 0.742746233, 0.919277030, 0.977081441, 0.993682642, 0.998272957),
    tolerance = 0.001
  )
})
