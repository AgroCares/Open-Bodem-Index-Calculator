test_that("calc_k_availability works", {
  expect_equal(
    calc_k_availability(
      A_PH_CC = 5.6,
      A_OS_GV = 7.9,
      A_CEC_CO = 122, 
      A_K_CC = 95,
      A_K_CEC = 1.56,
      A_CLAY_MI = 1.73,
      B_BT_AK = 'dekzand',
      B_LU_BRP = 3732
    ),
    expected = 0.2662949,
    tolerance = 0.001
  )
  expect_equal(
    ind_resistance(
      A_OS_GV = 0:8
    ),
    expected = c(0.004491325, 0.049927928, 0.266294851, 0.620746792, 0.857766085, 0.953887800, 0.985788749, 0.995689634, 0.998698998),
    tolerance = 0.001
  )
})