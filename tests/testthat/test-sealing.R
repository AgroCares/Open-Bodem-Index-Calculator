test_that("calc_sealing works", {
  expect_equal(
    calc_sealing_risk(
      A_CLAY_MI = 20, 
      A_OS_GV = 5
    ),
    expected = 8.4375,
    tolerance = 0.001
  )
  expect_equal(
    calc_sealing_risk(
      A_CLAY_MI = c(25, 20),
      A_OS_GV = c(1.5, 8)
    ),
    expected = c(8.6, 10),
    tolerance = 0.001
  )
})

test_that("ind_sealing works", {
  expect_equal(
    ind_sealing(
      D_SE = 8.6, 
      B_LU_BRP = 2014
    ),
    expected = 0.9609441,
    tolerance = 0.001
  )
  expect_equal(
    ind_sealing(
      D_SE = c(8.6, 2),
      B_LU_BRP = c(2014, 265)
    ),
    expected = c(0.9609441, 1),
    tolerance = 0.001
  )
})