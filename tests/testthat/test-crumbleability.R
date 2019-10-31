test_that("calc_crumbleability works", {
  expect_equal(
    calc_crumbleability(
      A_CLAY_MI = 10, 
      A_OS_GV = 5, 
      A_PH_CC = 6
    ),
    expected = 9.3,
    tolerance = 0.001
  )
  expect_equal(
    calc_crumbleability(
      A_CLAY_MI = c(2, 4, 45), 
      A_OS_GV = c(5, 5, 5), 
      A_PH_CC= c(6, 6, 6)
    ), 
    expected = c(10, 10, 1),
    tolerance = 0.001
  )
})

test_that("eval_crumbleability works", {
  expect_equal(
    ind_crumbleability(
      D_CR = 10,
      B_LU_BRP = 2014
    ),
    expected = 1,
    tolerance = 0.001
  )
  expect_equal(
    ind_crumbleability(
      D_CR = seq(5,15,1),
      B_LU_BRP = rep(1042,11)
    ),
    expected = c(0,0,0,0,1,1,1,1,1,1,1),
    tolerance = 0.1
  )
  expect_equal(
    ind_crumbleability(
      D_CR = seq(1,11,1),
      B_LU_BRP = rep(233,11)
    ),
    expected = c(0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1,1,1),
    tolerance = 0.1
  )
  
})
