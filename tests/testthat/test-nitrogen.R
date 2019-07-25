test_that("calc_nlv works", {
  expect_equal(
    calc_nlv(
      A_N_TOT = 5,
      D_OC = 100,
      B_LU_BRP = 265,
      B_BT_AK = "rivierklei",
      D_BDS = 1300,
      A_CN_RAT = 4,
      D_GA = 2
    ),
    expected = 204.22254,
    tolerance = 0.001
  )
  expect_equal(
    calc_nlv(
      A_N_TOT = c(5, 10, 25, 8),
      D_OC = c(100, 250, 300, 75),
      B_LU_BRP = c(265, 265, 235, 235),
      B_BT_AK = c("rivierklei", "veen", "veen", "dekzand"),
      D_BDS = c(1300, 800, 850, 1100),
      A_CN_RAT = c(4, 4, 4, 4),
      D_GA = c(2, 8, 0, 0)
    ),
    expected = c(204.22254, 250, 209.43603,  52.31861),
    tolerance = 0.001
  )
})

test_that("eval_nitrogen works", {
  expect_equal(
    ind_nitrogen(
      D_NLV = 120
    ),
    expected = 1,
    tolerance = 0.001
  )
  expect_equal(
    ind_nitrogen(
      D_NLV = seq(from = -30, to = 250, by = 50)
    ),
    expected = c(0, 0.3055556, 0.8263889, 1, 1, 1),
    tolerance = 0.001
  )
})