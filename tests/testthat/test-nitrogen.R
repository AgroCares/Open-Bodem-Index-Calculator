test_that("calc_nlv works", {
  expect_equal(
    calc_nlv(
      A_N_TOT = 5000,
      D_OC = 86000,
      B_LU_BRP = 265,
      B_BT_AK = "rivierklei",
      D_BDS = 1300,
      A_CN_RAT = 4,
      D_GA = 2
    ),
    expected = 204.22254,
    tolerance = 1
  )
  expect_equal(
    calc_nlv(
      A_N_TOT = c(5000, 10000, 25000, 8000),
      D_OC = c(86000, 86000, 86000,86000),
      B_LU_BRP = c(265, 265, 235, 235),
      B_BT_AK = c("rivierklei", "veen", "veen", "dekzand"),
      D_BDS = c(1300, 800, 850, 1100),
      A_CN_RAT = c(12, 12, 12, 12),
      D_GA = c(2, 8, 0, 0)
    ),
    expected = c(204.22254, 250, 90,  90),
    tolerance = 1
  )
})

test_that("eval_nitrogen works", {
  expect_equal(
    ind_nitrogen(
      D_NLV = c(140, 100),
      B_LU_BRP = c(265, 2014)
    ),
    expected = c(1, 1),
    tolerance = 0.001
  )
  expect_equal(
    ind_nitrogen(
      D_NLV = seq(from = -30, to = 250, by = 50),
      B_LU_BRP = rep(265, 6)
    ),
    expected = c(0, 0.2653061, 0.7500000, 0.9795918, 1, 1),
    tolerance = 0.001
  )
  expect_equal(
    ind_nitrogen(
      D_NLV = seq(from = -30, to = 250, by = 50),
      B_LU_BRP = rep(2014, 6)
    ),
    expected = c(0, 0.36, 0.91, 1, 1, 1),
    tolerance = 0.001
  )
})