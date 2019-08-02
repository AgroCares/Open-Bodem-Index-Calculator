test_that("calc_pmn works", {
  expect_equal(
    calc_pmn(
      A_N_PMN = 65,
      B_LU_BRP = 265,
      B_BT_AK = "rivierklei"
    ),
    expected = 74.21,
    tolerance = 0.01
  )
  expect_equal(
    calc_pmn(
      A_N_PMN = seq(15,75,length.out = 8),
      B_LU_BRP = c(rep(265,4), rep(235,4)),
      B_BT_AK = rep(c("rivierklei", "veen", "veen", "dekzand"),2)
    ),
    expected = c(17.13,23.57,32.14,38.43,25.82,57.86,66.43,64.67),
    tolerance = 0.1
  )
})

test_that("eval_pmn works", {
  expect_equal(
    ind_pmn(
      D_PMN = 65
    ),
    expected = 0.9999,
    tolerance = 0.001
  )
  expect_equal(
    ind_pmn(
      D_PMN = c(seq(from = 0, to = 60, by = 10),500)
    ),
    expected = c(0.035,0.1699,0.561,.8996,.985,.998,.9997,1),
    tolerance = 0.001
  )
})