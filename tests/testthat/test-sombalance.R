test_that("calc_sombalance works", {
  expect_equal(
    # aardappel
    calc_sombalance(
      A_OS_GV = 7.92, 
      A_P_PAL = 63, 
      A_P_WA = 65, 
      B_LU_BRP = 3732,
      M_M3 = 3, 
      M_M6 = TRUE
     ),
    expected = c(1117),
    tolerance = 0.1
  )
  set.seed(123)
  a= sample(24:100,10)
  expect_equal(
    calc_sombalance(
      A_OS_GV = seq(1,20,length.out = 10), 
      A_P_PAL = a, 
      A_P_WA = a*1.05, 
      B_LU_BRP = rep(3732,10),
      M_M3 = rep(3,10), 
      M_M6 = rep(TRUE,10)
    ),
    expected = c(2360,1692,1366,1161,1022,924,855,807,991,1294),
    tolerance = 1
  )
  expect_equal(
    calc_sombalance(
      A_OS_GV = seq(1,20,length.out = 10), 
      A_P_PAL = a, 
      A_P_WA = a*1.05, 
      B_LU_BRP = rep(265,10),
      M_M3 = rep(3,10), 
      M_M6 = rep(TRUE,10)
    ),
    expected = c(3718,2717,2391,2186,2047,2615,1880,1832,1800,1780),
    tolerance = 1
  )
})
