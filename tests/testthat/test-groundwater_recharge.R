test_that("test whether calc_gw_recharge works", {
  expect_equal(
    calc_gw_recharge(
      B_LU_BRP = 265,
      M_GREEN = TRUE
      ),
    expected = 220.993,
    tolerance = 0.01
  )
  
  expect_equal(
    calc_gw_recharge(
      B_LU_BRP = c(265, 265, 1079, 1079, 1079, 237,237, 265, 265, 265, 308, 308, 265),
      M_GREEN = rep(TRUE,13)
      ),
    expected = c(220.993, 239.698, 273.973, 273.973, 273.973, 401.339, 401.339, 239.698, 239.698, 239.698, 462.602, 462.602, 239.698),
    tolerance = 0.01
  )
})

test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      D_PSP = c(100,200,300,400,500,600)
    ),
    expected = c(0.2150968, 0.4499966, 0.7937005, 0.9686533, 0.9966717, 0.9996643),
    tolerance = 0.01
  )
 
})
