test_that("test whether calc_gw_recharge works", {
  expect_equal(
    calc_gw_recharge(
      ID = 1, 
      B_LU_BRP = 265,
      M_GREEN = TRUE
    ),
    expected = 187.12,
    tolerance = 0
  )
  expect_equal(
    calc_gw_recharge(
      ID = rep(1,13), 
      B_LU_BRP = c(265, 265, 1079, 1079, 1079, 237,237, 265, 265, 265, 308, 308, 265),
      M_GREEN = rep(TRUE,13)
    ),
    expected = c(377.1,377.1,rep(187.12,6),259.88,259.88,259.88,464.31,464.31),
    tolerance = 0.01
  )
})

test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      D_PSP = c(100,200,300,400,500)
    ),
    expected = c(0.1,0.2,0.3,0.4,0.5),
    tolerance = 0.01
  )
 
})
