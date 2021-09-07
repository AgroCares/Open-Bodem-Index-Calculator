test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      D_PSP = c(100,200,300,400,500,600),
      I_P_CO = c(0.1,1,0.1,1,0.1,1),
      I_P_SE = rep(0.8,6),
      D_WRI_WHC =  c(0.1,0.25,.35,0.5,0.75,1),
      B_DRAIN = rep(TRUE,6)
    ),
    expected = c(0.15,0.216,0.411,0.532,0.561,0.564),
    tolerance = 0.01
  )
 
})
