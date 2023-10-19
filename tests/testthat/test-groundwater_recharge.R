test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      B_LU_BRP = c(265,265,265,233,233,233),
      D_PSP = c(100,200,300,400,500,600),
      D_WRI_K = c(50,90,50,100,50,90),
      I_P_SE = rep(0.8,6),
      I_P_CO = c(0.1,1,0.1,1,0.1,1),
      B_DRAIN = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),
      B_GWL_CLASS = c('GtIII','GtIII','GtIII','GtIV','GtIII','GtIV')
    ),
    expected = c(0.1274698, 0.3502220, 0.5416138, 0.9614681, 0.6772030, 0.9557414),
    tolerance = 0.01
  )
 
})
