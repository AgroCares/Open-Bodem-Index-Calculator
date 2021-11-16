test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      D_PSP = c(100,200,300,400,500,600),
      D_WRI_K = c(50,90,50,100,50,90),
      I_P_SE = rep(0.8,6),
      I_P_CO = c(0.1,1,0.1,1,0.1,1),
      B_DRAIN = rep(TRUE,6),
      B_GWL_CLASS = c('GtIII','GtIII','GtIII','GtIV','GtIII','GtIV')
    ),
    expected = c( 0.1274698, 0.3502220, 0.5416138, 0.5768809, 0.6772030, 0.5734448),
    tolerance = 0.01
  )
 
})
