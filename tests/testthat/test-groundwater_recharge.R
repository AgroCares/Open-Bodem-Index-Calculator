test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      B_LU_BRP = c(265,265,265,233,233,233),
      D_PSP = c(100,200,300,400,500,600),
      D_WRI_K = c(50,90,50,100,50,90),
      I_P_SE = rep(0.8,6),
      I_P_CO = c(0.1,1,0.1,1,0.1,1),
      B_DRAIN = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),
      B_GWL_CLASS = c('III','III','III','IV','III','IV')
    ),
    expected = c(0.1274698, 0.3502220, 0.5416138, 0.9614681, 0.6772030, 0.9557414),
    tolerance = 0.01
  )
 
})

test_that("B_DRAIN affects scores output for IIIb and IV", {
  expect_false(
    all(
      ind_gw_recharge(
        B_LU_BRP = c(265,265,265,233,233,233),
        D_PSP = c(100,200,300,400,500,600),
        D_WRI_K = c(50,90,50,100,50,90),
        I_P_SE = rep(0.8,6),
        I_P_CO = c(0.1,1,0.1,1,0.1,1),
        B_DRAIN = rep(TRUE, 6),
        B_GWL_CLASS = c('IIIb','IIIb','IIIb','IV','IIIb','IV')
      ) ==
        ind_gw_recharge(
          B_LU_BRP = c(265,265,265,233,233,233),
          D_PSP = c(100,200,300,400,500,600),
          D_WRI_K = c(50,90,50,100,50,90),
          I_P_SE = rep(0.8,6),
          I_P_CO = c(0.1,1,0.1,1,0.1,1),
          B_DRAIN = rep(FALSE, 6),
          B_GWL_CLASS = c('IIIb','IIIb','IIIb','IV','IIIb','IV')
        )
    )
  )
})

test_that("Groundwater recharge is correctly modified with a target by ind_gw_target", {
  dt <- data.table(
    I_H_GWR = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7),
    B_SOILTYPE_AGR = c('veen',
                       'zeeklei', 'zeeklei', 'zeeklei', 'zeeklei',
                       'loess', 'loess'),
    B_GWL_CLASS = c('IIa',
                    'IIa', 'Va', 'Vb', 'IVu',
                    'IIa', 'VIIIo')
  )
  
  dt[,S_H_GWR := ind_gw_target(I_H_GWR, B_SOILTYPE_AGR, B_GWL_CLASS)]
  
  expect_gt(dt[B_SOILTYPE_AGR == 'veen' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IIa', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Va', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Va', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Vb', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Vb', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IVu', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'loess' & B_GWL_CLASS == 'IIa', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'loess' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'loess' & B_GWL_CLASS == 'VIIIo', S_H_GWR])

})
