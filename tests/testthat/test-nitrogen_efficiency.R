test_that("test whether calc_nitrogen_efficiency works", {
  expect_equal(
    calc_n_efficiency(B_LU_BRP = c(233,259,2014,308),
                      B_SOILTYPE_AGR = rep('rivierklei',4),
                      B_GWL_CLASS = rep('GtIII',4),
                      B_AER_CBS = rep('IJsselmeerpolders',4),
                      A_SOM_LOI = rep(3,4),
                      A_CLAY_MI = rep(20,4),
                      D_PBI = rep(2,4),
                      D_K = rep(12,4),
                      D_PH_DELTA = rep(0.2,4),
                      M_GREEN = rep(FALSE,4),
                      B_FERT_NORM_FR = c(1,1,1,0.8),
                      leaching_to = 'gw'
    ),
    expected = c(10.00, 11.25, 10.00, 10.00),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_n_efficiency(B_LU_BRP = c(233,259,2014,308),
                      B_SOILTYPE_AGR = rep('dekzand',4),
                      B_GWL_CLASS = rep('GtII',4),
                      B_AER_CBS = rep('Zuidwest-Brabant',4),
                      A_SOM_LOI = rep(3,4),
                      A_CLAY_MI = rep(15,4),
                      D_PBI = rep(2,4),
                      D_K = rep(12,4),
                      D_PH_DELTA = rep(0.2,4),
                      M_GREEN = rep(TRUE,4),
                      B_FERT_NORM_FR = c(1,0.8,1,1),
                      leaching_to = 'gw'
    ),
    expected = c(1.90,2.09,1.90,1.90),
    tolerance = 0.01
  )
  
})


test_that("test whether ind_nitrogen_efficiency works", {
  expect_equal(
    ind_n_efficiency(D_NLEACH = c(1,10,30,40,50),
                     leaching_to = 'gw'
    ),
    expected = c(0.992600350,0.955434864,0.273600241,0.048369957,0.006828972),
    tolerance = 0.01
  )
  
 
  
})
