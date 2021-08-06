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
                      D_NLV = rep(110,4),
                      M_GREEN = rep(FALSE,4),
                      B_FERT_NORM_FR = c(1,1,1,0.8)
    ),
    expected = c(10.80, 12.15, 10.80, 10.80),
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
                      D_NLV = rep(90,4),
                      M_GREEN = rep(TRUE,4),
                      B_FERT_NORM_FR = c(1,0.8,1,1)
    ),
    expected = c(4.0, 4.4, 4.0, 4.0),
    tolerance = 0.01
  )
  
})


test_that("test whether ind_nitrogen_efficiency works", {
  expect_equal(
    ind_n_efficiency(c(1,10,30,40,50)
    ),
    expected = c(0.98415, 0.68749, 1.595222e-03, 4.365378e-05, 1.192833e-06),
    tolerance = 0.01
  )
  
 
  
})
