test_that("calc_bodemconditiescore works", {
  expect_equal(
    calc_bcs(
      A_EW_BCS = c(0,1,2), 
      A_SC_BCS = c(0,1,2), 
      A_GS_BCS = c(0,1,2), 
      A_P_BCS = c(0,1,2), 
      A_C_BCS = c(0,1,2), 
      A_RT_BCS = c(0,1,2), 
      A_RD_BCS = c(0,1,2), 
      A_SS_BCS = c(0,1,2), 
      A_CC_BCS = c(0,1,2),
      A_SOM_LOI = c(4,5,5),
      D_PH_DELTA = c(0.5,0.5,0.5),
      B_LU_BRP = rep(3732,3),
      B_SOILTYPE_AGR = rep('dekzand',3)
      ),
    expected = c(6,20,31),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_bcs(
      A_EW_BCS = c(0,1,2), 
      A_SC_BCS = c(0,1,2), 
      A_GS_BCS = c(0,1,2), 
      A_P_BCS = c(0,1,2), 
      A_C_BCS = c(0,1,2), 
      A_RT_BCS = c(0,1,2), 
      A_RD_BCS = c(0,1,2), 
      A_SS_BCS = c(0,1,2), 
      A_CC_BCS = c(0,1,2),
      A_SOM_LOI = c(7.5,7.5,7.5),
      D_PH_DELTA = c(0,0,0),
      B_LU_BRP = rep(265,3),
      B_SOILTYPE_AGR = rep('dekzand',3)
    ),
    expected = c(12, 23, 34),
    tolerance = 0.01
  )
  
})

test_that("ind_bodemconditiescore works", {
  expect_equal(
    ind_bcs(
      D_BCS = c(6,17,36)
    ),
    expected = c(0.15,0.425,0.9),
    tolerance = 0.01
  )
  
})


