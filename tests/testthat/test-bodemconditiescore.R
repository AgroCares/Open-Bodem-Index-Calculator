test_that("calc_bodemconditiescore works", {
  expect_equal(
    calc_bcs(
      A_RW_BC = c(0,1,2), 
      A_BS_BC = c(0,1,2), 
      A_GV_BC = c(0,1,2), 
      A_PV_BC = c(0,1,2), 
      A_AS_BC = c(0,1,2), 
      A_SV_BC = c(0,1,2), 
      A_RD_BC = c(0,1,2), 
      A_SS_BC = c(0,1,2), 
      A_CO_BC = c(0,1,2),
      A_OS_GV = c(4,5,5),
      D_PH_DELTA = c(0.5,0.5,0.5),
      B_LU_BRP = rep(3732,3),
      B_BT_AK = rep('dekzand',3)
      ),
    expected = c(3,20,31),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_bcs(
      A_RW_BC = c(0,1,2), 
      A_BS_BC = c(0,1,2), 
      A_GV_BC = c(0,1,2), 
      A_PV_BC = c(0,1,0), 
      A_AS_BC = c(0,1,0), 
      A_SV_BC = c(0,1,0), 
      A_RD_BC = c(0,1,2), 
      A_SS_BC = c(0,1,2), 
      A_CO_BC = c(0,1,2),
      A_OS_GV = c(7.5,7.5,7.5),
      D_PH_DELTA = c(0,0,0),
      B_LU_BRP = rep(265,3),
      B_BT_AK = rep('dekzand',3)
    ),
    expected = c(12, 23, 42),
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


