test_that("calc_potassium_availability works", {
  expect_equal(
    # aardappel op dekzand
    calc_potassium_availability(
      A_PH_CC = 5.6,
      A_SOM_LOI = 7.9,
      A_CEC_CO = 122, 
      A_K_CC = 95,
      A_K_CO_PO = 1.56,
      A_CLAY_MI = 1.73,
      B_SOILTYPE_AGR = 'dekzand',
      B_LU_BRP = 3732
    ),
    expected = 10.0,
    tolerance = 0.1
  )
  expect_equal(
    # aardappel op dekzand, zeeklei, rivierklei en loss
    calc_potassium_availability(
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      A_CEC_CO = c(122,191,157,142), 
      A_K_CC = c(95,107,73,89),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      A_CLAY_MI = c(1.7,26,31,14),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(3732,4)
    ),
    expected = c(10.13,28.53,24.29,16.05),
    tolerance = 1
  )
  expect_equal(
    # grassland op dekzand, zeeklei, rivierklei en loss
    calc_potassium_availability(
      A_PH_CC = c(5.6,7.04,5.65,6.22),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      A_CEC_CO = c(122,191,157,142), 
      A_K_CC = c(95,107,73,89),
      A_K_CO_PO = c(1.6,2.9,3.1,2.4),
      A_CLAY_MI = c(1.7,26,31,14),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(265,4)
    ),
    expected = c(3.31,3.21,2.50,3.06),
    tolerance = 0.01
  )
  expect_equal(
    # grassland op rivierklei
    calc_potassium_availability(
      A_PH_CC = rep(5.2,10),
      A_SOM_LOI = rep(4.6,10),
      A_CEC_CO = seq(10,400,length.out = 10), 
      A_K_CC = seq(10,400,length.out = 10), 
      A_K_CO_PO = seq(1,8,length.out = 10), 
      A_CLAY_MI = rep(8,10),
      B_SOILTYPE_AGR = rep('rivierklei',10),
      B_LU_BRP = rep(265,10)
    ),
    expected = c(1.58,2.71,3.47,3.77,3.89,3.94,3.97,3.98,3.99,3.99),
    tolerance = 0.01
  )
  expect_equal(
    # aardappel op rivierklei
    calc_potassium_availability(
      A_PH_CC = rep(5.2,10),
      A_SOM_LOI = rep(4.6,10),
      A_CEC_CO = seq(10,400,length.out = 10), 
      A_K_CC = seq(10,225,length.out = 10), 
      A_K_CO_PO = seq(1,8,length.out = 10), 
      A_CLAY_MI = rep(8,10),
      B_SOILTYPE_AGR = rep('rivierklei',10),
      B_LU_BRP = rep(3732,10)
    ),
    expected = c(0.70,11.43,25.57,43.11,64.07,88.43,116.2,147.37,181.96,219.95),
    tolerance = 0.01
  )
})

test_that("ind_potassium works", {
  expect_equal(
    # aardappel op dekzand
    ind_potassium(
      D_K = 10,
      B_SOILTYPE_AGR = 'dekzand',
      A_SOM_LOI = 7.9,
      B_LU_BRP = 3732
    ),
    expected = 0.60,
    tolerance = 0.1
  )
  expect_equal(
    # aardappel op dekzand, zeeklei, rivierklei en loss
    ind_potassium(
      D_K = c(10.13,28.53,24.29,16.05),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(3732,4)
    ),
    expected = c(0.61,0.999,0.998,0.915),
    tolerance = 0.01
  )
  expect_equal(
    # grassland op dekzand, zeeklei, rivierklei en loss
    ind_potassium(
      D_K = c(3.31,3.21,2.50,3.06),
      A_SOM_LOI = c(7.9,4.2,6.12,3.57),
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = rep(265,4)
    ),
    expected = c(0.99,0.99,0.917,0.998),
    tolerance = 0.01
  )
  expect_equal(
    # grassland op rivierklei
    ind_potassium(
      D_K = c(1.58,2.71,3.47,3.77,3.89,3.94,3.97,3.98,3.99,3.99),
      A_SOM_LOI = rep(4.6,10),
      B_SOILTYPE_AGR = rep('rivierklei',10),
      B_LU_BRP = rep(265,10)
    ),
    expected = c(0.40,0.98,rep(0.99,8)),
    tolerance = 0.01
  )
  expect_equal(
    # aardappel op rivierklei
    ind_potassium(
      D_K = c(1.01,10.82,25.56,44.71,68.24,96.17,128.5,165.23,206.34,251.86),
      A_SOM_LOI = rep(4.6,10),
      B_SOILTYPE_AGR = rep('rivierklei',10),
      B_LU_BRP = rep(3732,10)
    ),
    expected = c(0.0085,0.450,0.99,rep(1,7)),
    tolerance = 0.1
  )
  
  expect_equal(
    # aardappel op loss
    ind_potassium(
      D_K = seq(1,30,length.out = 10),
      A_SOM_LOI = rep(4.6,10),
      B_SOILTYPE_AGR = rep('loess',10),
      B_LU_BRP = rep(3732,10)
    ),
    expected = c(00.008,0.04,0.14,0.43,0.79,0.95,0.99,0.998,1,1),
    tolerance = 0.1
  )
  
  expect_equal(
    # aardappel op zeeklei > 10%
    ind_potassium(
      D_K = seq(1,40,length.out = 10),
      A_SOM_LOI = rep(15,10),
      B_SOILTYPE_AGR = rep('zeeklei',10),
      B_LU_BRP = rep(3732,10)
    ),
    expected = c(0.02,0.0986,0.36,0.75,0.94,0.99,0.99,0.99,1,1),
    tolerance = 0.1
  )
  expect_equal(
    # aardappel op dekzand
    ind_potassium(
      D_K = seq(1,30,length.out = 10),
      A_SOM_LOI = rep(15,10),
      B_SOILTYPE_AGR = rep('dekzand',10),
      B_LU_BRP = rep(3732,10)
    ),
    expected = c(0.1,0.22,0.42,0.65,0.83,0.93,0.97,0.99,1,1),
    tolerance = 0.1
  )
  
})

