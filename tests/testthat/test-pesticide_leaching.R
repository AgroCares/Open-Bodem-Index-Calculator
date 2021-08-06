test_that("test whether calc_petsicide_leaching works", {
  expect_equal(
    calc_pesticide_leaching(B_SOILTYPE_AGR = rep('dekzand',4),
                            A_SOM_LOI = c(2,3,5,8),
                            A_CLAY_MI = rep(20,4),
                            A_SAND_MI = rep(15,4),
                            A_SILT_MI = rep(10,4),
                            D_PSP = rep(300,4)
    ),
    expected = c(0.9458933, 0.9004914, 0.8216655, 0.7271828),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_pesticide_leaching(B_SOILTYPE_AGR = rep('dekzand',4),
                            A_SOM_LOI = rep(3,4),
                            A_CLAY_MI = rep(20,4),
                            A_SAND_MI = rep(15,4),
                            A_SILT_MI = rep(10,4),
                            D_PSP = c(50,100,400,800)
    ),
    expected = c(0.5331843, 0.7301947, 0.9243994, 0.9614569),
    tolerance = 0.01
  )
  
})


test_that("test whether ind_petsicide_leaching works", {
  expect_equal(
    ind_pesticide_leaching(D_PESTICIDE = c(0.3,0.5,0.7,0.9),
                           B_GWL_CLASS = rep('GtII',4)
    ),
    expected = c(1.00000000, 1.00000000, 0.83356949, 0.08925114),
    tolerance = 0.01
  )
  
  expect_equal(
    ind_pesticide_leaching(D_PESTICIDE = c(0.3,0.5,0.7,0.9),
                           B_GWL_CLASS = rep('GtV',4)
    ),
    expected = c(0.9987274, 0.9817146, 0.7577904, 0.0811374),
    tolerance = 0.01
  )
  
})

