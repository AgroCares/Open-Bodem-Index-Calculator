test_that("test whether calc_petsicide_leaching works", {
  expect_equal(
    calc_pesticide_leaching(B_SOILTYPE_AGR = rep('dekzand',4),
                            A_SOM_LOI = c(2,3,5,8),
                            A_CLAY_MI = rep(20,4),
                            A_SAND_MI = rep(15,4),
                            A_SILT_MI = rep(10,4),
                            D_PSP = rep(300,4),
                            M_PESTICIDES_DST = rep(FALSE,4),
                            M_MECHWEEDS = rep(FALSE,4)
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
                            D_PSP = c(50,100,400,800),
                            M_PESTICIDES_DST = rep(FALSE,4),
                            M_MECHWEEDS = rep(FALSE,4)
    ),
    expected = c(0.5331843, 0.7301947, 0.9243994, 0.9614569),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_pesticide_leaching(B_SOILTYPE_AGR = rep('dekzand',4),
                            A_SOM_LOI = c(2,3,5,8),
                            A_CLAY_MI = rep(20,4),
                            A_SAND_MI = rep(15,4),
                            A_SILT_MI = rep(10,4),
                            D_PSP = rep(300,4),
                            M_PESTICIDES_DST = rep(TRUE,4),
                            M_MECHWEEDS = rep(FALSE,4)
    ),
    expected = c(0.7094200, 0.6753685, 0.6162491, 0.5453871),
    tolerance = 0.01
  )
  
})


test_that("test whether ind_petsicide_leaching works", {
  expect_equal(
    ind_pesticide_leaching(D_PESTICIDE = c(0.3,0.5,0.7,0.9)),
    expected = c(.999,0.996,0.851,0.0386),
    tolerance = 0.01
  )
  
})

