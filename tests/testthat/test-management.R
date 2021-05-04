test_that("calc_management works", {
  expect_equal(
    calc_management(
      A_SOM_LOI = 4.5,
      B_LU_BRP = 3732, 
      B_SOILTYPE_AGR = 'dekzand',
      B_GWL_CLASS = 'GtIV',
      D_SOM_BAL = 1115,
      D_CP_GRASS = 0.2,
      D_CP_POTATO = 0.5,
      D_CP_RUST = 0.3,
      D_CP_RUSTDEEP = 0.2,
      D_GA = 0,
      M_COMPOST = rep(25,1),
      M_GREEN = TRUE, 
      M_NONBARE = TRUE, 
      M_EARLYCROP = TRUE, 
      M_SLEEPHOSE = TRUE, 
      M_DRAIN = TRUE, 
      M_DITCH = TRUE, 
      M_UNDERSEED = TRUE,
      M_LIME = TRUE, 
      M_NONINVTILL = TRUE, 
      M_SSPM = TRUE, 
      M_SOLIDMANURE = TRUE,
      M_STRAWRESIDUE = TRUE,
      M_MECHWEEDS = TRUE,
      M_PESTICIDES_DST = TRUE),
    expected = 13,
    tolerance = 1
  )
  expect_equal(
    calc_management(
      A_SOM_LOI = seq(2.1,24.5,length.out = 10),
      B_LU_BRP = rep(3732,10), 
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      B_GWL_CLASS = c(rep('GtIV',4),'GtII','GtI','GtVII',rep('GtIV',3)),
      D_SOM_BAL = rep(1115,10),
      D_CP_GRASS = c(rep(0.2,5),0.9,0,0.2,0.2,.1),
      D_CP_POTATO = c(rep(0.5,5),0,rep(0.3,4)),
      D_CP_RUST = c(rep(0.3,5),0.1,0.7,0.5,0.5,0.6),
      D_CP_RUSTDEEP = rep(0,10),
      D_GA = c(rep(5,5),9,0,2,2,1),
      M_COMPOST = rep(25,10),
      M_GREEN = rep(TRUE,10), 
      M_NONBARE = rep(TRUE,10),
      M_EARLYCROP = rep(TRUE,10),
      M_SLEEPHOSE = rep(TRUE,10),
      M_DRAIN = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_DITCH = rep(TRUE,10),
      M_UNDERSEED = rep(TRUE,10),
      M_LIME = rep(TRUE,10), 
      M_NONINVTILL = rep(TRUE,10), 
      M_SSPM = rep(TRUE,10), 
      M_SOLIDMANURE = rep(TRUE,10),
      M_STRAWRESIDUE = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_MECHWEEDS = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_PESTICIDES_DST = c(rep(FALSE,5),TRUE,rep(FALSE,4))),
    expected = c(10,10,10,10,10,8,11,11,11,11),
    tolerance = 1
  )
  expect_equal(
    calc_management(
      A_SOM_LOI = seq(2.1,24.5,length.out = 10),
      B_LU_BRP = rep(265,10), 
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      B_GWL_CLASS = c(rep('GtIV',4),'GtII','GtI','GtVII',rep('GtIV',3)),
      D_SOM_BAL = rep(1115,10),
      D_CP_GRASS = c(rep(0.2,5),0.9,0,0.2,0.2,.1),
      D_CP_POTATO = c(rep(0.5,5),0,rep(0.3,4)),
      D_CP_RUST = c(rep(0.3,5),0.1,0.7,0.5,0.5,0.6),
      D_CP_RUSTDEEP = rep(0,10),
      D_GA = c(rep(5,5),9,0,2,2,1),
      M_COMPOST = rep(25,10),
      M_GREEN = rep(TRUE,10), 
      M_NONBARE = rep(TRUE,10),
      M_EARLYCROP = rep(TRUE,10),
      M_SLEEPHOSE = rep(TRUE,10),
      M_DRAIN = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_DITCH = rep(TRUE,10),
      M_UNDERSEED = rep(TRUE,10),
      M_LIME = rep(TRUE,10), 
      M_NONINVTILL = rep(TRUE,10), 
      M_SSPM = rep(TRUE,10), 
      M_SOLIDMANURE = rep(TRUE,10),
      M_STRAWRESIDUE = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_MECHWEEDS = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_PESTICIDES_DST = c(rep(FALSE,5),TRUE,rep(FALSE,4))),
    expected = c(5,5,5,5,4,17,4,4,4,4),
    tolerance = 1
  )
  
})


test_that("ind_management works", {
  expect_equal(
    ind_management(
      D_MAN = c(2,2,2,2,1,11,1,1,1,1),
      B_LU_BRP = rep(265,10),
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond')),
    expected = c(rep(0.2,4),0.10,0.65,rep(0.10,4)),
    tolerance = .01
  )
  expect_equal(
    ind_management(
      D_MAN = c(2,2,2,2,1,11,1,1,1,1),
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond')),
    expected = c(rep(0.11,4),0.06,0.61,rep(0.06,4)),
    tolerance = .01
  )
  expect_equal(
    ind_management(
      D_MAN = rep(17,10),
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond')),
    expected = rep(0.94,10),
    tolerance = .01
  )
  expect_equal(
    ind_management(
      D_MAN = c(1,3,5,6,8,10,12,14,16,17,18),
      B_LU_BRP = rep(3732,11),
      B_SOILTYPE_AGR = c(rep('dekzand',6),'veen','duinzand','rivierklei','zeeklei','dalgrond')),
    expected =c(0.06,0.17,0.28,0.33,0.44,0.56,0.67,0.78,0.89,0.94,1),
    tolerance = .01
  )  
    
})
