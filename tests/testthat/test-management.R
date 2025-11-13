test_that("calc_management works", {
  expect_equal(
    calc_management(
      A_SOM_LOI = 4.5,
      B_LU_BRP = 3732, 
      B_SOILTYPE_AGR = 'dekzand',
      B_GWL_CLASS = 'IV',
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
      B_GWL_CLASS = c(rep('IV',4),'II','I','VII',rep('IV',3)),
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
      B_GWL_CLASS = c(rep('IV',4),'II','I','VII',rep('IV',3)),
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

test_that("calc_man_ess", {
  expect_equal(
    calc_man_ess(
      A_SOM_LOI = 4.5,
      B_LU_BRP = 3732, 
      B_SOILTYPE_AGR = 'dekzand',
      B_GWL_CLASS = 'IV',
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
      M_PESTICIDES_DST = TRUE,
      type = 'I_M_SOILFERTILITY'),
    expected = 11,
    tolerance = 1
  )
  expect_equal(
    calc_man_ess(
      A_SOM_LOI = seq(2.1,24.5,length.out = 10),
      B_LU_BRP = rep(3732,10), 
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      B_GWL_CLASS = c(rep('IV',4),'II','I','VII',rep('IV',3)),
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
      M_PESTICIDES_DST = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      type = 'I_M_SOILFERTILITY'),
    expected = c(10,10,10,10,10,6,11,11,11,11),
    tolerance = 1
  )
  expect_equal(
    calc_man_ess(
      A_SOM_LOI = seq(2.1,24.5,length.out = 10),
      B_LU_BRP = rep(265,10), 
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      B_GWL_CLASS = c(rep('IV',4),'II','I','VII',rep('IV',3)),
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
      M_PESTICIDES_DST = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      type = 'I_M_CLIMATE'),
    expected = c(2,2,2,2,1,12,1,1,1,1),
    tolerance = 1
  )
  expect_equal(
    calc_man_ess(
      A_SOM_LOI = rep(5,10),
      B_LU_BRP = rep(265,10), 
      B_SOILTYPE_AGR = c(rep('dekzand',10)),
      B_GWL_CLASS = c(rep('IV',4),'II','I','VII',rep('IV',3)),
      D_SOM_BAL = rep(1115,10),
      D_CP_GRASS = rep(1,10),
      D_CP_POTATO = c(rep(0.5,5),0,rep(0.3,4)),
      D_CP_RUST = c(rep(0.3,5),0.1,0.7,0.5,0.5,0.6),
      D_CP_RUSTDEEP = rep(0,10),
      D_GA = 10:1,
      M_COMPOST = rep(25,10),
      M_GREEN = rep(TRUE,10), 
      M_NONBARE = rep(TRUE,10),
      M_EARLYCROP = rep(TRUE,10),
      M_SLEEPHOSE = rep(TRUE,10),
      M_DRAIN = rep(TRUE,10),
      M_DITCH = rep(TRUE,10),
      M_UNDERSEED = rep(TRUE,10),
      M_LIME = rep(TRUE,10), 
      M_NONINVTILL = rep(TRUE,10), 
      M_SSPM = rep(TRUE,10), 
      M_SOLIDMANURE = rep(TRUE,10),
      M_STRAWRESIDUE = rep(TRUE,10),
      M_MECHWEEDS = rep(TRUE,10),
      M_PESTICIDES_DST = rep(TRUE,10),
      type = 'I_M_SOILFERTILITY'),
    expected = c(9,9,9,9,9,9,9,8,8,8),
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

test_that("ind_man_ess works", {
  expect_equal(
    ind_man_ess(
      D_MAN = c(9,9,9,9,9,9,9,8,8,8),
      B_LU_BRP = rep(265,10),
      B_SOILTYPE_AGR = c(rep('dekzand',10)),
      type='I_M_SOILFERTILITY'),
    expected = c(.9,.9,.9,.9,.9,.9,.9,.8,.8,.8),
    tolerance = .01
  )
  expect_equal(
    ind_man_ess(
      D_MAN = c(9,9,9,9,12,12,9,8,8,8),
      B_LU_BRP = rep(265,10),
      B_SOILTYPE_AGR = c(rep('veen',10)),
      type='I_M_SOILFERTILITY'),
    expected = c(.69,.69,.69,.69,.92,.92,.69,.62,.62,.62),
    tolerance = .01
  )
  expect_equal(
    ind_man_ess(
      D_MAN = seq(3,15,length.out = 10),
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      type='I_M_SOILFERTILITY'),
    expected = c(0.25,0.36,0.47,0.58,0.69,0.81,0.92,1,1,1),
    tolerance = .01
  )
  expect_equal(
    ind_man_ess(
      D_MAN = seq(3,15,length.out = 10),
      B_LU_BRP = rep(3732,10),
      B_SOILTYPE_AGR = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      type='I_M_CLIMATE'),
    expected = c(0.23,0.33,0.44,0.54,0.64,0.88,0.85,0.95,1,1),
    tolerance = .01
  )
  expect_equal(
    ind_man_ess(
      D_MAN = c(1,3,5,6,8,10,12,14,15,15,15),
      B_LU_BRP = rep(3732,11),
      B_SOILTYPE_AGR = c(rep('dekzand',6),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      type='I_M_SOILFERTILITY'),
    expected =c(0.08,0.25,0.42,0.5,0.67,0.83,1,1,1,1,1),
    tolerance = .01
  )  
  
})

test_that('add_management enforces the use of green manure when growing maize or potato on sand', {
  maize_potato_codes <- c(259, 316, 317, 814, 859, 1909, 1910, 1911, 1912, 1928,
                          1929, 1935, 2014:2017, 2025, 2032, 2951, 3730:3732, 3792)
  expect_equal(
    object = add_management(
      ID = rep(1, length(maize_potato_codes)),
      B_LU_BRP = maize_potato_codes,
      M_GREEN = FALSE,
      B_SOILTYPE_AGR = rep('dekzand', length(maize_potato_codes))
    ),
    expected = add_management(
      ID = rep(1, length(maize_potato_codes)),
      B_LU_BRP = maize_potato_codes,
      M_GREEN = TRUE,
      B_SOILTYPE_AGR = rep('dekzand', length(maize_potato_codes))
    )
  )
  
  expect_equal(
    object = add_management(
      ID = rep(1, length(maize_potato_codes)),
      B_LU_BRP = maize_potato_codes,
      M_GREEN = FALSE,
      B_SOILTYPE_AGR = rep('dekzand', length(maize_potato_codes))
    )$M_GREEN,
    expected = rep(TRUE, length(maize_potato_codes))
  )
  
  expect_true(
    add_management(
      ID = 1,
      B_LU_BRP = 2014,
      M_GREEN = FALSE,
      B_SOILTYPE_AGR = 'zeeklei'
    )$M_GREEN == FALSE
  )
})
