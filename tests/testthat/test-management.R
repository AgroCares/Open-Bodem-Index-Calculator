test_that("calc_management works", {
  expect_equal(
    calc_management(
      A_OS_GV = 4.5,
      B_LU_BRP = 3732, 
      B_BT_AK = 'dekzand',
      B_GT = 'GtIV',
      D_OS_BAL = 1115,
      D_CP_GRASS = 0.2,
      D_CP_POTATO = 0.5,
      D_CP_RUST = 0.3,
      D_CP_RUSTDEEP = 0.2,
      D_GA = 0,
      M_M4 = TRUE, 
      M_M6 = TRUE, 
      M_M10 = TRUE, 
      M_M11 = TRUE, 
      M_M12 = TRUE, 
      M_M13 = TRUE, 
      M_M14 = TRUE, 
      M_M15 = TRUE),
    expected = 4,
    tolerance = 1
  )
  expect_equal(
    calc_management(
      A_OS_GV = seq(2.1,24.5,length.out = 10),
      B_LU_BRP = rep(3732,10), 
      B_BT_AK = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      B_GT = c(rep('GtIV',4),'GtII','GtI','GtVII',rep('GtIV',3)),
      D_OS_BAL = rep(1115,10),
      D_CP_GRASS = c(rep(0.2,5),0.9,0,0.2,0.2,.1),
      D_CP_POTATO = c(rep(0.5,5),0,rep(0.3,4)),
      D_CP_RUST = c(rep(0.3,5),0.1,0.7,0.5,0.5,0.6),
      D_CP_RUSTDEEP = rep(0,10),
      D_GA = c(rep(5,5),9,0,2,2,1),
      M_M4 = rep(TRUE,10), 
      M_M6 = rep(TRUE,10), 
      M_M10 = rep(TRUE,10),
      M_M11 = rep(TRUE,10),
      M_M12 = rep(TRUE,10),
      M_M13 = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_M14 = rep(TRUE,10),
      M_M15 = rep(TRUE,10)),
    expected = c(6,6,6,6,6,0,7,7,7,7),
    tolerance = 1
  )
  expect_equal(
    calc_management(
      A_OS_GV = seq(2.1,24.5,length.out = 10),
      B_LU_BRP = rep(265,10), 
      B_BT_AK = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond'),
      B_GT = c(rep('GtIV',4),'GtII','GtI','GtVII',rep('GtIV',3)),
      D_OS_BAL = rep(1115,10),
      D_CP_GRASS = c(rep(0.2,5),0.9,0,0.2,0.2,.1),
      D_CP_POTATO = c(rep(0.5,5),0,rep(0.3,4)),
      D_CP_RUST = c(rep(0.3,5),0.1,0.7,0.5,0.5,0.6),
      D_CP_RUSTDEEP = rep(0,10),
      D_GA = c(rep(5,5),9,0,2,2,1),
      M_M4 = rep(TRUE,10), 
      M_M6 = rep(TRUE,10), 
      M_M10 = rep(TRUE,10),
      M_M11 = rep(TRUE,10),
      M_M12 = rep(TRUE,10),
      M_M13 = c(rep(FALSE,5),TRUE,rep(FALSE,4)),
      M_M14 = rep(TRUE,10),
      M_M15 = rep(TRUE,10)),
    expected = c(2,2,2,2,1,14,1,1,1,1),
    tolerance = 1
  )
  
})


test_that("ind_management works", {
  expect_equal(
    ind_management(
      D_MAN = c(2,2,2,2,1,14,1,1,1,1),
      B_LU_BRP = rep(265,10),
      B_BT_AK = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond')),
    expected = c(rep(0.29,4),0.14,1,rep(0.14,4)),
    tolerance = .01
  )
  expect_equal(
    ind_management(
      D_MAN = c(2,2,2,2,1,14,1,1,1,1),
      B_LU_BRP = rep(3732,10),
      B_BT_AK = c(rep('dekzand',5),'veen','duinzand','rivierklei','zeeklei','dalgrond')),
    expected = c(rep(.14,4),0.07,1,rep(0.07,4)),
    tolerance = .01
  )
  
})
