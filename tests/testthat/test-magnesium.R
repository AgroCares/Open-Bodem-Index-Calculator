test_that("calc_magnesium_availability works", {
  expect_equal(
    # aardappel op dekzand
    calc_magnesium_availability(
      A_MG_CC = 122,
      A_PH_CC = 5.6,
      A_OS_GV = 7.9,
      A_CEC_CO = 122, 
      A_K_CC = 95,
      A_K_CEC = 1.56,
      A_CLAY_MI = 1.73,
      B_BT_AK = 'dekzand',
      B_LU_BRP = 3732
    ),
    expected = 122,
    tolerance = 1
  )
  expect_equal(
  # aardappel op dekzand, zeeklei, rivierklei en loss
  calc_magnesium_availability(
    A_MG_CC = c(122,146,291,160),
    A_PH_CC = c(5.6,7.04,5.65,6.22),
    A_OS_GV = c(7.9,4.2,6.12,3.57),
    A_CEC_CO = c(122,191,157,142), 
    A_K_CC = c(95,107,73,89),
    A_K_CEC = c(1.6,2.9,3.1,2.4),
    A_CLAY_MI = c(1.7,26,31,14),
    B_BT_AK = c('dekzand','zeeklei','rivierklei','loess'),
    B_LU_BRP = rep(3732,4)
  ),
  expected = c(122,136,281,160),
  tolerance = 1
)
  expect_equal(
  # grassland op dekzand, zeeklei, rivierklei en loss
  calc_magnesium_availability(
    A_MG_CC = c(122,146,291,160),
    A_PH_CC = c(5.6,7.04,5.65,6.22),
    A_OS_GV = c(7.9,4.2,6.12,3.57),
    A_CEC_CO = c(122,191,157,142), 
    A_K_CC = c(95,107,73,89),
    A_K_CEC = c(1.6,2.9,3.1,2.4),
    A_CLAY_MI = c(1.7,26,31,14),
    B_BT_AK = c('dekzand','zeeklei','rivierklei','loess'),
    B_LU_BRP = rep(265,4)
  ),
  expected = c(122,2.026,0.4580,160),
  tolerance = 0.01
)
})



test_that("ind_magnesium works", {
  # aardappel op dekzand
  expect_equal(
    ind_magnesium(
      D_MG = 122,
      B_LU_BRP=3732,
      B_BT_AK = 'dekzand'
    ),
    expected = 1,
    tolerance = 0.01
  )
  # aardappel op dekzand, zeeklei, rivierklei en loss
  expect_equal(
    ind_magnesium(
      D_MG = seq(1,400,length.out = 10),
      B_LU_BRP=rep(3732,10),
      B_BT_AK = rep('dekzand',10)
    ),
    expected = c(0.12,0.58,0.999,0.999,rep(1,6)),
    tolerance = 0.01
  )
  # grasland op klei
  expect_equal(
    ind_magnesium(
      D_MG = seq(0.1,2,length.out = 10),
      B_LU_BRP=rep(265,10),
      B_BT_AK = rep('zeeklei',10)
    ),
    expected = c(0.6755,0.9977,0.999,rep(1,7)),
    tolerance = 0.01
  )
})