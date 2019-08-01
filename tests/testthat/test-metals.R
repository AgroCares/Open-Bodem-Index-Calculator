test_that("calc_cu works", {
  expect_equal(
    calc_copper_availability(
      A_CU_CC = seq(1,50,length.out = 7),
      B_LU_BRP = rep(3732,7),
      B_BT_AK = rep('zeeklei',7)
      ),
    expected = c(2.42,3.67,4.14,4.45,4.69,4.88,5.05),
    tolerance = 0.01
  )
  expect_equal(
    calc_copper_availability(
      A_CU_CC = seq(1,50,length.out = 7),
      B_LU_BRP = rep(265,7),
      B_BT_AK = rep('dekzand',7)
    ),
    expected = c(1,9.17,17.33,25.5,33.66,41.83,50),
    tolerance = 0.01
  )
  
})

test_that("ind_copper works", {
  expect_equal(
    ind_copper(
      D_CU = c(2.42,3.67,4.14,4.45,4.69,4.88,5.05)
    ),
    expected = c(0.42,0.599,0.6566,0.692,0.718,0.738,0.755),
    tolerance = 0.99
  )
  expect_equal(
    ind_copper(
      D_CU = c(1,9.17,17.33,25.5,33.66,41.83,50)
    ),
    expected = c(.19,0.99,1,1,1,1,1),
    tolerance = 0.99
  )
})

test_that("calc_zn works", {
  expect_equal(
    calc_zinc_availability(
      A_ZN_CC = seq(100,5000,length.out = 7),
      A_PH_CC = rep(4.5,7),
      B_LU_BRP = rep(3732,7),
      B_BT_AK = rep('zeeklei',7)
    ),
    expected = c(0.54,1.88,2.68,3.33,3.89,4.396,4.858),
    tolerance = 0.01
  )
  expect_equal(
    calc_zinc_availability(
      A_ZN_CC = c(seq(100,1000,length.out = 6),2500),
      A_PH_CC = rep(4.5,7),
      B_LU_BRP = rep(265,7),
      B_BT_AK = rep('dekzand',7)
    ),
    expected = c(3.47,6.91,9.64,12.03,14.199,16.22,29.97),
    tolerance = 1
  )
})

test_that("ind_zinc works", {
  expect_equal(
    ind_zinc(
      D_ZN = c(0.54,1.88,2.68,3.33,3.89,4.396,4.858)
    ),
    expected = c(0.007,0.0249,0.0354,0.0439,0.0512,0.0577,0.064),
    tolerance = 0.01
  )
  expect_equal(
    ind_zinc(
      D_ZN = c(3.47,6.91,9.64,12.03,14.199,16.22,29.97)
    ),
    expected = c(0.05,0.09,0.12,0.15,0.18,0.21,0.36),
    tolerance = 0.01
  )
})

