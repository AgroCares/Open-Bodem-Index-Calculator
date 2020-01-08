test_that("calc_cec works", {
  expect_equal(
    calc_cec(
      A_CEC_CO = seq(10,500,length.out = 5),
      A_K_CEC = rep(5,5),
      A_MG_CEC = rep(2.8,5),
      A_CA_CEC = seq(35,92,length.out = 5),
      A_OS_GV = rep(2,5),
      B_BT_AK = c('dekzand','dekzand','maasklei','veen','loess'),
      advice='fertility_index'
    ),
    expected = c(10,132.5,255,377.5,500),
    tolerance = 0
  )
  expect_equal(
    calc_cec(
      A_CEC_CO = rep(10,5),
      A_K_CEC = rep(5,5),
      A_MG_CEC = rep(2.8,5),
      A_CA_CEC = seq(35,92,length.out = 5),
      A_OS_GV = rep(2,5),
      B_BT_AK = c('dekzand','dekzand','maasklei','veen','loess'),
      advice='structure_index'
    ),
    expected = c(0.36,0.25,0.18,0.07,0.07),
    tolerance = 0.01
  )
})

test_that("eval_cec works", {
  expect_equal(
    ind_cec(
      D_CEC = c(0.37,0.26,0.16,0.09,0.13),
      advice='structure_index'
    ),
    expected = c(0.63,0.74,0.84,0.91,0.87),
    tolerance = 0.01
  )
  expect_equal(
    ind_cec(
      D_CEC = seq(10,500,50),
      advice='fertility_index'
    ),
    expected = c(0.1,0.6,1,1,1,1,1,1,1,1),
    tolerance = 0.01
  )
})
