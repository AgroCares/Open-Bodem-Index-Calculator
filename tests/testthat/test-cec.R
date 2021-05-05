test_that("calc_cec works", {
  expect_equal(
    calc_cec(
      A_CEC_CO = seq(10,500,length.out = 5)
    ),
    expected = c(10,132.5,255,377.5,500),
    tolerance = 0
  )
  expect_equal(
    calc_aggregatestability(
      A_K_CO_PO = rep(5,5),
      A_MG_CO_PO = rep(2.8,5),
      A_CA_CO_PO = seq(35,92,length.out = 5),
      A_SOM_LOI = rep(5, 5), 
      B_SOILTYPE_AGR = rep('dekzand', 5)
    ),
    expected = c(0.3625942, 0.2497810, 0.1389193, 0.0468888, 0.1053117),
    tolerance = 0.01
  )
})

test_that("eval_cec works", {
  expect_equal(
    ind_aggregatestability(
      D_AS = c(0.37,0.26,0.16,0.09,0.13)
    ),
    expected = c(0.63,0.74,0.84,0.91,0.87),
    tolerance = 0.01
  )
  expect_equal(
    ind_cec(
      D_CEC = seq(10,500,50)
    ),
    expected = c(0.1,0.6,1,1,1,1,1,1,1,1),
    tolerance = 0.01
  )
})
