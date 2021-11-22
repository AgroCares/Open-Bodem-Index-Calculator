test_that("calc_psp works", {
  expect_equal(
    calc_psp(c(233,256,266,259,2014),rep(FALSE,5)
    ),
    expected = c(417.70, 413.40, 209.48, 354.15, 387.19),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_psp(c(233,256,266,259,2014),rep(TRUE,5)
    ),
    expected = c(396.38, 370.14, 239.70, 332.21, 387.19),
    tolerance = 0.01
  )
})


test_that("ind_psp works", {
  expect_equal(
    ind_psp(c(100,200,300,350,400),rep(233,5)
    ),
    expected = c(0.018, 0.135, 0.758, 0.969, 0.997),
    tolerance = 0.001
  )
  
  
  expect_equal(
    ind_psp(c(50,100,200,250,300),rep(265,5)
    ),
    expected = c(0.07406955, 0.19790332, 0.83064703, 0.97091494, 0.99591035),
    tolerance = 0.001
  )
  
})
