test_that("calc_psp works", {
  expect_equal(
    calc_psp(c(233,256,266,259,2014),rep(FALSE,5)
    ),
    expected = c(417.695, 413.397, 209.475, 354.146, 387.193),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_psp(c(233,256,266,259,2014),rep(TRUE,5)
    ),
    expected = c(396.377, 370.138, 239.698, 332.205, 387.193),
    tolerance = 0.01
  )
})


test_that("ind_psp works", {
  expect_equal(
    ind_psp(c(100,200,300,350,400),rep(233,5)
    ),
    expected = c(0.01831531, 0.13497224, 0.75785828, 0.96893680, 0.99731747),
    tolerance = 0.001
  )
  
  
  expect_equal(
    ind_psp(c(50,100,200,250,300),rep(265,5)
    ),
    expected = c(0.006737937, 0.018315306, 0.134972242, 0.356451928, 0.757858283),
    tolerance = 0.001
  )
  
})
