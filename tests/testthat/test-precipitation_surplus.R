test_that("whether calc_psp works", {
  expect_equal(
    calc_psp(
      B_LU_BRP = c(265, 265, 266, 266, 235, 235),
      M_GREEN = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    ),
    expected = c(209.475, 209.475, 209.475, 209.475, 400.749, 417.695),
    tolerance = 0.01
  )
})

test_that("calc_psp works", {
  expect_equal(
    calc_psp(c(233,256,266,259,2014),rep(FALSE,5)
    ),
    expected = c(417.695, 413.397, 209.475, 375.46, 430.45),
    tolerance = 0.01
  )
  
  expect_equal(
    calc_psp(c(233,256,266,259,2014),rep(TRUE,5)
    ),
    expected = c(400.75, 374.510, 209.475, 336.577, 391.565),
    tolerance = 0.01
  )
})

test_that("calc_psp() works without start and termination arguments", {
  expect_equal(
    object = calc_psp(B_LU_BRP = 2014, M_GREEN = TRUE),
    expected = 410,
    tolerance = 1
  )
  # two years 
  expect_equal(
    object = calc_psp(B_LU_BRP = c(2014, 233), M_GREEN = c(TRUE, FALSE)),
    expected = c(410, 417),
    tolerance = 1
  )
  # two fields
  expect_equal(
    object = calc_psp(B_LU_BRP = c(2014, 233), M_GREEN = c(TRUE, FALSE)),
    expected = c(410, 417),
    tolerance = 1
  )
})

test_that("calc_psp() works with specified green manure start and termination dates", {
  expect_equal(
    object = calc_psp(B_LU_BRP = 2014, M_GREEN = TRUE, 
                           M_GREEN_START = 9L, M_GREEN_TERMINATE = 3L),
    expected = 384,
    tolerance = 1
  )
  
  expect_equal(
    object = calc_psp(B_LU_BRP = c(6794, 6794) , M_GREEN = c(TRUE, FALSE), 
                           M_GREEN_START = 8L, M_GREEN_TERMINATE = 4L),
    expected = c(573, 645),
    tolerance = 1
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
