test_that("calc_organic_carbon works", {
  expect_equal(
    calc_organic_carbon(
      A_OS_GV = 5,
      D_BDS = 1000, 
      D_RD = 0.3
    ),
    expected = 8700000,
    tolerance = 10
  )
  expect_equal(
    calc_organic_carbon(
      A_OS_GV = c(5, 10, 15, 20, 25),
      D_BDS = c(1300, 1100, 1000, 900, 800) , 
      D_RD = c(0.3, 0.3, 0.1, 0.1, 0.1)
    ),
    expected = c(11310000, 19140000, 8700000, 10440000, 11600000),
    tolerance = 10
  )
})