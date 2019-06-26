test_that("verkruimelbaarheid werkt", {
  expect_equal(
    verkruimelbaarheid(
      lutum = 10, 
      leem = 3, 
      os = 5, 
      ph = 6
    ),
    expected = 9,
    tolerance = 0.001
  )
  expect_equal(
    verkruimelbaarheid(
      lutum = c(2, 4, 45), 
      leem = c(3, 3, 3), 
      os = c(5, 5, 5), 
      ph = c(6, 6, 6)
    ), 
    expected = c(10, 11.58333, 1),
    tolerance = 0.001
  )
})
