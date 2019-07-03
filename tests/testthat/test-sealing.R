test_that("calc_sealing works", {
  expect_equal(
    calc_sealing(
      lutum = 20, 
      om = 5
    ),
    expected = 8.625,
    tolerance = 0.001
  )
  expect_equal(
    calc_sealing(
      lutum = c(25, 20),
      om = c(1.5, 8)
    ),
    expected = c(8.6, 10.5),
    tolerance = 0.001
  )
})

test_that("eval_sealing works", {
  expect_equal(
    eval_sealing(
      value.sealing = 8.6, 
      crop = 2014
    ),
    expected = 0.9999,
    tolerance = 0.001
  )
  expect_equal(
    eval_sealing(
      value.sealing = c(8.6, 2),
      crop = c(2014, 265)
    ),
    expected = c(0.9999, 1),
    tolerance = 0.001
  )
})