test_that("calc_depth works", {
  expect_equal(
    calc_root_depth(
      B_LU_BRP = 265
    ),
    expected = 0.1,
    tolerance = 0.001
  )
  expect_equal(
    calc_root_depth(
      B_LU_BRP = c(265, 2719, 266, 1079)
    ),
    expected = c(0.1, 0.25, 0.1, 0.25),
    tolerance = 0.001
  )
})