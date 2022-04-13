test_that("whether calc_psp works", {
  expect_equal(
    calc_psp(
      B_LU_BRP = c(265, 265, 266, 266, 235, 235),
      M_GREEN = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    ),
    expected = c(220.993, 228.180, 220.993, 246.870, 403.549, 417.695),
    tolerance = 0.01
  )
})