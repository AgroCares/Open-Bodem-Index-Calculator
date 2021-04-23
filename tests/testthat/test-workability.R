test_that("calc_workability works", {
  expect_equal(
    calc_workability(
      A_CLAY_MI = c(15.6,13.6, 4.3, 22.6, 1.9, 2.9, 3.1, 4.3, 15.6),
      A_SILT_MI = c(16.7,30.5, 11.8, 36.6, 9.2, 8.6, 10.6, 11.8, 16.7),
      B_LU_BRP = c(233, 234, 236, 256, 259, 265, 265, 317, 2014),
      B_SOILTYPE_AGR = c('zeeklei','zeeklei', 'dekzand','zeeklei', 'dekzand', 'dekzand', 'veen', 'dekzand', 'zeeklei'),
      B_GWL_GLG = c(173,139, 106, 144, 115, 113, 42, 106, 173),
      B_GWL_GHG = c(21, 18, 62, 70, 49, 81, 9, 62, 21),
      B_Z_TWO = c(400, 400, 400, 400, 400, 400, 400, 400, 400)
    ),
    expected = c(0.06,0.01,0.04,0.79,0.62, 0.98, 0.00,0.46,0.20),
    tolerance = .01
  )
})
