test_that("calc_workability works", {
  expect_equal(
    calc_waterstressindex(
      A_CLAY_MI = c(28, 28, 28, 28, 28, 28, 10, 0),
      A_SILT_MI = c(20, 20, 20, 20, 20, 20, 20, 14),
      B_LU_BRP = c(238, 240, 370, 238, 980, 259, 265, 1929),
      B_BT_AK = c('zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'zeeklei', 'loess', 'veen','dekzand'),
      B_GLG = c(90, 90, 90, 80, 90, 80, 105, 150),
      B_GHG = c(35, 35, 35, 30, 35, 50, 15, 25)
    ),
    expected = c(0.75,0.90,1,1,0.82, 1, 0.635,0.546),
    tolerance = .01
  )
})