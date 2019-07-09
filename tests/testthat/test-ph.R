test_that("calc_ph_delta works", {
  expect_equal(
    calc_ph_delta(
      ph.cacl2 = 6,
      soiltype = "klei",
      lutum = 20, 
      om = 5,
      cp.starch = 0,
      cp.potato = 0.3,
      cp.grass = 0,
      cp.mais = 0.2,
      cp.sugarbeet = 0.2,
      cp.other = 0.3
    ),
    expected = 0.3,
    tolerance = 0.001
  )
  expect_equal(
    calc_ph_delta(
      ph.cacl2 = c(6, 5.8, 6.2, 5.6),
      soiltype = c("klei", "veen", "veen", "zavel"),
      lutum = c(20, 5, 8, 12), 
      om = c(5, 20, 23, 8),
      cp.starch = c(0, 0.2, 0, 0.4),
      cp.potato = c(0.3, 0.15, 0, 0),
      cp.grass = c(0, 0.45, 0.7, 0),
      cp.mais = c(0.1, 0.2, 0.15, 0.1),
      cp.sugarbeet = c(0.2, 0, 0.15, 0),
      cp.other = c(0.4, 0, 0, 0.5)
    ),
    expected = c(0.3, 0, 0, 0),
    tolerance = 0.001
  )
})