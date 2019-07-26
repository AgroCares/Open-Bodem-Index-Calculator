test_that("calc_ph_delta works", {
  expect_equal(
    calc_ph_delta(
      A_PH_CC = 6,
      B_BT_AK = "rivierklei",
      A_CLAY_MI = 20,
      A_OS_GV = 5,
      D_CP_STARCH = 0,
      D_CP_POTATO = 0.3,
      D_CP_GRASS = 0,
      D_CP_MAIS = 0.2,
      D_CP_SUGARBEET = 0.2,
      D_CP_OTHER = 0.3
    ),
    expected = 0.3,
    tolerance = 0.001
  )
  expect_equal(
    calc_ph_delta(
      A_PH_CC = c(6, 5.8, 6.2, 4.5),
      B_BT_AK = c("rivierklei", "veen", "veen", "loess"),
      A_CLAY_MI = c(20, 5, 8, 12),
      A_OS_GV = c(5, 20, 23, 8),
      D_CP_STARCH = c(0, 0.2, 0, 0.4),
      D_CP_POTATO = c(0.3, 0.15, 0, 0),
      D_CP_GRASS = c(0, 0.45, 0.7, 0),
      D_CP_MAIS = c(0.1, 0.2, 0.15, 0.1),
      D_CP_SUGARBEET = c(0.2, 0, 0.15, 0),
      D_CP_OTHER = c(0.4, 0, 0, 0.5)
    ),
    expected = c(0.3, 0, 0, 0.6),
    tolerance = 0.001
  )
})

test_that("eval_ works", {
  expect_equal(
    ind_ph(
      D_PH_DELTA = seq(from = 0, to = 1.5, by = 0.2)
    ),
    expected = c(0.0009951581, 0.0449194368, 0.4261837504, 0.8499235247, 0.9727585651, 0.9954239501, 0.9992415551, 0.9998745743),
    tolerance = 0.001
  )
})