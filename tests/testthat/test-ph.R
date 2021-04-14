test_that("calc_ph_delta works", {
  expect_equal(
    calc_ph_delta(
      A_PH_CC = 6,
      B_SOILTYPE_AGR = "rivierklei",
      A_CLAY_MI = 20,
      A_SOM_LOI = 5,
      D_CP_STARCH = 0,
      D_CP_POTATO = 0.3,
      D_CP_GRASS = 0,
      D_CP_MAIS = 0.2,
      D_CP_SUGARBEET = 0.2,
      D_CP_OTHER = 0.3,
      B_LU_BRP = 265
    ),
    expected = 0.3,
    tolerance = 0.001
  )
  expect_equal(
    calc_ph_delta(
      A_PH_CC = c(6, 4.0, 6.2, 4.5, 5.0),
      B_SOILTYPE_AGR = c("rivierklei", "veen", "veen", "loess", "dekzand"),
      A_CLAY_MI = c(20, 5, 8, 12, 5),
      A_SOM_LOI = c(5, 20, 23, 8, 10),
      D_CP_STARCH = c(0, 0.2, 0, 0.4, 0),
      D_CP_POTATO = c(0.3, 0.15, 0, 0, 0),
      D_CP_GRASS = c(0, 0.45, 0.7, 0, 0.5),
      D_CP_MAIS = c(0.1, 0.2, 0.15, 0.1, 0.1),
      D_CP_SUGARBEET = c(0.2, 0, 0.15, 0, 0),
      D_CP_OTHER = c(0.4, 0, 0, 0.5, 0),
      B_LU_BRP = c(265, 265, 265, 265, 800)
    ),
    expected = c(0.3, 01.31, 0, 0.6, 0.81),
    tolerance = 0.001
  )
})

test_that("eval_ works", {
  expect_equal(
    ind_ph(
      D_PH_DELTA = seq(from = 0, to = 1.5, by = 0.2)
    ),
    expected = c(0.9990048419, 0.9550805632, 0.5738162496, 0.1500764753, 0.0272414349, 0.0045760499, 0.0007584449, 0.0001254257),
    tolerance = 0.001
  )
})
