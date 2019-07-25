test_that("calc_grass_age works", {
  expect_equal(
    calc_rotation_fraction(
      ID = c(1, 1, 1, 1, 1),
      B_LU_BRP = c(265, 265, 1079, 1079, 1079),
      crop = "grass"
    ),
    expected = c(0.4, 0.4, 0.4, 0.4, 0.4),
    tolerance = 0.1
  )
  expect_equal(
    calc_rotation_fraction(
      ID = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
      B_LU_BRP = c(265, 265, 1079, 1079, 1079, 237,237, 265, 265, 265, 308, 308, 265),
      crop = "grass"
    ),
    expected = c(0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    tolerance = 0.1
  )
})