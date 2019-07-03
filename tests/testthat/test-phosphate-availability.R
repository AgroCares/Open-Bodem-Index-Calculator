test_that("calc_phosphate_availability works", {
  expect_equal(
    calc_phosphate_availability(
      p_al = 25,
      p_cacl2 = 1.5,
      crop = 2014
    ),
    expected = 2.333333,
    tolerance = 0.001
  )
  expect_equal(
    calc_phosphate_availability(
      p_al = c(25, 20),
      p_cacl2 = c(1.5, 3),
      crop = c(2014, 265)
    ),
    expected = c(2.333333, 4.986531),
    tolerance = 0.001
  )
})

test_that("eval_phosphate_availability works", {
  expect_equal(
    eval_phosphate_availability(
      value.phosphate.availability = 2
    ),
    expected = 0.3804136,
    tolerance = 0.001
  )
  expect_equal(
    eval_phosphate_availability(
      value.phosphate.availability = 0:7
    ),
    expected = c(0.004930119, 0.074904299, 0.380413642, 0.742746233, 0.919277030, 0.977081441, 0.993682642, 0.998272957),
    tolerance = 0.001
  )
})