test_that("calc_crumbleability works", {
  expect_equal(
    calc_crumbleability(
      lutum = 10, 
      om = 5, 
      ph = 6
    ),
    expected = 9.3,
    tolerance = 0.001
  )
  expect_equal(
    calc_crumbleability(
      lutum = c(2, 4, 45), 
      om = c(5, 5, 5), 
      ph = c(6, 6, 6)
    ), 
    expected = c(10, 10, 1),
    tolerance = 0.001
  )
})

# test_that("eval_crumbleability works", {
#   expect_equal(
#     eval_crumbleability(
#       value.crumbleability = 10, 
#       crop = 2014
#     ),
#     expected = 9.3,
#     tolerance = 0.001
#   )
#   expect_equal(
#     eval_crumbleability(
#       value.crumbleability = c(1, 5, 10), 
#       crop = c(2555, 2334)
#     ), 
#     expected = c(10, 1),
#     tolerance = 0.001
#   )
# })
