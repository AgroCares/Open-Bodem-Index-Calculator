# test_that("phosphate availability works", {
#   expect_equal(
#     calc_phosphate_availability(
#       p_al = 25, 
#       p_cacl2 = 1.5, 
#       crop = 2014
#     ),
#     expected = 1,
#     tolerance = 0.001
#   )
#   expect_equal(
#     calc_phosphate_availability(
#       p_al = c(25, 20), 
#       p_cacl2 = c(1.5, 3), 
#       crop = 2014
#     ),
#     expected = c(1, 1),
#     tolerance = 0.001
#   )
# })
