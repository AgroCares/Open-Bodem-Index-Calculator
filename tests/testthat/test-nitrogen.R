# test_that("calc_nlv works", {
#   expect_equal(
#     calc_nlv(
#       n.org = 5, 
#       c.org = 100,
#       crop = 265,
#       soiltype = "klei",
#       bulk_density = 1300, 
#       cn_ratio = 4,
#       grass.age = 2
#     ),
#     expected = 204.22254,
#     tolerance = 0.001
#   )
#   expect_equal(
#     calc_nlv(
#       n.org = c(5, 10, 25, 8),
#       c.org = c(100, 250, 300, 75),
#       crop = c(265, 265, 235, 235),
#       soiltype = c("klei", "veen", "veen", "zavel"),
#       bulk_density = c(1300, 800, 850, 1100), 
#       cn_ratio = c(4, 4, 4, 4),
#       grass.age = c(2, 8, 0, 0)
#     ),
#     expected = c(204.22254, 250, 209.43603,  52.31861),
#     tolerance = 0.001
#   )
# })
# 
# test_that("eval_nitrogen works", {
#   expect_equal(
#     eval_nitrogen(
#       value.nlv = 120
#     ),
#     expected = 1,
#     tolerance = 0.001
#   )
#   expect_equal(
#     eval_nitrogen(
#       value.nlv = seq(from = -30, to = 250, by = 50)
#     ),
#     expected = c(0, 0.3055556, 0.8263889, 1, 1, 1),
#     tolerance = 0.001
#   )
# })