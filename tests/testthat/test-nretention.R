test_that("calc_nleach works", {
  # groundwater leaching
  expect_equal(
    calc_nleach(
      B_BT_AK = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = c(265, 2014, 259, 259), # grasland, bouwland, mais, mais
      B_GT = c('GtIV','GtV','GtIII','GtII'),
      D_NLV = c(5, 30, 100, 150),
      leaching_to = "gw"
    ),
    expected = c(4.1,  3.9,  9.0, 16.5),
    tolerance = 0.001
  )
  
  # surfacewater run-off
  expect_equal(
    calc_nleach(
      B_BT_AK = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = c(265, 2014, 259, 259), # grasland, bouwland, mais, mais
      B_GT = c('GtIV','GtV','GtIII','GtII'),
      D_NLV = c(5, 30, 100, 150),
      leaching_to = "ow"
    ),
    expected = c(0.95,  2.70,  7.00, 21.00),
    tolerance = 0.001
  )
})


test_that("ind_nretention gw works", {
  expect_equal(
    ind_nretention(
      D_NW = seq(from = 0, to = 70, length.out = 8),
      leaching_to ="gw"
    ),
    expected = c(0.99509412, 0.96249024, 0.82322330, 0.54303762, 0.27190229, 0.11438094, 0.04436078, 0.01664823),
    tolerance = 0.001
  )
})

test_that("ind_nretention ow works", {
  expect_equal(
    ind_nretention(
      D_NW = seq(from = 0, to = 40, length.out = 8),
      leaching_to = "ow"
    ),
    expected = c(0.97739468, 0.88195626, 0.65926244, 0.39340009, 0.19856821, 0.09129823, 0.04013735, 0.01729469),
    tolerance = 0.001
  )
})