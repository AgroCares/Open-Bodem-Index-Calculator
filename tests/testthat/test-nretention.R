test_that("calc_nleach works", {
  # groundwater leaching
  expect_equal(
    calc_nleach(
      B_BT_AK = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = c(265, 2014, 259, 259), # grasland, bouwland, mais, mais
      B_GT = c('GtIV','GtV','GtIII','GtII'),
      D_NLV = c(5, 30, 100, 150),
      B_LG_CBS = c('Rivierengebied', 'Rivierengebied', 'Centraal Veehouderijgebied', 'Centraal Veehouderijgebied'),
      leaching_to = "gw"
    ),
    expected = c(0.41, 0.39, 0.90, 4.62),
    tolerance = 0.001
  )
  
  # surfacewater run-off
  expect_equal(
    calc_nleach(
      B_BT_AK = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = c(265, 2014, 259, 259), # grasland, bouwland, mais, mais
      B_GT = c('GtIV','GtV','GtIII','GtII'),
      D_NLV = c(5, 30, 100, 150),
      B_LG_CBS = c('Rivierengebied', 'Rivierengebied', 'Centraal Veehouderijgebied', 'Centraal Veehouderijgebied'),
      leaching_to = "ow"
    ),
    expected = c(0.095, 0.270, 0.700, 5.880),
    tolerance = 0.001
  )
})


test_that("ind_nretention gw works", {
  expect_equal(
    ind_nretention(
      D_NW = seq(from = 0, to = 70, length.out = 8),
      leaching_to ="gw"
    ),
    expected = c(0.98646511, 0.93940181, 0.79394480, 0.53088136, 0.27251898, 0.11657842, 0.04556770, 0.01715504),
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