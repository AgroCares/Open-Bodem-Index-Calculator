test_that("calc_nleach works", {
  # groundwater leaching
  expect_equal(
    calc_nleach(
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = c(265, 2014, 259, 259), # grasland, bouwland, mais, mais
      B_GWL_CLASS = c('IV','V','III','II'),
      D_NLV = c(5, 30, 100, 150),
      B_AER_CBS = c('Rivierengebied', 'Rivierengebied', 'Centraal Veehouderijgebied', 'Centraal Veehouderijgebied'),
      leaching_to = "gw"
    ),
    expected = c(0.41, 0.39, 5.77, 15.64),
    tolerance = 0.001
  )
  
  # add check for groundwater leaching
  expect_equal(
    calc_nleach(
      B_SOILTYPE_AGR = 'dekzand',
      B_LU_BRP = 265, 
      B_GWL_CLASS = 'VII',
      D_NLV = 135,
      B_AER_CBS = 'Centraal Veehouderijgebied',
      leaching_to = "gw"
    ),
    expected = 11.88,
    tolerance = 0.001
  )
  
  # surfacewater run-off
  expect_equal(
    calc_nleach(
      B_SOILTYPE_AGR = c('dekzand','zeeklei','rivierklei','loess'),
      B_LU_BRP = c(265, 2014, 259, 259), # grasland, bouwland, mais, mais
      B_GWL_CLASS = c('IV','V','III','II'),
      D_NLV = c(5, 30, 100, 150),
      B_AER_CBS = c('Rivierengebied', 'Rivierengebied', 'Centraal Veehouderijgebied', 'Centraal Veehouderijgebied'),
      leaching_to = "ow"
    ),
    expected = c(0.095, 0.270, 4.49, 19.90),
    tolerance = 0.001
  )
})


test_that("ind_nretention gw works", {
  expect_equal(
    ind_nretention(
      D_NW = seq(from = 0, to = 70, length.out = 8),
      leaching_to ="gw"
    ),
    expected = c(9.890428e-01, 6.874900e-01, 5.530339e-02, 1.595222e-03, 4.365378e-05, 1.192833e-06, 3.259268e-08, 8.905534e-10),
    tolerance = 0.001
  )
})

test_that("ind_nretention ow works", {
  expect_equal(
    ind_nretention(
      D_NW = seq(from = 0, to = 40, length.out = 8),
      leaching_to = "ow"
    ),
    expected = c( 9.749461e-01, 4.992744e-01, 3.997841e-02, 1.890118e-03, 8.650998e-05, 0, 0, 0),
    tolerance = 0.001
  )
})
