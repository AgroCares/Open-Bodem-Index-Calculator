test_that("calc_slv works", {
  expect_equal(
    # aardappel op dekzand
    calc_slv(
      A_OS_GV = 7.9,
      A_S_TOT = 563, 
      B_LU_BRP = 3732, 
      B_BT_AK ='dekzand', 
      B_LG_CBS = 'Oostelijk Veehouderijgebied',
      D_BDS = 1285),
    expected = 24.22,
    tolerance = 0.01
  )
  expect_equal(
  # aardappel op dekzand, zeeklei, rivierklei en loss
    calc_slv(
    A_OS_GV = c(7.9,4.2,6.13,6.9,3.57,39.8),
    A_S_TOT = c(563,390,614,485,236,3664), 
    B_LU_BRP = rep(3732,6), 
    B_BT_AK = c('dekzand','zeeklei','rivierklei','dalgrond','loess','veen'),
    B_LG_CBS = c('Oostelijk Veehouderijgebied','IJsselmeerpolders','Rivierengebied',
                 'Oostelijk Veehouderijgebied','Zuid-Limburg','Hollands/Utrechts Weidegebied'),
    D_BDS = c(rep(1285,5),1100)
  ),
  expected = c(24.22,20.2,33.11,20.61,9.45,150),
  tolerance = 0.1
  )
  expect_equal(
    # grasland op dekzand, zeeklei, rivierklei, dalgrond, loss en veen
    calc_slv(
      A_OS_GV = c(7.9,4.2,6.13,6.9,3.57,39.8),
      A_S_TOT = c(563,390,614,485,236,3664), 
      B_LU_BRP = rep(265,6), 
      B_BT_AK = c('dekzand','zeeklei','rivierklei','dalgrond','loess','veen'),
      B_LG_CBS = c('Oostelijk Veehouderijgebied','IJsselmeerpolders','Rivierengebied',
                   'Oostelijk Veehouderijgebied','Zuid-Limburg','Hollands/Utrechts Weidegebied'),
      D_BDS = c(1172,1136,1070,1207,1344,689)
    ),
    expected = c(11.74,7.89,11.69,10.42,5.65,44.9),
    tolerance = 0.1
  )
})
  
test_that("ind_sulpher works", {
  # aardappel op dekzand
  expect_equal(
    ind_sulpher(
      D_SLV = 22.08,
      B_LU_BRP = 3732, 
      B_BT_AK ='dekzand', 
      B_LG_CBS = 'Oostelijk Veehouderijgebied'
    ),
    expected = 1,
    tolerance = 0.01
  )
  # aardappel op dekzand, zeeklei, rivierklei en loss
  expect_equal(
    ind_sulpher(
      D_SLV = seq(1,105,length.out = 10),
      B_LU_BRP=rep(3732,10),
      B_BT_AK = rep('dekzand',10),
      B_LG_CBS = rep('Oostelijk Veehouderijgebied',10)
    ),
    expected = c(0.272,0.779,1,1,1,1,1,1,1,1),
    tolerance = 0.01
  )
  # grasland op klei
  expect_equal(
    ind_sulpher(
      D_SLV = seq(1,105,length.out = 10),
      B_LU_BRP=rep(265,10),
      B_BT_AK = rep('zeeklei',10),
      B_LG_CBS = rep('IJsselmeerpolders',10)
    ),
    expected = c(0.0408, 0.4102,rep(1,8)),
    tolerance = 0.001
  )
})
