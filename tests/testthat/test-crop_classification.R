test_that("crop classification works", {
  expect_equal(
    calc_cropclass(
      B_LU_BRP = 235,
      B_SOILTYPE_AGR = 'dekzand', 
      nutrient = 'P'
      ),
    expected = 'class3'
  )
  # chinese kool (class 0), aardappel (class 1), suikerbiet (class 2), gerst (class 3), boerenkool (class 4)
  # faunarand gras (3720) en engels raai (3506)
  expect_equal(
    calc_cropclass(
      B_LU_BRP = c(2721,2951,256,236,2715,3720,3506,3506,3506),
      B_SOILTYPE_AGR = c('dekzand','duinzand','zeeklei','rivierklei','maasklei','dalgrond','moerige_klei','veen','loess'), 
      nutrient = c('P')
    ),
    expected = c('class0','class1','class2','class3','class4','natuur','gras','gras','gras')
  )
  #witte kool (class 1), aardappel (class 1), suikerbiet (class 1), gerst (class 4), boerenkool (class 1)
  # fabrieksaardappel, bloemkool (Class 2), voederbieten (Class 3) en graszaad en mais (class 4)
  expect_equal(
    calc_cropclass(
      B_LU_BRP = c(2789,2951,256,236,2715,3732,2713,257,383,814),
      B_SOILTYPE_AGR = rep('dekzand',10), 
      nutrient = c('K')
    ),
    expected = c('class1','class1','class1','class4','class1','class2','class2','class3','class4','class4')
  )
  
  # chinese kool (class 2), aardappel (class 3), suikerbiet (class 4), gerst (class 3), boerenkool (class 3)
  # fabrieksaardappel (class3), bloemkool (Class 2), voederbieten (Class 3) en spruitkool (class1) en mais (class 3)
  expect_equal(
    calc_cropclass(
      B_LU_BRP = c(2721,2951,256,236,2715,3732,2713,257,2777,814),
      B_SOILTYPE_AGR = rep('dekzand',10), 
      nutrient = c('S')
    ),
    expected = c('class2','class3','class4','class3','class3','class3','class2','class4','class1','class3')
  )
  
})
