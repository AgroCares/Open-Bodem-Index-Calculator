test_that("format_gwt works", {
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c('sVb', 'sVII', 'sVI', 'sV', 'bVII', 'bVI', 'Vb', 'Va', 'VIII', 'VII', 'VI',
      'V', 'IVu', 'IV', 'IIb', 'IIIb', 'IIIa', 'III', 'II', 'I', '-')
    ),
    expected = c('sVb', 'sVII', 'sVI', 'sV', 'bVII', 'bVI', 'Vb', 'Va', 'VIII', 'VII', 'VI',
                 'V', 'IVu', 'IV', 'IIb', 'IIIb', 'IIIa', 'III', 'II', 'I', 'III')
  )
  expect_error(
    format_gwt(B_GWL_CLASS = 'GtIII')
  )
})

test_that("format_gwt() reassigns unknown classes", {
  # expect gt 3 for non Zuid-Limburg
  expect_equal(
    format_gwt(B_GWL_CLASS = '-', B_AER_CBS = 'Zuidwest-Brabant'),
    'III'
  )
  expect_equal(
    format_gwt(B_GWL_CLASS = '-', B_AER_CBS = 'Zuidwest-Brabant'),
    'III'
  )
  
  # expect gt 8 for Zuid-limburg
  expect_equal(
    format_gwt(B_GWL_CLASS = '-', B_AER_CBS = 'Zuid-Limburg'),
    'VIII'
  )
  expect_equal(
    format_gwt(B_GWL_CLASS = '-', B_AER_CBS = 'Zuid-Limburg'),
    'VIII'
  )
})
