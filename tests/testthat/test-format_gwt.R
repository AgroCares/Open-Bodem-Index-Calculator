test_that("format_gwt works", {
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c('sVb', 'sVa', 'sVII', 'sVI', 'sV', 'bVII', 'bVI', 'Vb', 'Va', 'VIII', 'VII', 'VI',
      'V', 'IVu', 'IV', 'IIb', 'IIIb', 'IIIa', 'III', 'II', 'I', '-')
    ),
    expected = c("V", "V", "VII", 'VI', 'V', 'VII', 'VI', 'V', 'V', 'VIII', 'VII', 'VI',
                 'V', 'IV', 'IV', 'II', 'III', 'III', 'III', 'II', 'I', 'III')
  )
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("III", "VI", "IIb")
    ),
    expected = c("III", "VI", "II")
  )
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("II", "VI", "IIb", "III", "IIIb")
    ),
    expected = c("II", "VI", "II", "III", "III")
  )
})
