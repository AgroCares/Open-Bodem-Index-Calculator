test_that("format_gwt works", {
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c('sVb', 'sVa', 'sVII', 'sVI', 'sV', 'bVII', 'bVI', 'Vb', 'Va', 'VIII', 'VII', 'VI',
      'V', 'IVu', 'IV', 'IIb', 'IIIb', 'IIIa', 'III', 'II', 'I', '-')
    ),
    expected = c("GtV", "GtV", "GtVII", 'GtVI', 'GtV', 'GtVII', 'GtVI', 'GtV', 'GtV', 'GtVIII', 'GtVII', 'GtVI',
                 'GtV', 'GtIV', 'GtIV', 'GtII', 'GtIII', 'GtIII', 'GtIII', 'GtII', 'GtI', 'GtIII')
  )
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("GtIII", "GtVI", "GtIIb")
    ),
    expected = c("GtIII", "GtVI", "GtII")
  )
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("II", "VI", "IIb", "GtIII", "GtIIIb")
    ),
    expected = c("GtII", "GtVI", "GtII", "GtIII", "GtIII")
  )
})