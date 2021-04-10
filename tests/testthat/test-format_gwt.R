test_that("format_gwt works", {
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("II", "VI", "IIb")
    ),
    expected = c("GtII", "GtVI", "GtIIb")
  )
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("GtIII", "GtVI", "GtIIb")
    ),
    expected = c("GtIII", "GtVI", "GtIIb")
  )
  expect_equal(
    format_gwt(
      B_GWL_CLASS = c("II", "VI", "IIb", "GtIII", "GtIIIb")
    ),
    expected = c("GtII", "GtVI", "GtIIb", "GtIII", "GtIIIb")
  )
})