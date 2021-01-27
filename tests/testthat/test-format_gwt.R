test_that("format_gwt works", {
  expect_equal(
    format_gwt(
      B_GT = c("II", "VI", "IIb")
    ),
    expected = c("GtII", "GtVI", "GtIIb")
  )
  expect_equal(
    format_gwt(
      B_GT = c("GtIII", "GtVI", "GtIIb")
    ),
    expected = c("GtIII", "GtVI", "GtIIb")
  )
  expect_equal(
    format_gwt(
      B_GT = c("II", "VI", "IIb", "GtIII", "GtIIIb")
    ),
    expected = c("GtII", "GtVI", "GtIIb", "GtIII", "GtIIIb")
  )
})