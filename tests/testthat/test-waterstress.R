test_that("calc_waterstressindex works", {
  expect_equal(
    calc_waterstressindex(
      B_HELP_WENR= c('gMn25C','bMn15A','gMn25C','bMn15A','gMn25C','bMn15A'),
      B_LU_BRP = c(3732,265,258,172,343,2709),
      B_GWL_CLASS = c('GtIV','GtV','GtIII','GtII','GtVI','GtIV')
    ),
    expected = c(5,9,0,0,0,5),
    tolerance = .1
  )
})

test_that("ind_waterstressindex works", {
  expect_equal(
    ind_waterstressindex(
      D_WSI = c(5,9,0,0,0,5)
    ),
    expected = (100-c(5,9,0,0,0,5))/100,
    tolerance = .1
  )
})
