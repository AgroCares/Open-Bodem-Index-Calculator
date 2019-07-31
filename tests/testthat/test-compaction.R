test_that("ind_compaction works", {
  expect_equal(
    ind_compaction(
      B_OV_WENR = c('bebouwing','water','glastuinbouw','zeer beperkt','beperkt','matig','groot','zeer groot','van nature verdicht')
    ),
    expected = c(1,1,1,1,0.8,0.6,0.4,0.2,0.2),
    tolerance = 0.001
  )
  expect_equal(
    ind_compaction(
      B_OV_WENR = c('zeer beperkt')
    ),
    expected = 1,
    tolerance = 0.001
  )
})
