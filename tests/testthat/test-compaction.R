test_that("ind_compaction works", {
  expect_equal(
    ind_compaction(
      B_SC_WENR = c('Zeer beperkt')
    ),
    expected = 1,
    tolerance = 0.001
  )
  expect_equal(
    ind_compaction(
      B_SC_WENR = c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                    "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                    "Beperkt", "Zeer beperkt")
    ),
    expected = c(1, 0.4, 0.2, 0.6, 1, 1, 0.8, 0.2, 0.8, 1),
    tolerance = 0.001
  )
})
