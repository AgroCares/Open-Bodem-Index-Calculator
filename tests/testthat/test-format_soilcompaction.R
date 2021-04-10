test_that("format_soilcompaction works", {
  expect_equal(
    format_soilcompaction(
      B_SC_WENR = c(1, 2, 3, 4, 5, 10, 11, 401, 901, 902)
      ),
    expected = c("Zeer beperkt", "Beperkt", "Matig", "Groot", "Zeer groot",
                 "Beperkt door veenlagen", "Van nature dicht", "Glastuinbouw, niet beoordeeld",
                 "Bebouwing en infrastructuur", "Water")
  )
  expect_equal(
    format_soilcompaction(
     B_SC_WENR = c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                   "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                   "Beperkt", "Zeer beperkt")
   ),
    expected = c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                 "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                 "Beperkt", "Zeer beperkt")
  )
})