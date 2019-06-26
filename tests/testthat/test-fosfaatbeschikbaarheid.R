test_that("fosfaatbeschikbaarheid werkt", {
  expect_equal(
    fosfaatbeschikbaarheid(
      p_al = 25, 
      p_cacl2 = 1.5, 
      gewas = "gras"
    ),
    expected = 1,
    tolerance = 0.001
  )
  expect_equal(
    fosfaatbeschikbaarheid(
      p_al = c(25, 20), 
      p_cacl2 = c(1.5, 3), 
      gewas = c("gras", "mais")
    ),
    expected = c(1, 1),
    tolerance = 0.001
  )
})
