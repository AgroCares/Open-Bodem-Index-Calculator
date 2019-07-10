test_that("calc_nlv works", {
  expect_equal(
    calc_nlv(
      n.org = 5, 
      c.org = 100,
      crop = 265,
      soiltype = "klei",
      bulk_density = 1300, 
      grass.age = 2
    ),
    expected = 0.3,
    tolerance = 0.001
  )
  expect_equal(
    calc_nlv(
      n.org = c(5, 10, 25, 8),
      c.org = c(100, 250, 300, 75),
      crop = c(265, 265, 235, 235),
      soiltype = c("klei", "veen", "veen", "zavel"),
      bulk_density = c(1300, 800, 850, 1100), 
      grass.age = c(2, 8, 0, 0)
    ),
    expected = c(0.3, 0, 0, 0),
    tolerance = 0.001
  )
})