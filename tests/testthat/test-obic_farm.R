# make an example set for five fields
dt <- OBIC::binnenveld[ID <=5]

test_that("obic_farm works with extra indicators", {
  out <- obic_farm(dt, useClassicOBI = FALSE)
  
  expect_equal(
    names(out),
    expected = c('fields','farm'),
    tolerance = 0.01
  )
  expect_equal(
    length(out$field),
    expected = 40,
    tolerance = 0.01
  )
  expect_equal(
    length(out$farm),
    expected = 2,
    tolerance = 0.01
  ) 
  expect_equal(
    nrow(out$farm$indicators),
    expected = 33,
    tolerance = 0.01
  )
  expect_equal(
    nrow(out$farm$scores),
    expected = 6,
    tolerance = 0.01
  )
  expect_equal(
    ncol(out$farm$indicators),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    ncol(out$farm$scores),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    unique(out$farm$indicators[,S_OBI_NFIELDS_LOW + S_OBI_NFIELDS_MEDIUM + S_OBI_NFIELDS_HIGH]),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    unique(out$farm$scores[,S_OBI_NFIELDS_LOW + S_OBI_NFIELDS_MEDIUM + S_OBI_NFIELDS_HIGH]),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    out$field$S_C_OBI_A,
    expected = c(0.86,0.833,0.825,0.897,0.764),
    tolerance = 0.01
  )
  expect_equal(
    c(sum(out$farm$indicators$S_OBI_NFIELDS_LOW),
      sum(out$farm$indicators$S_OBI_NFIELDS_HIGH)),
    expected = c(48, 86),
    tolerance = 0.01
  )
  expect_equal(
    c(sum(out$farm$scores$S_OBI_NFIELDS_LOW),sum(out$farm$scores$S_OBI_NFIELDS_HIGH)),
    expected = c(10, 10),
    tolerance = 0.01
  )
})

test_that("obic_farm works with extra indicators", {
  out <- obic_farm(dt, useClassicOBI = TRUE)
  
  expect_equal(
    names(out),
    expected = c('fields','farm'),
    tolerance = 0.01
  )
  expect_equal(
    length(out$field),
    expected = 35,
    tolerance = 0.01
  )
  expect_equal(
    length(out$farm),
    expected = 2,
    tolerance = 0.01
  ) 
  expect_equal(
    nrow(out$farm$indicators),
    expected = 28,
    tolerance = 0.01
  )
  expect_equal(
    nrow(out$farm$scores),
    expected = 6,
    tolerance = 0.01
  )
  expect_equal(
    ncol(out$farm$indicators),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    ncol(out$farm$scores),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    unique(out$farm$indicators[,S_OBI_NFIELDS_LOW + S_OBI_NFIELDS_MEDIUM + S_OBI_NFIELDS_HIGH]),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    unique(out$farm$scores[,S_OBI_NFIELDS_LOW + S_OBI_NFIELDS_MEDIUM + S_OBI_NFIELDS_HIGH]),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    out$field$S_C_OBI_A,
    expected = c(0.86,0.833,0.825,0.897,0.764),
    tolerance = 0.01
  )
  expect_equal(
    c(sum(out$farm$indicators$S_OBI_NFIELDS_LOW),sum(out$farm$indicators$S_OBI_NFIELDS_HIGH)),
    expected = c(34, 77),
    tolerance = 0.01
  )
  expect_equal(
    c(sum(out$farm$scores$S_OBI_NFIELDS_LOW),sum(out$farm$scores$S_OBI_NFIELDS_HIGH)),
    expected = c(8,14),
    tolerance = 0.01
  )
})
