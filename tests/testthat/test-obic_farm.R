# make an example set for five fields
dt <- OBIC::binnenveld[ID <=5]

out <- obic_farm(dt)

test_that("obic_farm works", {
  expect_equal(
    names(out),
    expected = c('field','farm'),
    tolerance = 0.01
  )
  expect_equal(
    length(out$field),
    expected = 35,
    tolerance = 0.01
  )
  expect_equal(
    nrow(out$farm),
    expected = 34,
    tolerance = 0.01
  )
  expect_equal(
    ncol(out$farm),
    expected = 4,
    tolerance = 0.01
  )
  expect_equal(
    unique(out$farm[,s_obi_farm_1 + s_obi_farm_2 + s_obi_farm_3]),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    out$field$S_C_OBI_A,
    expected = c(0.86,0.833,0.825,0.897,0.764),
    tolerance = 0.01
  )
  expect_equal(
    c(sum(out$farm$s_obi_farm_1),sum(out$farm$s_obi_farm_3)),
    expected = c(40,85),
    tolerance = 0.01
  )
})

# rerun with adapted and variable threshold values
out <- obic_farm(dt,
                 th_obi_c = c(0.6,0.9,1.0),
                 th_obi_p = c(0.6,0.9,1.0),
                 th_obi_b = c(0.4,0.8,1.0),
                 th_obi_e = c(0.2,0.4,1.0),
                 th_obi_m = c(0.1,0.2,1.0))

test_that("obic_farm works", {
  expect_equal(
    names(out),
    expected = c('field','farm'),
    tolerance = 0.01
  )
  expect_equal(
    length(out$field),
    expected = 35,
    tolerance = 0.01
  )
  expect_equal(
    nrow(out$farm),
    expected = 34,
    tolerance = 0.01
  )
  expect_equal(
    ncol(out$farm),
    expected = 4,
    tolerance = 0.01
  )
  expect_equal(
    unique(out$farm[,s_obi_farm_1 + s_obi_farm_2 + s_obi_farm_3]),
    expected = 5,
    tolerance = 0.01
  )
  expect_equal(
    out$field$S_C_OBI_A,
    expected = c(0.86,0.833,0.825,0.897,0.764),
    tolerance = 0.01
  )
  expect_equal(
    c(sum(out$farm$s_obi_farm_1),sum(out$farm$s_obi_farm_3)),
    expected = c(28,98),
    tolerance = 0.01
  )
})