test_that("eval_structure works", {
  expect_equal(
    eval_structure(
      eval.crumbleability = 0.5,
      eval.sealing = 0.5
    ),
    expected = 0.5,
    tolerance = 0.001
  )
  expect_equal(
    eval_structure(
      eval.crumbleability = seq(from  = 0, to = 1, by = 0.1),
      eval.sealing = seq(from  = 1, to = 0, by = -0.1)
    ),
    expected = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    tolerance = 0.001
  )
})