test_that("whether calc_permeability works", {
  expect_equal(
    calc_permeability(A_CLAY_MI = c(5,5,20,20,40,40),
                      A_SAND_MI = c(20,40,20,40,10,30),
                      A_SILT_MI = c(50,20,30,10,30,10),
                      A_SOM_LOI = c(3,10,3,10,3,10)
                      ),
    expected = c(8023.31,37.56,69.90,156.66,55.69,58.41),
    tolerance = 0.01
  )
  
})


test_that("whether ind_permeability works", {
  expect_equal(
    ind_permeability(D_WRI_K = c(40,60,80,100)
    ),
    expected = c(0.054,0.395, 0.805, 0.9556),
    tolerance = 0.001
  )
  
})

