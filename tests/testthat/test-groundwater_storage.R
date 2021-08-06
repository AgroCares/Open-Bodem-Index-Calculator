test_that("test whether calc_groundwater_recharge works", {
  expect_equal(
    ind_gw_storage(D_WRI_WHC = c(0.1,0.3,0.4,0.5),
                   I_P_SE = rep(0.8,4),
                   B_COMPACTION = rep(FALSE,4),
                   B_DRAINAGE = rep(FALSE,4)
                   ),
    expected = 0.6005033,
    tolerance = 0.01
  )
  
  expect_equal(
    ind_gw_storage(D_WRI_WHC = c(0.1,0.3,0.4,0.5),
                   I_P_SE = rep(0.8,4),
                   B_COMPACTION = rep(TRUE,4),
                   B_DRAINAGE = rep(FALSE,4)
                   ),
    expected = 0.4804026,
    tolerance = 0.01
  )
  
  expect_equal(
    ind_gw_storage(D_WRI_WHC = c(0.1,0.3,0.4,0.5),
                   I_P_SE = rep(0.8,4),
                   B_COMPACTION = FALSE,
                   B_DRAINAGE = TRUE
                   ),
    expected = 0.360302,
    tolerance = 0.01
  )
  
})
