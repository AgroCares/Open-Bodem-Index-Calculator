test_that("calc_waterretention works", {
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = 25,
      A_SILT_MI = 15,
      A_SAND_MI = 60,
      A_OS_GV = 6,
      type = 'plant available water',
      ptf = 'Wosten1999'
      ),
    expected = 62.17101,
    tolerance = 0.01
  )
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = seq(5,90,length.out = 7),
      A_SILT_MI = rep(10,7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_OS_GV = rep(6,7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(64.02126, 63.01393, 60.43996, 59.32812, 63.05791, 71.11550, 78.72014),
    tolerance = 1
  )
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_OS_GV = rep(6,7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c(62.06244, 65.27085, 67.59961, 70.30614, 73.32562, 76.54982, 79.86564),
    tolerance = 1
  )
  
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_OS_GV = seq(1,80,length.out = 7),
      type = 'plant available water',
      ptf = 'Wosten1999'
    ),
    expected = c( 58.45007,  59.72155,  67.46154, 122.20450, 196.44924,   0.00000,   0.00000),
    tolerance = 1
  )
  
  # test for ksat
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_OS_GV = rep(6,7),
      type = 'Ksat',
      ptf = 'Wosten1999'
    ),
    expected = c(7.90, 6.96, 6.39, 5.83, 5.19, 4.46, 3.62),
    tolerance = 0.1
  )
  
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_OS_GV = seq(1,80,length.out = 7),
      type = 'Ksat',
      ptf = 'Wosten1999'
    ),
    expected = c(12.49, 11.49, 10.11,  9.51,  8.07,  5.00,  5.005),
    tolerance = .1
  )
  # water holding capacity
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_OS_GV = rep(6,7),
      type = 'water holding capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(140.6191, 145.3394, 146.8205, 147.2566, 147.0892, 146.4875, 145.5356),
    tolerance = 1
  )
  
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_OS_GV = seq(1,40,length.out = 7),
      type = 'water holding capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(136.3153, 155.2113, 163.4369, 168.5193, 175.4615, 186.5310, 201.2698),
    tolerance = 1
  )
  
  # wilting point
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_OS_GV = rep(6,7),
      type = 'wilting point',
      ptf = 'Wosten1999'
    ),
    expected = c(22.54346, 35.77212, 40.28961, 42.04489, 42.44540, 42.06602, 41.20514),
    tolerance = 1
  )
  
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_OS_GV = seq(1,40,length.out = 7),
      type = 'wilting point',
      ptf = 'Wosten1999'
    ),
    expected = c(22.69128, 48.39626, 65.88317, 74.29892, 73.90963, 63.60485, 42.51990),
    tolerance = 1
  )
  
  # field capacity
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(10,7), 
      A_SILT_MI = seq(5,90,length.out = 7),
      A_SAND_MI = 100 - 10 - seq(5,90,length.out = 7),
      A_OS_GV = rep(6,7),
      type = 'field capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(67.63037, 107.31637, 120.86883, 126.13468, 127.33621, 126.19805, 123.61542),
    tolerance = 1
  )
  
  expect_equal(
    calc_waterretention(
      A_CLAY_MI = rep(25,7), 
      A_SILT_MI = rep(5,7),
      A_SAND_MI = rep(70,7),
      A_OS_GV = seq(1,40,length.out = 7),
      type = 'field capacity',
      ptf = 'Wosten1999'
    ),
    expected = c(68.07385, 145.18877, 197.64951, 222.89675, 221.72890, 190.81454, 127.55971),
    tolerance = 1
  )
  
})

test_that("ind_waterretention works", {
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(49.4,54.13,56.98,59.99,63.25,66.72,70.32),
      type = 'plant available water'
    ),
    expected = c(0.42,0.45,0.48,0.5,0.53,0.55,0.58),
    tolerance = 0.99
  )
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(40.4,50.6,59.8,90.88,202.55,196.4,0),
      type = 'plant available water'
    ),
    expected = c(0.35,0.43,0.50,0.73,0.99,0.98,0.12),
    tolerance = 0.99
  )
  
  # K saturation
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(11.88,11.18,10.62,10.22,9.93,9.7,9.5),
      type = 'Ksat'
    ),
    expected = c(.41,0.36,0.33,0.31,0.29,0.27,0.26),
    tolerance = 0.01
  )
  
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(7.7,6.7,6.2,5.6,4.96,4.23,3.39),
      type = 'Ksat'
    ),
    expected = c(0.16,0.12,0.11,0.09,0.07,0.05,0.04),
    tolerance = 0.01
  )
  
  # water holding capacity
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(134,139,141,141,141,140,139),
      type = 'water holding capacity'
    ),
    expected = c(.91,.92,.92,.92,.92,.92,.92),
    tolerance = 0.01
  )
  
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(118,157,184,211,243.5,281,326),
      type = 'water holding capacity'
    ),
    expected = c(.85,.95,.98,.99,.996,.999,1),
    tolerance = 0.1
  )
  
  # wilting point
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(20.9,33.7,38.3,40.36,41.13,41.17,40.78),
      type = 'wilting point'
    ),
    expected = c(0.01,0.07,0.11,0.14,0.15,0.15,0.14),
    tolerance = 0.01
  )
  
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(22,65,73,38,5.18,84.9,326),
      type = 'wilting point'
    ),
    expected = c(.013,0.54,0.66,0.11,0.0003,0.79,0.9999),
    tolerance = 0.1
  )
  
  # field capacity
  expect_equal(
    ind_waterretention(
      D_P_WRI =  c(62.82,101.13,115.01,121.09,123.38,123.51,122.34),
      type = 'field capacity'
    ),
    expected = c(.5,0.9,.95,.96,.97,.97,.96),
    tolerance = 0.1
  )
  
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(66.51,195.29,217.88,112.60,15.53,254.69,978.38),
      type = 'field capacity'
    ),
    expected = c(0.56,0.999,.999,.94,.004,0.999,1),
    tolerance = 0.1
  )
})




