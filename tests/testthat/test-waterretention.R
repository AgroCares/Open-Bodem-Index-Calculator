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
    expected = c(0.4687305, 0.4844646, 0.4894018, 0.4908554, 0.4902973, 0.4882918, 0.4851186),
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
    expected = c(0.4543842, 0.5173710, 0.5447898, 0.5617310, 0.5848718, 0.6217699, 0.6708992),
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
    expected = c(0.6337925, 0.7061359, 0.7456985, 0.7836507, 0.8200783, 0.8535175, 0.8826870),
    tolerance = 0.01
  )
  expect_equal(
    ind_waterretention(
      D_P_WRI = c(40.4,50.6,59.8,90.88,202.55,196.4,0),
      type = 'plant available water'
    ),
    expected = c(0.48335824, 0.65282670, 0.78137645, 0.97036472, 0.99999013, 0.99998463, 0.06508809),
    tolerance = 0.01
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
      D_P_WRI = c(0.1,0.25,0.5,0.75,1),
      type = 'water holding capacity'
    ),
    expected = c(0.012, 0.205, 0.850, 0.986, 0.998),
    tolerance = 0.01
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




