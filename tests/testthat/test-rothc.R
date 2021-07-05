# set directory to the right one
tloc <- if(length(list.files("../testdata/"))>0){"../testdata/"} else {'tests/testdata/'}


# Correction factors
test_that("calc_cor_factors works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_cor_factors_1.rds"))
  input <- readRDS(paste0(tloc,"calc_rotation_1.rds"))
  
  expect_equal(
    calc_cor_factors(
      A_TEMP_MEAN = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7.0,4.2),
      A_PREC_MEAN = c(70.8,63.1,57.8,41.6,59.3,70.5,85.2,83.6,77.9,81.1,80.0,83.8),
      A_ET_MEAN = c(8.5,15.5,35.3,62.4,87.3,93.3,98.3,82.7,51.7,28.0,11.3,6.5), 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3, 
      crop_cover = input$crop_cover,
      mcf = input$mcf,
      renewal = NULL),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_cor_factors_2.rds"))
  input <- readRDS(paste0(tloc,"calc_rotation_2.rds"))
  
  expect_equal(
    calc_cor_factors(
      A_TEMP_MEAN = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7.0,4.2),
      A_PREC_MEAN = c(70.8,63.1,57.8,41.6,59.3,70.5,85.2,83.6,77.9,81.1,80.0,83.8),
      A_ET_MEAN = c(8.5,15.5,35.3,62.4,87.3,93.3,98.3,82.7,51.7,28.0,11.3,6.5), 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3, 
      crop_cover = input$crop_cover,
      mcf = input$mcf,
      renewal = NULL),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 3
  sample <- readRDS(paste0(tloc,"calc_cor_factors_3.rds"))
  input <- readRDS(paste0(tloc,"calc_rotation_3.rds"))
  
  expect_equal(
    calc_cor_factors(
      A_TEMP_MEAN = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7.0,4.2),
      A_PREC_MEAN = c(70.8,63.1,57.8,41.6,59.3,70.5,85.2,83.6,77.9,81.1,80.0,83.8),
      A_ET_MEAN = c(8.5,15.5,35.3,62.4,87.3,93.3,98.3,82.7,51.7,28.0,11.3,6.5), 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3, 
      crop_cover = input$crop_cover,
      mcf = input$mcf,
      renewal = NULL),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 4
  sample <- readRDS(paste0(tloc,"calc_cor_factors_4.rds"))
  input <- readRDS(paste0(tloc,"calc_rotation_3.rds"))
  
  expect_equal(
    calc_cor_factors(
      A_TEMP_MEAN = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7.0,4.2),
      A_PREC_MEAN = c(70.8,63.1,57.8,41.6,59.3,70.5,85.2,83.6,77.9,81.1,80.0,83.8),
      A_ET_MEAN = c(8.5,15.5,35.3,62.4,87.3,93.3,98.3,82.7,51.7,28.0,11.3,6.5), 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3, 
      crop_cover = input$crop_cover,
      mcf = input$mcf,
      renewal = c(2,4)),
    sample,
    tolerance = 0.01
  )
  
})


# Carbon pools
test_that("calc_cpools works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_cpools_1.rds"))
  
  expect_equal(
    calc_cpools(
      B_SOILTYPE_AGR = "rivierklei",
      A_SOM_LOI = 3,
      A_CLAY_MI = 15,
      A_DEPTH = 0.3,
      history = 'default',
      c_fractions = c(0.0558,0.015,0.125,0.015)),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_cpools_2.rds"))
  
  expect_equal(
    calc_cpools(
      B_SOILTYPE_AGR = "dekzand",
      A_SOM_LOI = 3,
      A_CLAY_MI = 15,
      A_DEPTH = 0.3,
      history = 'manure',
      c_fractions = c(0.0558,0.015,0.125,0.015)),
    sample,
    tolerance = 0.01
  )
  
})


# Rotch
test_that("calc_rothc works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_rothc_1.rds"))
  event <- readRDS(paste0(tloc,"calc_event_1.rds"))
  cpools <- readRDS(paste0(tloc,"calc_cpools_1.rds"))
  cor_factors <- readRDS(paste0(tloc,"calc_cor_factors_1.rds"))
  
  expect_equal(
    calc_rothc(
      B_SOILTYPE_AGR = 'rivierklei', 
      A_SOM_LOI = 3, 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3,
      event = event,
      pool_size = cpools,
      cor_factors = cor_factors,
      dec_rates = c(10, 0.3, 0.66, 0.02)),
    expected = sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_rothc_2.rds"))
  event <- readRDS(paste0(tloc,"calc_event_2.rds"))
  cpools <- readRDS(paste0(tloc,"calc_cpools_1.rds"))
  cor_factors <- readRDS(paste0(tloc,"calc_cor_factors_2.rds"))
  
  expect_equal(
    calc_rothc(
      B_SOILTYPE_AGR = 'rivierklei', 
      A_SOM_LOI = 3, 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3,
      event = event,
      pool_size = cpools,
      cor_factors = cor_factors,
      dec_rates = c(10, 0.3, 0.66, 0.02)),
    expected = sample,
    tolerance = 0.01
  )
  
  # retreive sample 3
  sample <- readRDS(paste0(tloc,"calc_rothc_3.rds"))
  event <- readRDS(paste0(tloc,"calc_event_3.rds"))
  cpools <- readRDS(paste0(tloc,"calc_cpools_2.rds"))
  cor_factors <- readRDS(paste0(tloc,"calc_cor_factors_3.rds"))
  
  expect_equal(
    calc_rothc(
      B_SOILTYPE_AGR = 'rivierklei', 
      A_SOM_LOI = 3, 
      A_CLAY_MI = 15,
      A_DEPTH = 0.3,
      event = event,
      pool_size = cpools,
      cor_factors = cor_factors,
      dec_rates = c(10, 0.3, 0.66, 0.02)),
    expected = sample,
    tolerance = 0.01
  )
})

