# set directory to the right one
tloc <- if(length(list.files("../testdata/"))>0){"../testdata/"} else {'tests/testdata/'}


# Cabon input
test_that("calc_carbon_input works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_carbon_input_1.rds"))
  
  expect_equal(
    calc_carbon_input(
      B_LU_BRP = c(256,259,2014,233,234,256,259,2014),
      A_P_AL = 40,
      A_P_WA = 30,
      M_GREEN = FALSE,
      effectivity = TRUE,
      manure_type = 'slurry',
      manure_in = NULL,
      compost_in = c(0,1500,1500,0,0,1500,1500,0)),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_carbon_input_2.rds"))
  
  expect_equal(
    calc_carbon_input(
      B_LU_BRP = c(256,259,2014,233,234,265,259,2014,233,234),
      A_P_AL = 40,
      A_P_WA = 30,
      M_GREEN = TRUE,
      effectivity = c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE),
      manure_type = 'solid',
      manure_in = NULL,
      compost_in = c(0,1500,1500,0,0,1500,1500,1500,0,0)),
    sample,
    tolerance = 0.01
  )
  
  
  # retreive sample 3
  sample <- readRDS(paste0(tloc,"calc_carbon_input_3.rds"))
  
  expect_equal(
    calc_carbon_input(
      B_LU_BRP = c(259,266,265,266,259),
      A_P_AL = 40,
      A_P_WA = 30,
      M_GREEN = TRUE,
      effectivity = TRUE,
      manure_type = 'slurry',
      manure_in = NULL,
      compost_in = 0),
    sample,
    tolerance = 0.01
  )
  
})


# Events
test_that("calc_event_current works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_event_1.rds"))
  input <- readRDS(paste0(tloc,"calc_carbon_input_1.rds"))
  
  expect_equal(
    calc_events_current(
      B_LU_BRP = c(256,259,2014,233,234,256,259,2014),
      manure_in = input$manure_in,
      compost_in = input$compost_in,
      catchcrop = input$catchcrop,
      grass_fertilization = 1,
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_event_2.rds"))
  input <- readRDS(paste0(tloc,"calc_carbon_input_2.rds"))
  
  expect_equal(
    calc_events_current(
      B_LU_BRP = c(256,259,2014,233,234,265,259,2014,233,234),
      manure_in = input$manure_in,
      compost_in = input$compost_in,
      catchcrop = input$catchcrop,
      grass_fertilization = 1,
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 3
  sample <- readRDS(paste0(tloc,"calc_event_3.rds"))
  input <- readRDS(paste0(tloc,"calc_carbon_input_3.rds"))
  
  expect_equal(
    calc_events_current(
      B_LU_BRP = c(259,266,265,266,259),
      manure_in = input$manure_in,
      compost_in = input$compost_in,
      catchcrop = input$catchcrop,
      grass_fertilization = 1,
      simyears = 50),
    sample,
    tolerance = 0.01
  )
})


# Minimal events
test_that("calc_event_minimal works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_event_minimal_1.rds"))
  
  expect_equal(
    calc_events_minimal(
      B_LU_BRP = c(256,259,2014,233,234,256,259,2014),
      catchcrop = rep(0,8),
      grass_fertilization = 1,
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_event_minimal_2.rds"))
  
  expect_equal(
    calc_events_minimal(
      B_LU_BRP = c(259,266,265,266,259),
      catchcrop = rep(0,5),
      grass_fertilization = 3,
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
})


# Rotations
test_that("calc_rotation works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_rotation_1.rds"))
  
  expect_equal(
    calc_crop_rotation(
      B_LU_BRP = c(256,259,2014,233,234,256,259,2014),
      M_GREEN = FALSE,
      effectivity = FALSE),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_rotation_2.rds"))
  
  expect_equal(
    calc_crop_rotation(
      B_LU_BRP = c(256,259,2014,233,234,256,259,2014,233,234),
      M_GREEN = TRUE,
      effectivity = c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE)),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 3
  sample <- readRDS(paste0(tloc,"calc_rotation_3.rds"))
  
  expect_equal(
    calc_crop_rotation(
      B_LU_BRP = c(259,266,265,266,259),
      M_GREEN = TRUE,
      effectivity = TRUE),
    sample,
    tolerance = 0.01
  )
})


# Wrap function
test_that("calc_c_seq_field works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"wrap_1.rds"))
  
  expect_equal(
    calc_c_seq_field(
      B_LU_BRP = c(256,259,2014,233,234,256,259,2014),
      B_SOILTYPE_AGR = 'rivierklei',
      A_SOM_LOI = 3,
      A_CLAY_MI = 15,
      A_P_AL = 40,
      A_P_WA = 30,
      A_DEPTH = 0.3,
      A_TEMP_MEAN = NULL,
      A_PREC_MEAN = NULL,
      A_ET_MEAN = NULL,
      M_GREEN = FALSE,
      effectivity = TRUE,
      manure_in = NULL,
      compost_in = 0,
      manure_type = "slurry",
      history = "default",
      renewal = NULL,
      grass_fertilization = 1,
      c_fractions = c(0.0558, 0.015, 0.125, 0.015),
      dec_rates = c(10, 0.3, 0.66, 0.02),
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"wrap_2.rds"))
  
  expect_equal(
    calc_c_seq_field(
      B_LU_BRP = c(256,259,2014,233,265,256,259,2014,233,265),
      B_SOILTYPE_AGR = 'dekzand',
      A_SOM_LOI = 3,
      A_CLAY_MI = 15,
      A_P_AL = 40,
      A_P_WA = 30,
      A_DEPTH = 0.3,
      A_TEMP_MEAN = NULL,
      A_PREC_MEAN = NULL,
      A_ET_MEAN = NULL,
      M_GREEN = TRUE,
      effectivity = c(FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE),
      manure_in = NULL,
      compost_in = 0,
      manure_type = "slurry",
      history = "default",
      renewal = NULL,
      grass_fertilization = 1,
      c_fractions = c(0.0558, 0.015, 0.125, 0.015),
      dec_rates = c(10, 0.3, 0.66, 0.02),
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
  
  # retreive sample 3
  sample <- readRDS(paste0(tloc,"wrap_3.rds"))
  
  expect_equal(
    calc_c_seq_field(
      B_LU_BRP = c(259,266,265,266,259),
      B_SOILTYPE_AGR = 'dekzand',
      A_SOM_LOI = 3,
      A_CLAY_MI = 15,
      A_P_AL = 40,
      A_P_WA = 30,
      A_DEPTH = 0.3,
      A_TEMP_MEAN = NULL,
      A_PREC_MEAN = NULL,
      A_ET_MEAN = NULL,
      M_GREEN = FALSE,
      effectivity = TRUE,
      manure_in = NULL,
      compost_in = 0,
      manure_type = "slurry",
      history = "default",
      renewal = c(2,4),
      grass_fertilization = 3,
      c_fractions = c(0.0558, 0.015, 0.125, 0.015),
      dec_rates = c(10, 0.3, 0.66, 0.02),
      simyears = 50),
    sample,
    tolerance = 0.01
  )
  
})

