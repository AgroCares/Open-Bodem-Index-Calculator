# set directory to the right one
tloc <- if(length(list.files("../testdata/"))>0){"../testdata/"} else {'tests/testdata/'}


# Makkink function
test_that("calc_makkink works", {
  # retreive sample 1
  sample <- readRDS(paste0(tloc,"calc_makkink_1.rds"))
  
  expect_equal(
    calc_makkink(c(256,259,2014,233,234,256,259,2014,233,234)),
    sample,
    tolerance = 0.01
  )
  
  # retreive sample 2
  sample <- readRDS(paste0(tloc,"calc_makkink_2.rds"))
  
  expect_equal(
    calc_makkink(c(212,240,258,314,428,661,800,967,1010,1080,1914,2029)),
    sample,
    tolerance = 0.01
  )
  
  
})