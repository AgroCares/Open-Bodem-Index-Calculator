#' Calculate indicator for workability 
#'
#' This function calculates the workability of soils, given as potential length of the growing season. Based on Huinink (2018)
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (in procent)
#' @param A_SILT_MI (numeric) The silt content of the soil (in procent)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param B_GT (character) The groundwater table class
#'  
#' @export
calc_workability <- function(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_BT_AK, B_GT) {
  
  # define variables used within the function
  
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  waterstress.obic <- as.data.table(OBIC::waterstress.obic)
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI), length(B_LU_BRP), B_BT_AK, length(B_GT))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_character(B_GT,any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GT, choices = c('unknown',unique(waterstress.obic$gt)), empty.ok = FALSE)

  # Collect in data table
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_LU_BRP = B_LU_BRP,
                   B_BT_AK = B_BT_AK,
                   B_GT = B_GT)
  
  # merge with OBIC crop and soil table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  ## determine workableability key numbers
  if(dt$soiltype.m == 'zand') {
    if(dt$A_SILT_MI < 10) {
      dt[,drukhoogte:= -45]
      dt[,gws_sub_workingdepth := 45]
      dt[,spring_depth := 30]
      dt[,z := 27]
    } if(dt$A_SILT_MI > 10 & dt$A_SILT_MI < 20) {
      dt[,drukhoogte:= -55]
      dt[,gws_sub_workingdepth := 55]
      dt[,spring_depth := 30]
      dt[,z := 65]
    } if(dt$A_SILT_MI > 20) {
      dt[,drukhoogte:= -60]
      dt[,gws_sub_workingdepth := 60]
      dt[,spring_depth := 30]
      dt[,z := 100]
    }
  } if(dt$soiltype.m == 'loess') {
    dt[,drukhoogte:= -65]
    dt[,gws_sub_workingdepth := 65]
    dt[,spring_depth := 12]
    dt[,z := 105]
  } if(dt$soiltype.m == 'veen') {
    dt[,drukhoogte:= -55]
    dt[,gws_sub_workingdepth := 55]
    dt[,spring_depth := 22]
    dt[,z := 45]
  } else{
    if(dt$A_CLAY_MI < 12) {
      dt[,drukhoogte:= -85]
      dt[,gws_sub_workingdepth := 85]
      dt[,spring_depth := 30]
      dt[,z := 73]
    } if(dt$A_CLAY_MI >12 & A_CLAY_MI < 17) {
      dt[,drukhoogte:= -85]
      dt[,gws_sub_workingdepth := 85]
      dt[,spring_depth := 12]
      dt[,z := 95]
    } if(dt$A_CLAY_MI >17 & A_CLAY_MI < 25) {
      dt[,drukhoogte:= -75]
      dt[,gws_sub_workingdepth := 75]
      dt[,spring_depth := 15]
      dt[,z := 60]
    } if(dt$A_CLAY_MI >25 & A_CLAY_MI < 35) {
      dt[,drukhoogte:= -65]
      dt[,gws_sub_workingdepth := 65]
      dt[,spring_depth := 15]
      dt[,z := 60]
    } if(dt$A_CLAY_MI >35) {
      dt[,drukhoogte:= -45]
      dt[,gws_sub_workingdepth := 45]
      dt[,spring_depth := 15]
      dt[,z := 53]
    }
  }
  # Overwrite spring working depth for perennial crops
  dt[crop_waterstress %in% c('boomteelt', 'overig boomteelt', 'groot fruit',
                             'grasland zonder herinzaai', 'grasland met herinzaai'),
     spring_depth := 0]
  
  # test 1: desired groundwater depth for work under hydrostatic equilibrium
  dt[, required_depth := gws_sub_workingdepth+spring_depth]
  
  # test 2: when capillary rise is lower than evaporation (something based on Z-h relations)
  
  
  ## Calculate growing season length based on Regimecurve
  regime_table <- data.table(gt = c('GtI',"GtII", "GtIII", "GtV", "GtIV", "GtVI", "GtVII", "GtVIII"),
                             feb15 = c(20, 40, 40, 40, 40, 80, 80, 140),
                             aug15 = c(50, 80, 120, 120, 120, 300, 300, 300)) # Not sure how to get a better estimate of what is essentially GLG and GHG
  # merge regime_table into dt
  dt <- merge.data.table(dt, regime_table, by.x = 'B_GT', by.y = 'gt')
  
  if(dt$required_depth < dt$feb15) { # If the required depth is more shallow than the highest water level, soil is always workable 
    relative_seasonlength <- 1
  } if(dt$required_depth > dt$aug15) { # If the required depth is deeper than the lowest water level, soil is never workable
    relative_seasonlength <- 0
  } else {
    # Calculate the day on which the desired water depth is reached
    dt[,season_start := round(138-sin(-required_depth - 0.5*(-feb15 - aug15)/ 0.5*(-feb15+aug15))/0.0172142)]
    dt[,season_end := round(138+sin(-required_depth - 0.5*(-feb15 - aug15)/ 0.5*(-feb15+aug15))/0.0172142)]
    relative_seasonlength <- (dt$season_start+dt$season_end)/dt$desired_season_length 
    ## to add: correction for season not being long enough before or after aug15
    if(relative_seasonlength > 1) {
      relative_seasonlength <- 1
    }
  }
  
  return(relative_seasonlength)
}
#' Calculate indicator for workability
#'
#' This function calculates the indicator for the workability of the soil expressed as the period in which the soil can be worked without
#' inflicting structural damage that cannot be restored by the regular management on the farm.
#'  
#' @param D_P_WO (numeric) The value for workability as calculated by \code{\link{calc_workability}}
#' 
#' @export
ind_workability <- function() {
  
}
