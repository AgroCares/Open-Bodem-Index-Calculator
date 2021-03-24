#' Calculate indicator for workability 
#'
#' This function calculates the workability of soils, given as potential length of the growing season. Based on Huinink (2018)
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (in procent)
#' @param A_SILT_MI (numeric) The silt content of the soil (in procent)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param B_GLG () The lowest groundwater level averaged over the most dry periods in 8 years
#' @param B_GHG () The highest groundwater level averaged over the most wet periods in 8 years
#' 
#' @import data.table
#' 
#' @references Huinink (2018) Bodem/perceel geschiktheidsbeoordeling voor Landbouw, Bosbouw en Recreatie. BodemConsult-Arnhem
#'  
#' @export
calc_workability <- function(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_BT_AK, B_GLG, B_GHG) {
  
  # define variables used within the function
  crop_code = soiltype = landuse = crop_name = crop_waterstress = crop_season = NULL
  soiltype.m = drukhoogte = gws_sub_workindepth = spring_depth = z = required_depth = NULL
  season_start = season_end = early_season_day_deficit = late_season_day_deficit = NULL
  req_days_pre_glg = req_days_post_glg = gws_sub_workingdepth = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  season.obic <- as.data.table(OBIC::season.obic)
  setkey(season.obic, landuse)

  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SILT_MI), length(B_LU_BRP), length(B_BT_AK), length(B_GLG), length(B_GHG))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(B_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_GHG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_true(B_GHG < B_GLG)

  # Collect in data table
  dt <- data.table(A_CLAY_MI = A_CLAY_MI,
                   A_SILT_MI = A_SILT_MI,
                   B_LU_BRP = B_LU_BRP,
                   B_BT_AK = B_BT_AK,
                   B_GLG = B_GLG,
                   B_GHG = B_GHG)
  
  # merge with OBIC crop and soil table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name, crop_waterstress, crop_season)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.m)], by.x = "B_BT_AK", by.y = "soiltype")
  dt <- merge(dt, season.obic, by.x = 'crop_season', by.y = 'landuse')
  
  ## determine workableability key numbers
  if(dt$soiltype.m == 'zand') {
    if(dt$A_SILT_MI < 10) {
      dt[,drukhoogte:= -45]
      dt[,gws_sub_workingdepth := 45]
      dt[,spring_depth := 30]
      dt[,z := 27]
    } 
    if(dt$A_SILT_MI > 10 & dt$A_SILT_MI < 20) {
      dt[,drukhoogte:= -55]
      dt[,gws_sub_workingdepth := 55]
      dt[,spring_depth := 30]
      dt[,z := 65]
    } 
    if(dt$A_SILT_MI > 20) {
      dt[,drukhoogte:= -60]
      dt[,gws_sub_workingdepth := 60]
      dt[,spring_depth := 30]
      dt[,z := 100]
    }
  }
  if(dt$soiltype.m == 'loess') {
    dt[,drukhoogte:= -65]
    dt[,gws_sub_workingdepth := 65]
    dt[,spring_depth := 12]
    dt[,z := 105]
  }
  if(dt$soiltype.m == 'veen') {
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
    }
    if(dt$A_CLAY_MI >12 & A_CLAY_MI < 17) {
      dt[,drukhoogte:= -85]
      dt[,gws_sub_workingdepth := 85]
      dt[,spring_depth := 12]
      dt[,z := 95]
    }
    if(dt$A_CLAY_MI >17 & A_CLAY_MI < 25) {
      dt[,drukhoogte:= -75]
      dt[,gws_sub_workingdepth := 75]
      dt[,spring_depth := 15]
      dt[,z := 60]
    }
    if(dt$A_CLAY_MI >25 & A_CLAY_MI < 35) {
      dt[,drukhoogte:= -65]
      dt[,gws_sub_workingdepth := 65]
      dt[,spring_depth := 15]
      dt[,z := 60]
    }
    if(dt$A_CLAY_MI >35) {
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
  
# Determine relative season length
  if(dt$required_depth < dt$B_GHG) { # If the required depth is more shallow than the highest water level, soil is always workable 
    relative_seasonlength <- 1
  } 
  if(dt$required_depth > dt$B_GLG) { # If the required depth is deeper than the lowest water level, soil is never workable
    relative_seasonlength <- 0
  } else {
    # Calculate the day on which the desired water depth is reached
    dt[,season_start := round(138-sin(-required_depth - 0.5*(-B_GHG - B_GLG)/ 0.5*(-B_GHG+B_GLG))/0.0172142)]
    dt[,season_end := round(138+sin(-required_depth - 0.5*(-B_GHG - B_GLG)/ 0.5*(-B_GHG+B_GLG))/0.0172142)]
    
    # Calculate the number of days deficit compared to ideal situation
    dt[,early_season_day_deficit := req_days_pre_glg-season_start]
    dt[early_season_day_deficit <0,early_season_day_deficit := 0 ] # Deficient number of days cannot be negative
    dt[,late_season_day_deficit := req_days_post_glg-season_end]
    dt[late_season_day_deficit <0, late_season_day_deficit := 0] # Deficient number of days cannot be negative
    
    # Calculate relative season length
    relative_seasonlength <- (dt$total_days-dt$late_season_day_deficit-dt$early_season_day_deficit)/dt$total_days
    
  }
  
  return(relative_seasonlength)
}
#' Calculate indicator for workability
#'
#' This function calculates the indicator for the workability of the soil expressed as the period in which the soil can be worked without
#' inflicting structural damage that cannot be restored by the regular management on the farm.
#'  
#' 
#' @export
ind_workability <- function() {
  
}
