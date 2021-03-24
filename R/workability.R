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
  id =crop_code = soiltype = landuse = crop_name = crop_waterstress = crop_season = NULL
  soiltype.m = drukhoogte = gws_sub_workindepth = spring_depth = z = required_depth = NULL
  season_start = season_end = early_season_day_deficit = late_season_day_deficit = NULL
  req_days_pre_glg = req_days_post_glg = gws_sub_workingdepth = NULL
  required_depth_hydrostatic = required_depth_capilary = NULL
  
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
  checkmate::assert_true(all(B_GHG < B_GLG))

  # Collect in data table
  dt <- data.table(id = 1:arg.length,
                   A_CLAY_MI = A_CLAY_MI,
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
  # soiltype.m == 'zand'
    # A_SILT_MI < 10
      dt[soiltype.m == 'zand' & A_SILT_MI < 10,drukhoogte:= -45]
      dt[soiltype.m == 'zand' & A_SILT_MI < 10,gws_sub_workingdepth := 45]
      dt[soiltype.m == 'zand' & A_SILT_MI < 10,spring_depth := 30]
      dt[soiltype.m == 'zand' & A_SILT_MI < 10,z := 27]
    
    # A_SILT_MI > 10 & dt$A_SILT_MI < 20
      dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & dt$A_SILT_MI < 20,drukhoogte:= -55]
      dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & dt$A_SILT_MI < 20,gws_sub_workingdepth := 55]
      dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & dt$A_SILT_MI < 20,spring_depth := 30]
      dt[soiltype.m == 'zand' & A_SILT_MI >= 10 & dt$A_SILT_MI < 20,z := 65]
    
    # A_SILT_MI > 20
      dt[soiltype.m == 'zand' & A_SILT_MI >= 20,drukhoogte:= -60]
      dt[soiltype.m == 'zand' & A_SILT_MI >= 20,gws_sub_workingdepth := 60]
      dt[soiltype.m == 'zand' & A_SILT_MI >= 20,spring_depth := 30]
      dt[soiltype.m == 'zand' & A_SILT_MI >= 20,z := 100]

  
  # soiltype.m == 'loess'
    dt[soiltype.m == 'loess',drukhoogte:= -65]
    dt[soiltype.m == 'loess',gws_sub_workingdepth := 65]
    dt[soiltype.m == 'loess',spring_depth := 12]
    dt[soiltype.m == 'loess',z := 105]
  
# soiltype.m == 'veen'
    dt[soiltype.m == 'veen',drukhoogte:= -55]
    dt[soiltype.m == 'veen',gws_sub_workingdepth := 55]
    dt[soiltype.m == 'veen',spring_depth := 22]
    dt[soiltype.m == 'veen',z := 45]

    # A_CLAY_MI < 12
      dt[soiltype.m == 'klei' & A_CLAY_MI < 12,drukhoogte:= -85]
      dt[soiltype.m == 'klei' & A_CLAY_MI < 12,gws_sub_workingdepth := 85]
      dt[soiltype.m == 'klei' & A_CLAY_MI < 12,spring_depth := 30]
      dt[soiltype.m == 'klei' & A_CLAY_MI < 12,z := 73]
    
    # A_CLAY_MI >12 & A_CLAY_MI < 17
      dt[soiltype.m == 'klei' & A_CLAY_MI >=12 & A_CLAY_MI < 17,drukhoogte:= -85]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=12 & A_CLAY_MI < 17,gws_sub_workingdepth := 85]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=12 & A_CLAY_MI < 17,spring_depth := 12]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=12 & A_CLAY_MI < 17,z := 95]
    
    # A_CLAY_MI >17 & A_CLAY_MI < 25
      dt[soiltype.m == 'klei' & A_CLAY_MI >=17 & A_CLAY_MI < 25,drukhoogte:= -75]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=17 & A_CLAY_MI < 25,gws_sub_workingdepth := 75]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=17 & A_CLAY_MI < 25,spring_depth := 15]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=17 & A_CLAY_MI < 25,z := 60]
    
    # A_CLAY_MI >25 & A_CLAY_MI < 35
      dt[soiltype.m == 'klei' & A_CLAY_MI >=25 & A_CLAY_MI < 35,drukhoogte:= -65]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=25 & A_CLAY_MI < 35,gws_sub_workingdepth := 65]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=25 & A_CLAY_MI < 35,spring_depth := 15]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=25 & A_CLAY_MI < 35,z := 60]

    # A_CLAY_MI >35
      dt[soiltype.m == 'klei' & A_CLAY_MI >=35,drukhoogte:= -45]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=35,gws_sub_workingdepth := 45]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=35,spring_depth := 15]
      dt[soiltype.m == 'klei' & A_CLAY_MI >=35,z := 53]

  # Overwrite spring working depth for perennial crops
  dt[crop_waterstress %in% c('boomteelt', 'overig boomteelt', 'groot fruit',
                             'grasland zonder herinzaai', 'grasland met herinzaai'),
     spring_depth := 0]
  
  # test 1: desired groundwater depth for work under hydrostatic equilibrium
  dt[, required_depth_hydrostatic := gws_sub_workingdepth+spring_depth]
  
  # test 2: when capillary rise is lower than evaporation (something based on Z-h relations)
  dt[, required_depth_capilary := 999]
  
  # Choose lowest required depth as required depth
  dt[,required_depth := fifelse(required_depth_hydrostatic<required_depth_capilary,required_depth_hydrostatic,required_depth_capilary)]
  
# Determine relative season length
  dt[required_depth < B_GHG, relative_seasonlength := 1]# If the required depth is more shallow than the highest water level, soil is always workable 
  dt[required_depth > B_GLG, relative_seasonlength := 0]# If the required depth is deeper than the lowest water level, soil is never workable

  # Calculate the day on which the desired water depth is reached
  dt[,season_start := round(138-sin(-required_depth - 0.5*(-B_GHG - B_GLG)/ 0.5*(-B_GHG+B_GLG))/0.0172142)]
  dt[,season_end := round(138+sin(-required_depth - 0.5*(-B_GHG - B_GLG)/ 0.5*(-B_GHG+B_GLG))/0.0172142)]
    
  # Calculate the number of days deficit compared to ideal situation
  dt[,early_season_day_deficit := req_days_pre_glg-season_start]
  dt[early_season_day_deficit <0,early_season_day_deficit := 0 ] # Deficient number of days cannot be negative
  dt[,late_season_day_deficit := req_days_post_glg-season_end]
  dt[late_season_day_deficit <0, late_season_day_deficit := 0] # Deficient number of days cannot be negative
  
  # Calculate relative season length
  dt[,relative_seasonlength := (total_days-late_season_day_deficit-early_season_day_deficit)/total_days]
  
  # Return relative season length
  setorder(dt, id)
  relative_seasonlength <- dt[,relative_seasonlength]
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
