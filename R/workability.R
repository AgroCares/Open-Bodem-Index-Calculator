#' Calculate indicator for workability 
#'
#' This function calculates the workability of soils, given as potential length of the growing season. Based on Huinink (2018)
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (in procent)
#' @param A_SILT_MI (numeric) The silt content of the soil (in procent)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param B_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param B_GHG (numeric) The highest groundwater level averaged over the most wet periods in 8 years in cm below ground level
#' 
#' @import data.table
#' 
#' @references Huinink (2018) Bodem/perceel geschiktheidsbeoordeling voor Landbouw, Bosbouw en Recreatie. BodemConsult-Arnhem
#'  
#' @export
calc_workability <- function(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_BT_AK, B_GLG, B_GHG) {
  
  # define variables used within the function
  id =crop_code = soiltype = landuse = crop_name = crop_waterstress = crop_season = NULL
  soiltype.m = drukhoogte = spring_depth = z = NULL
  season_start = season_end = early_season_day_deficit = late_season_day_deficit = NULL
  req_days_pre_glg = req_days_post_glg = gws_sub_workingdepth = NULL
  req_depth_hydrostatic = req_depth_capilary = total_days = req_depth_spring= NULL
  
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
  dt[, req_depth_hydrostatic_spring := gws_sub_workingdepth+spring_depth]
  dt[, req_depth_hydrostatic_fall := gws_sub_workingdepth]
  
  # test 2: when capillary rise is lower than evaporation (something based on Z-h relations)
  dt[, req_depth_capilary := 999]
  
  # Choose lowest required depth as required depth
  dt[,req_depth_spring := fifelse(req_depth_hydrostatic_spring <= req_depth_capilary, req_depth_hydrostatic_spring,req_depth_capilary)]
  dt[,req_depth_fall   := fifelse(req_depth_hydrostatic_fall   <= req_depth_capilary, req_depth_hydrostatic_fall,  req_depth_capilary)]
  
# Calculate season length -----
  # Calculate the day on which the desired water depth is reached for spring and fall
  # Spring
  dt[req_depth_spring >= B_GHG & req_depth_spring <= B_GLG,
     req_spring_depth_day := round(138-(asin((-req_depth_spring-0.5*(-B_GHG-B_GLG))/(0.5*(-B_GHG+B_GLG)))/0.0172024))]
  dt[req_depth_spring < B_GHG, req_spring_depth_day := 1] # if highest groundwater level is deeper than required depth, soil is always workable
  dt[req_depth_spring > B_GLG, req_spring_depth_day := 228] # if lowest groundwater level is higher than required depth, soil is never workable.
  # required_depth_day is set to 228 (which is 15 aug/GLG) so season length will be 0
  
  # Fall
  dt[req_depth_fall >= B_GHG & req_depth_fall <= B_GLG,
     req_fall_depth_day :=   round(138-(asin((-req_depth_fall-0.5*(-B_GHG-B_GLG))/(0.5*(-B_GHG+B_GLG)))/0.0172024))]
  dt[req_depth_fall < B_GHG, req_fall_depth_day := 1] # if highest groundwater level is deeper than required depth, soil is always workable
  dt[req_depth_fall > B_GLG, req_fall_depth_day := 228] # if lowest groundwater level is higher than required depth, soil is never workable.
  
  # Calculate the number of days deficit compared to ideal situation
  dt[,early_season_day_deficit := req_days_pre_glg-(228-req_spring_depth_day)]
  dt[early_season_day_deficit <0,early_season_day_deficit := 0 ] # Deficient number of days cannot be negative
  dt[,late_season_day_deficit := req_days_post_glg-(228-req_fall_depth_day)]
  dt[late_season_day_deficit <0, late_season_day_deficit := 0] # Deficient number of days cannot be negative
  
  # Calculate relative season length
  dt[,rsl := (total_days-late_season_day_deficit-early_season_day_deficit)/total_days]
  
  # # Calculate yield % loss by sub-optimal season length
  dt[derving == 'zaai groenten', yl := 538*rsl^2-1144*rsl+606]
  dt[derving == 'zomergranen'   , yl := 232*rsl^2- 475*rsl+243]
  dt[derving == 'plant groenten', yl := 392*rsl^2- 785*rsl+393]
  dt[derving == 'wintergranen'  , yl :=(232*rsl^2- 475*rsl+243)*0.85/2]
  dt[derving == 'boomteelt'     , yl :=(538*rsl^2-1144*rsl+606)/2]
  dt[derving == 'overig'        , yl := 100*rsl^2- 100*rsl+100]
  
  # functions to determine yield loss in grass
  ylveen <- approxfun(x = c(1, 0.9, 0.8, 0.6, 0.55, 0.43, 0.22, 0.08, 0),
                      y = c(23, 25, 28, 36, 38, 41, 54, 59, 100), method = 'linear',
                      yleft = NA_integer_, yright = NA_integer_)
  ylsoil <- approxfun(x = c(1, 0.9, 0.8, 0.6, 0.55, 0.43, 0.22, 0.08, 0),
                      y = c(23, 25, 29, 41, 43, 51, 68, 72, 100), method = 'linear',
                      yleft = NA_integer_, yright = NA_integer_)
  
  dt[derving == 'grasland' & soiltype.m == 'veen', yl := ylveen(rsl)]
  dt[derving == 'grasland' & !soiltype.m == 'veen', yl := ylsoil(rsl)]
  
  # Calculate yield fraction
  dt[,yield := 1-0.01*yl] 
  # Set yield to 0 if negative
  dt[yield < 0, yield := 0]
  
  # Return yield
  setorder(dt, id)
  value <- dt[,yield]
  return(value)
}
#' Calculate indicator for workability
#'
#' This function calculates the indicator for the workability of the soil expressed as the period in which the soil can be worked without
#' inflicting structural damage that cannot be restored by the regular management on the farm.
#'  
#' @param D_P_WO (numeric) The value of workability calculated by \code{\link{calc_workability}}
#'  
#' @export
ind_workability <- function(D_P_WO) {
  
  # Check inputs
  checkmate::assert_numeric(D_P_WO, lower = 0, upper = 1, any.missing = FALSE)
  # Maybe insert logistic function
  return(D_P_WO)
}
