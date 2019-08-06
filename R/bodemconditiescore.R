#' Calculate the BodemConditieScore
#' 
#' This function calculates the BodemConditieScore given input from manual observations made in the field.
#' The individual parameters are scored in three classes: poor (0), neutral (1) or good (2)
#' More information on this test can be found \href{http://mijnbodemconditie.nl/}{here}
#' 
#' 
#' @param A_OS_GV (numeric) The organic matter content of the soil (\%)
#' @param D_PH_DELTA (numeric) The pH difference with the optimal pH.
#' @param A_RW_BC (numeric) The presence of earth worms (score 0-1-2)
#' @param A_BS_BC (numeric) The presence of compaction of soil structure (score 0-1-2)
#' @param A_GV_BC (numeric) The presence of waterlogged conditions (score 0-1-2)
#' @param A_PV_BC (numeric) The presence / occurence of water puddles on the land (score 0-1-2)
#' @param A_AS_BC (numeric) The presence of visible cracks in the top layer (score 0-1-2)
#' @param A_SV_BC (numeric) The presence of visible tracks on the land (score 0-1-2)
#' @param A_RD_BC (integer) The rooting depth (score 0-1-2)
#' @param A_SS_BC (integer) The soil structure (score 0-1-2)
#' @param A_CO_BC (integer) The crop density (score 0-1-2)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' 
#' @references \href{http://mijnbodemconditie.nl/}{mijnbodemconditie.nl}
#' 
#' @import data.table
#' 
#' @export
calc_bcs <- function(A_RW_BC, A_BS_BC, A_GV_BC, A_PV_BC, A_AS_BC, A_SV_BC, A_RD_BC, A_SS_BC, A_CO_BC,
                     A_OS_GV, D_PH_DELTA,
                     B_LU_BRP,B_BT_AK) {
  
  id = crop_code = crop_n = crop_category = bcs_om = bcs_ph = soiltype = soiltype.n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_RW_BC), length(A_BS_BC), length(A_GV_BC), length(A_PV_BC),
                    length(A_AS_BC), length(A_SV_BC), length(A_RD_BC), length(A_SS_BC),
                    length(A_CO_BC), length(B_LU_BRP))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_PH_DELTA, lower = 0, upper = 5, any.missing = FALSE)
  checkmate::assert_numeric(A_RW_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_BS_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_GV_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_PV_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_AS_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_SV_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_RD_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_SS_BC,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_CO_BC,lower = 0, upper = 2, any.missing = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_RW_BC = A_RW_BC,
    A_BS_BC = A_BS_BC, 
    A_GV_BC = A_GV_BC, 
    A_PV_BC = A_PV_BC, 
    A_AS_BC = A_AS_BC, 
    A_SV_BC = A_SV_BC, 
    A_RD_BC = A_RD_BC,
    A_SS_BC = A_RD_BC,
    A_CO_BC = A_CO_BC,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # calculate the scores of the BodemConditieScore: these range from 0 (poor) to 2 (good)
  
  # Calculate the score for pH
  dt[, bcs_ph := round(ind_ph(D_PH_DELTA) * 2)]
  
  # Calculate the score for organic matter 
  dt[, bcs_om := 0]
  
  # organic matter class 'low' when the OM content is lower than 30% quantile
  dt[crop_category == 'akkerbouw' & soiltype.n == 'klei' & A_OS_GV < 2.2, bcs_om := 1]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'zand' & A_OS_GV < 3.0, bcs_om := 1]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'loess' & A_OS_GV < 2.4, bcs_om := 1]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'veen' & A_OS_GV < 7.9, bcs_om := 1]
  dt[crop_category == 'grasland' & soiltype.n == 'klei' & A_OS_GV < 6.8, bcs_om := 1]
  dt[crop_category == 'grasland' & soiltype.n == 'zand' & A_OS_GV < 4.6, bcs_om := 1]
  dt[crop_category == 'grasland' & soiltype.n == 'loess' & A_OS_GV < 5.1, bcs_om := 1]
  dt[crop_category == 'grasland' & soiltype.n == 'veen' & A_OS_GV < 15.5, bcs_om := 1]
  dt[crop_category == 'mais' & soiltype.n == 'klei' & A_OS_GV < 3.4, bcs_om := 1]
  dt[crop_category == 'mais' & soiltype.n == 'zand' & A_OS_GV < 3.4, bcs_om := 1]
  dt[crop_category == 'mais' & soiltype.n == 'loess' & A_OS_GV < 2.6, bcs_om := 1]
  dt[crop_category == 'mais' & soiltype.n == 'veen' & A_OS_GV < 8.7, bcs_om := 1]
  
  # organic matter class 'high' when the OM content is higher than 70% quantile
  dt[crop_category == 'akkerbouw' & soiltype.n == 'klei' & A_OS_GV > 3.8, bcs_om := 2]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'zand' & A_OS_GV > 4.8, bcs_om := 2]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'loess' & A_OS_GV > 3.3, bcs_om := 2]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'veen' & A_OS_GV > 14.6, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'klei' & A_OS_GV > 12.9, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'zand' & A_OS_GV > 6.6, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'loess' & A_OS_GV > 7.7, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'veen' & A_OS_GV > 28.6, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'klei' & A_OS_GV > 6.2, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'zand' & A_OS_GV > 4.8, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'loess' & A_OS_GV > 3.4, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'veen' & A_OS_GV > 20.1, bcs_om := 2]
  
  # evaluation for nature (not present yet)
  dt[crop_category == 'natuur', bcs_om := 2]
  
  # Calculate final score
  dt[,value := 2 * A_CO_BC + 3 * A_RD_BC + 3 * A_BS_BC + 3 * A_RW_BC + 
               3 * A_SS_BC +3 * bcs_ph + 3 * bcs_om + 1 * A_GV_BC -
               2 * A_PV_BC - 1 * A_AS_BC - 1 * A_SV_BC]

  # Combine both tables and extract values
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}

#' Calculate the indicator for BodemConditieScore
#' 
#' This function calculates the final score for the BodemConditieScore by using the scores calculated by \code{\link{calc_bcs}}
#' 
#' @param D_BCS (numeric) The value of BCS  calculated by \code{\link{calc_bcs}}
#' 
#' @export
ind_bcs <- function(D_BCS) {
  
  # Check inputs
  checkmate::assert_numeric(D_BCS, lower = 0, upper = 40, any.missing = FALSE)
  
  # Evaluate the BodemConditieScore
  value <- D_BCS / 40
  
  # return output
  return(value)
}
