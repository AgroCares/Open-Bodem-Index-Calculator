#' Calculate the BodemConditieScore
#' 
#' This function calculates the BodemConditieScore given input from manual observations made in the field.
#' The individual parameters are scored in three classes: poor (0), neutral (1) or good (2)
#' More information on this test can be found \href{http://mijnbodemconditie.nl/}{here}
#' 
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param D_PH_DELTA (numeric) The pH difference with the optimal pH.
#' @param A_EW_BCS (numeric) The presence of earth worms (score 0-1-2)
#' @param A_SC_BCS (numeric) The presence of compaction of subsoil (score 0-1-2)
#' @param A_GS_BCS (numeric) The presence of waterlogged conditions, gley spots (score 0-1-2)
#' @param A_P_BCS (numeric) The presence / occurrence of water puddles on the land, ponding (score 0-1-2)
#' @param A_C_BCS (numeric) The presence of visible cracks in the top layer (score 0-1-2)
#' @param A_RT_BCS (numeric) The presence of visible tracks / rutting or trampling on the land (score 0-1-2)
#' @param A_RD_BCS (integer) The rooting depth (score 0-1-2)
#' @param A_SS_BCS (integer) The soil structure (score 0-1-2)
#' @param A_CC_BCS (integer) The crop cover on the surface (score 0-1-2)
#' 
#' @references \href{http://mijnbodemconditie.nl/}{mijnbodemconditie.nl}
#' 
#' @import data.table
#' 
#' @export
calc_bcs <- function(B_LU_BRP,B_SOILTYPE_AGRA_SOM_LOI, D_PH_DELTA,
                     A_EW_BCS, A_SC_BCS, A_GS_BCS, A_P_BCS, A_C_BCS, A_RT_BCS, A_RD_BCS, A_SS_BCS, A_CC_BCS
                     ) {
  
  id = crop_code = crop_n = crop_category = bcs_om = bcs_ph = soiltype = soiltype.n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_EW_BCS), length(A_SC_BCS), length(A_GS_BCS), length(A_P_BCS),
                    length(A_C_BCS), length(A_RT_BCS), length(A_RD_BCS), length(A_SS_BCS),
                    length(A_CC_BCS), length(B_LU_BRP))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_PH_DELTA, lower = 0, upper = 5, any.missing = FALSE)
  checkmate::assert_numeric(A_EW_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_SC_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_GS_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_P_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_C_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_RT_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_RD_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_SS_BCS,lower = 0, upper = 2, any.missing = FALSE)
  checkmate::assert_numeric(A_CC_BCS,lower = 0, upper = 2, any.missing = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_EW_BCS = A_EW_BCS,
    A_SC_BCS = A_SC_BCS, 
    A_GS_BCS = A_GS_BCS, 
    A_P_BCS = A_P_BCS, 
    A_C_BCS = A_C_BCS, 
    A_RT_BCS = A_RT_BCS, 
    A_RD_BCS= A_RD_BCS,
    A_SS_BCS = A_SS_BCS,
    A_CC_BCS = A_CC_BCS,
    value = NA_real_
  )
  
  # merge data.table with crop type and soil type
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # calculate the scores of the Visual Soil Assessment Form: these range from 0 (poor) to 2 (good)
  
  # Calculate the score for pH
  dt[, bcs_ph := round(ind_ph(D_PH_DELTA) * 2)]
  
  # Calculate the score for organic matter 
  dt[, bcs_om := 1]
  
  # organic matter class 'low' when the OM content is lower than 30% quantile
  dt[crop_category == 'akkerbouw' & soiltype.n == 'klei' & A_SOM_LOI < 2.2, bcs_om := 0]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'zand' & A_SOM_LOI < 3.0, bcs_om := 0]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'loess' & A_SOM_LOI < 2.4, bcs_om := 0]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'veen' & A_SOM_LOI < 7.9, bcs_om := 0]
  dt[crop_category == 'grasland' & soiltype.n == 'klei' & A_SOM_LOI < 6.8, bcs_om := 0]
  dt[crop_category == 'grasland' & soiltype.n == 'zand' & A_SOM_LOI < 4.6, bcs_om := 0]
  dt[crop_category == 'grasland' & soiltype.n == 'loess' & A_SOM_LOI < 5.1, bcs_om := 0]
  dt[crop_category == 'grasland' & soiltype.n == 'veen' & A_SOM_LOI < 15.5, bcs_om := 0]
  dt[crop_category == 'mais' & soiltype.n == 'klei' & A_SOM_LOI < 3.4, bcs_om := 0]
  dt[crop_category == 'mais' & soiltype.n == 'zand' & A_SOM_LOI < 3.4, bcs_om := 0]
  dt[crop_category == 'mais' & soiltype.n == 'loess' & A_SOM_LOI < 2.6, bcs_om := 0]
  dt[crop_category == 'mais' & soiltype.n == 'veen' & A_SOM_LOI < 8.7, bcs_om := 0]
  
  # organic matter class 'high' when the OM content is higher than 70% quantile
  dt[crop_category == 'akkerbouw' & soiltype.n == 'klei' & A_SOM_LOI > 3.8, bcs_om := 2]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'zand' & A_SOM_LOI > 4.8, bcs_om := 2]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'loess' & A_SOM_LOI > 3.3, bcs_om := 2]
  dt[crop_category == 'akkerbouw' & soiltype.n == 'veen' & A_SOM_LOI > 14.6, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'klei' & A_SOM_LOI > 12.9, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'zand' & A_SOM_LOI > 6.6, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'loess' & A_SOM_LOI > 7.7, bcs_om := 2]
  dt[crop_category == 'grasland' & soiltype.n == 'veen' & A_SOM_LOI > 28.6, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'klei' & A_SOM_LOI > 6.2, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'zand' & A_SOM_LOI > 4.8, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'loess' & A_SOM_LOI > 3.4, bcs_om := 2]
  dt[crop_category == 'mais' & soiltype.n == 'veen' & A_SOM_LOI > 20.1, bcs_om := 2]
  
  # evaluation for nature (not present yet)
  dt[crop_category == 'natuur', bcs_om := 2]
  
  # Calculate final score
  dt[,value := 2 * A_CC_BCS + 3 * A_RD_BCS+ 3 * A_SC_BCS + 3 * A_EW_BCS + 
               3 * A_SS_BCS +3 * bcs_ph + 3 * bcs_om + 1 * A_GS_BCS -
               2 * A_P_BCS - 1 * A_C_BCS - 1 * A_RT_BCS]

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
  checkmate::assert_numeric(D_BCS, lower = 0, upper = 50, any.missing = FALSE)
  
  # Evaluate the BodemConditieScore
  # ensure that it does not exceed 1
  value <- pmin(D_BCS / 40, 1)
  
  # return output
  return(value)
}
