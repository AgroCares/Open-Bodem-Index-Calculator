#' Calculate the BodemConditieScore
#' 
#' This function calculates the BodemConditieScore given input from manual observations made in the field.
#' The individual parameters are scored in three classes: poor (0), neutral (1) or good (2)
#' More information on this test can be found here: http://mijnbodemconditie.nl/
#' 
#' @param A_RW_BC (numeric) The number of earth worms present
#' @param A_BS_BC (numeric) The presence of compaction of soil structure
#' @param A_GV_BC (numeric) The presence of waterlogged conditions
#' @param A_PV_BC (numeric) The presence / occurence of water puddles on the land
#' @param A_AS_BC (numeric) The presence of visible cracks in the top layer
#' @param A_SV_BC (numeric) The presence of visible tracks on the land
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_bcs <- function(A_RW_BC, A_BS_BC, A_GV_BC, A_PV_BC, A_AS_BC, A_SV_BC, B_LU_BRP) {
  
  id = crop_code = crop_n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_N_TOT))
  checkmate::assert_numeric(A_N_TOT, lower = 0, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)

  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_RW_BC = A_RW_BC,
    A_BS_BC = A_BS_BC, 
    A_GV_BC = A_GV_BC, 
    A_PV_BC = A_PV_BC, 
    A_AS_BC = A_AS_BC, 
    A_SV_BC = A_SV_BC, 
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # Calculate NLV for grass
  
  
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
  checkmate::assert_numeric(D_BCS, lower = 0, upper = 10, any.missing = FALSE)
  
  # Evaluate the nitrogen
  value <- OBIC::evaluate_parabolic(D_BCS, x.top = 5)
  
  # return output
  return(value)
}
