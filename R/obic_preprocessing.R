#' Preprocess for the OBI
#' 
#' This wrapper function contains the functions to preprocess the data of agricultural fields.
#' 
#' @param dt (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_preprocessing <- function(dt) {
  
  # Check inputs
  checkmate::assert_data_table(dt)
  
  A_CLAY_MI = A_OS_GV = A_PH_CC = NULL
  B_BT_AK = NULL
  D_SE = D_CR = NULL

  # Calculate soil sealing risk
  dt[, D_SE := calc_sealing_risk(A_CLAY_MI, A_OS_GV)]
  
  # Calculate the crumbleability of the soil
  dt[, D_CR := calc_crumbleability(A_CLAY_MI, A_OS_GV, A_PH_CC)]
  
  # Calculate bulk density
  dt[, D_BDS := calc_bulk_density(A_OS_GV, B_BT_AK)]
  
  return(dt)
}