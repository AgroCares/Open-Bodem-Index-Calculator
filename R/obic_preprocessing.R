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
  
  ID = NULL
  A_CLAY_MI = A_OS_GV = A_PH_CC = A_CN_RAT = A_N_TOT = A_P_PAL = A_P_PAE = NULL
  B_BT_AK = B_LU_BRP = NULL
  D_SE = D_CR = D_BDS = D_RD = D_OC = D_GA = D_NLV = D_PBI = NULL
  D_CP_STARCH = D_CP_POTATO = D_CP_SUGARBEET = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = NULL

  # Calculate soil sealing risk
  dt[, D_SE := calc_sealing_risk(A_CLAY_MI, A_OS_GV)]
  
  # Calculate the crumbleability of the soil
  dt[, D_CR := calc_crumbleability(A_CLAY_MI, A_OS_GV, A_PH_CC)]
  
  # Calculate bulk density
  dt[, D_BDS := calc_bulk_density(A_OS_GV, B_BT_AK)]
  
  # Determine the root depth of the soil
  dt[, D_RD := calc_root_depth(B_LU_BRP)]
  
  # Calculate the amount of total organic carbon
  dt[, D_OC := calc_organic_carbon(A_OS_GV, D_BDS, D_RD)]
  
  # Calculate the grass age
  dt[, D_GA := calc_grass_age(ID, B_LU_BRP)]
  
  # Calculate the NLV
  dt[, D_NLV := calc_nlv(A_N_TOT, D_OC, B_LU_BRP, B_BT_AK, D_BDS, A_CN_RAT, D_GA)]
  
  # Calculate the PBI
  dt[, D_PBI := calc_phosphate_availability(A_P_PAL, A_P_PAE, B_LU_BRP)]
  
  # Calculate the crop rotation fraction
  dt[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
  dt[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
  dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
  dt[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
  dt[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
  dt[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
  
  return(dt)
}