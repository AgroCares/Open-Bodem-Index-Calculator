#' Calculate the indicators for the OBI
#' 
#' This wrapper function contains the functions to calculate the indicators of agricultural fields.
#' 
#' @param dt.ppr (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_indicators<- function(dt.ppr) {
  
  # Check inputs
  checkmate::assert_data_table(dt.ppr)
  
  A_OS_GV = NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_OM = I_B_SF = I_B_SB = NULL
  B_LU_BRP = NULL
  D_SE = D_CR = NULL
  
  # Evaluate nutrients ------------------------------------------------------
  
  # Nitrogen
  dt.ppr[, I_C_N := -999]
  
  # Phosphorus
  dt.ppr[, I_C_P := -999]
  
  # Potassium
  dt.ppr[, I_C_K := -999]
  
  # Magnesium
  dt.ppr[, I_C_MG := -999]
  
  # Sulphur
  dt.ppr[, I_C_S := -999]
  
  # pH
  dt.ppr[, I_C_PH := -999]
  
  # CEC
  dt.ppr[, I_C_CEC := -999]
  
  # Copper
  dt.ppr[, I_C_CU := -999]
  
  # Zinc
  dt.ppr[, I_C_ZN := -999]
  

  # Evaluate physical -------------------------------------------------------
  
  # Crumbleability
  dt.ppr[, I_P_CR := ind_crumbleability(D_CR, B_LU_BRP)]
  
  # Sealing
  dt.ppr[, I_P_SE := ind_sealing(D_SE, B_LU_BRP)]
  
  # Moisture supply
  dt.ppr[, I_P_MS := -999]
  
  # Bearing capacity
  dt.ppr[, I_P_BC := -999]
  
  # Dustiness
  dt.ppr[, I_P_DU := -999]
  
  # Compaction
  dt.ppr[, I_P_CO := -999]
  

  # Evaluate biological -----------------------------------------------------
  
  # Disease / pest resistance
  dt.ppr[, I_B_DI := ind_resistance(A_OS_GV)]
  
  # Organic matter
  dt.ppr[, I_B_OM := -999]
  
  # Soil life activity
  dt.ppr[, I_B_SF := -999]
  
  # Soil biodiversity
  dt.ppr[, I_B_SB := -999]
  
  
  return(dt.ppr)
}