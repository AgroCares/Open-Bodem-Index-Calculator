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
  A_CLAY_MI = A_OS_GV = A_PH_CC = A_CN_RAT = A_N_TOT = A_P_PAL = A_P_PAE = A_SILT_MI = A_S_TOT = NULL
  A_MG_CC = A_CEC_CO = A_K_CC = A_K_CEC = NULL
  B_BT_AK = B_LU_BRP = B_GT = B_LG_CBS = B_HELP_WENR = NULL
  D_SE = D_CR = D_BDS = D_RD = D_OC = D_GA = D_NLV = D_PBI = D_OS_BAL = NULL
  D_CP_STARCH = D_CP_POTATO = D_CP_SUGARBEET = D_CP_RUST = D_CP_RUSTDEEP = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_PH_DELTA = NULL
  D_MAN = D_P_DU = D_SLV = D_MG = NULL
  A_OS_GV = A_P_PAL = A_P_WA = M_M3 = M_M6 = NULL
  M_M4 = M_M10 = M_M11 = M_M12 = M_M13 = M_M14 = M_M15 = NULL
  D_WSI = D_K = NULL
  
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
  
  # Calculate a simple organic matter balance
  dt[,D_OS_BAL := calc_sombalance(A_OS_GV, A_P_PAL, A_P_WA, B_LU_BRP, M_M3, M_M6)]
  
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
  dt[, D_CP_RUST := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewas")]
  dt[, D_CP_RUSTDEEP := calc_rotation_fraction(ID, B_LU_BRP, crop = "rustgewasdiep")]
  
  # Calculate the difference between the actual pH and optimum pH
  dt[, D_PH_DELTA := calc_ph_delta(A_PH_CC, B_BT_AK, A_CLAY_MI, A_OS_GV, D_CP_STARCH, D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]
  
  # Determine the managment
  dt[, D_MAN := calc_management(A_OS_GV,B_LU_BRP, B_BT_AK,B_GT,
                                D_OS_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                                M_M4, M_M6, M_M10, M_M11, M_M12, M_M13, M_M14, M_M15)]
  
  # Calculate the wind erodibility 
  dt[, D_P_DU := calc_winderodibility(A_CLAY_MI, A_SILT_MI, B_LU_BRP)]
  
  # Calculate the sulphur supply
  dt[, D_SLV := calc_slv(A_OS_GV,A_S_TOT, B_LU_BRP, B_BT_AK, B_LG_CBS,D_BDS)]
  
  # Calculate the magnesium index
  dt[, D_MG := calc_magnesium_availability(A_MG_CC,A_PH_CC,A_OS_GV,A_CEC_CO, A_K_CC,
                                           A_K_CEC,A_CLAY_MI,B_BT_AK,B_LU_BRP)]
  
  # Calculate the potassium index
  # dt[,DK := calc_potassium_availability(A_K_CC, A_K_CEC,A_CEC_CO, A_PH_CC, A_OS_GV, A_CLAY_MI, B_LU_BRP, B_BT_AK)]
  
  # Calculate Water Stress Risk
  dt[,D_WSI := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GT, WSI = 'waterstress')]
  
  
  return(dt)
}