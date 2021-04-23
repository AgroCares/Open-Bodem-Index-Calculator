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
  
  # make local copy
  dt <- copy(dt)
  
  # define variables used within the function
  ID = NULL
  A_CLAY_MI = A_OS_GV = A_PH_CC = A_CN_RAT = A_N_TOT = A_P_PAL = A_P_PAE = A_P_WA = A_SILT_MI = A_S_TOT = NULL
  A_MG_CC = A_CEC_CO = A_K_CC = A_K_CEC = A_CU_CC = A_MN_CC = A_ZN_CC = A_SAND_MI = NULL
  A_RW_BC = A_BS_BC = A_GV_BC = A_PV_BC = A_AS_BC = A_SV_BC = A_RD_BC = A_SS_BC = A_CO_BC = NULL
  A_CA_CEC = A_MG_CEC = NULL
  A_N_PMN = NULL
  B_BT_AK = B_LU_BRP = B_GT = B_LG_CBS = B_HELP_WENR = B_OV_WENR = NULL
  D_SE = D_CR = D_BDS = D_RD = D_OC = D_GA = D_NLV = D_PBI = D_OS_BAL = NULL
  D_CP_STARCH = D_CP_POTATO = D_CP_SUGARBEET = D_CP_RUST = D_CP_RUSTDEEP = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_PH_DELTA = NULL
  D_CEC = D_AS = D_P_WRI = NULL
  D_MAN = D_P_DU = D_SLV = D_MG = D_PMN = D_ZN = D_CU = D_PH_DELTA = D_BCS = NULL
  M_M3 = M_M6 = M_M10 = M_M11 = M_M12 = M_M13 = M_M14 = M_M15 = NULL
  D_WSI = D_K = D_NGW = D_NSW = NULL
  D_P_WO = B_GLG = B_GHG = B_Z_TWO= NULL
  
  # Convert B_OV_WENR input when numeric
  dt[, B_OV_WENR := format_soilcompaction(B_OV_WENR)]
  
  # Format GT when required
  dt[, B_GT := format_gwt(B_GT)]
  
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
  
  # Calculate the potassium availability
  dt[, D_K := calc_potassium_availability(A_K_CC, A_K_CEC,A_CEC_CO, A_PH_CC, A_OS_GV, A_CLAY_MI, B_LU_BRP, B_BT_AK)]

  # Calculate the PBI
  dt[, D_PBI := calc_phosphate_availability(A_P_PAL, A_P_PAE, A_P_WA,B_LU_BRP)]
  
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
  dt[, D_PH_DELTA := calc_ph_delta(A_PH_CC, B_BT_AK, A_CLAY_MI, A_OS_GV, D_CP_STARCH, D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER, B_LU_BRP)]
  
  # Determine the managment
  dt[, D_MAN := calc_management(A_OS_GV,B_LU_BRP, B_BT_AK,B_GT,
                                D_OS_BAL,D_CP_GRASS,D_CP_POTATO,D_CP_RUST,D_CP_RUSTDEEP,D_GA,
                                M_M6, M_M10, M_M11, M_M12, M_M13, M_M14, M_M15)]
  
  # Calculate the wind erodibility 
  dt[, D_P_DU := calc_winderodibility(A_CLAY_MI, A_SILT_MI, B_LU_BRP)]
  
  # Calculate the sulphur supply
  dt[, D_SLV := calc_slv(A_S_TOT,A_OS_GV,B_LU_BRP, B_BT_AK, D_BDS)]
  
  # Calculate the magnesium index
  dt[, D_MG := calc_magnesium_availability(B_LU_BRP,B_BT_AK,A_OS_GV,A_CLAY_MI,
                                           A_PH_CC, A_CEC_CO,A_K_CEC,A_MG_CC,A_K_CC)]
 
  # Calculate the Cu-index
  dt[, D_CU := calc_copper_availability(A_CU_CC, A_OS_GV, A_MN_CC,A_CLAY_MI,A_K_CC,
                                        B_LU_BRP)]
  # Calculate the Zn-index
  dt[, D_ZN := calc_zinc_availability(A_ZN_CC, B_LU_BRP, B_BT_AK, A_PH_CC)]
  
  # Calculate the index for microbial activity
  dt[, D_PMN := calc_pmn(A_N_PMN,B_LU_BRP,B_BT_AK)]

  # Calculate the CEC as cationbuffer index
  dt[,D_CEC := calc_cec(A_CEC_CO)]

  # Calculate the aggregate stability given the CEC
  dt[,D_AS := calc_aggregatestability(B_BT_AK,A_OS_GV,A_K_CEC,A_CA_CEC,A_MG_CEC)]
  
  # Calculate the score of the BodemConditieScore
  dt[, D_BCS := calc_bcs(A_RW_BC, A_BS_BC, A_GV_BC, A_PV_BC, A_AS_BC, A_SV_BC, A_RD_BC, A_SS_BC, A_CO_BC,
                         A_OS_GV, D_PH_DELTA,
                         B_LU_BRP,B_BT_AK)]
  
  # Calculate water retention index 1. Plant Available Water
  dt[,D_P_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_OS_GV,type = 'plant available water')]
  
  # Calculate Water Stress Risk
  dt[,D_WSI := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GT, WSI = 'waterstress')]
  
  # calculate N leaching to groundwater (mgNO3/L)
  dt[,D_NGW := calc_nleach(B_BT_AK, B_LU_BRP, B_GT, D_NLV, B_LG_CBS, leaching_to = "gw")]
  
  # calculate N run-off to surface water (kgN/ha/year)
  dt[,D_NSW := calc_nleach(B_BT_AK, B_LU_BRP, B_GT, D_NLV, B_LG_CBS, leaching_to = "ow")]

  # calculate workability 
  dt[,D_P_WO := calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_BT_AK, B_GLG, B_GHG, B_Z_TWO)]

  return(dt)
}
