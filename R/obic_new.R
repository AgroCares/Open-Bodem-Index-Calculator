#' Calculate the Open Bodem Index score for one field
#' 
#' This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI) for a single field.
#' 
#' @param refid (character) a field id
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' 
#' @import data.table
#' 
#' @export
obic_field <- function(refid, B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_SOILTYPE_AGR,
                       A_SOM_LOI,A_CLAY_MI) {

  
  # check input
  
  # collect input for field and crop rotation
  dt.field <- CJ(refid = refid, B_LU_BRP = B_LU_BRP)
  
  # collect input for soil properties
  dt.soil <- data.table(B_SC_WENR = B_SC_WENR,
                        B_GWL_CLASS = B_GWL_CLASS,
                        B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                        A_SOM_LOI = A_SOM_LOI,
                        A_CLAY_MI = A_CLAY_MI
                        )
  
  
  # update soil properties
  
    # check format B_SC_WENR and B_GWL_CLASS
    dt.soil[, B_SC_WENR := format_soilcompaction(B_SC_WENR)]
    dt.soil[, B_GWL_CLASS := format_gwt(B_GWL_CLASS)]
  
    # calculate soil sealing risk
    dt.soil[, D_SE := calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
  
    # calculate the crumbleability of the soil
    dt.soil[, D_CR := calc_crumbleability(A_SOM_LOI, A_CLAY_MI,A_PH_CC)]
  
    # calculate bulk density
    dt.soil[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  
  # update field properties
    
    # calculate the rootable depth of the soil
    dt.field[, D_RD := calc_root_depth(B_LU_BRP)]
  
    # Calculate the grass age
    dt.field[, D_GA := calc_grass_age(ID = refid, B_LU_BRP)]
    
    # Calculate the crop rotation fraction
    dt.field[, D_CP_STARCH := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "starch")]
    dt.field[, D_CP_POTATO := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "potato")]
    dt.field[, D_CP_SUGARBEET := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "sugarbeet")]
    dt.field[, D_CP_GRASS := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "grass")]
    dt.field[, D_CP_MAIS := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "mais")]
    dt.field[, D_CP_OTHER := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "other")]
    dt.field[, D_CP_RUST := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "rustgewas")]
    dt.field[, D_CP_RUSTDEEP := calc_rotation_fraction(ID = refid, B_LU_BRP, crop = "rustgewasdiep")]
    
    
    
  # Calculate the amount of total organic carbon
  dt[, D_OC := calc_organic_carbon(A_OS_GV, D_BDS, D_RD)]
  
  # Calculate a simple organic matter balance
  dt[,D_OS_BAL := calc_sombalance(A_OS_GV, A_P_PAL, A_P_WA, B_LU_BRP, M_M3, M_M6)]
  
  
  
  # Calculate the NLV
  dt[, D_NLV := calc_nlv(A_N_TOT, D_OC, B_LU_BRP, B_BT_AK, D_BDS, A_CN_RAT, D_GA)]
  
  # Calculate the potassium availability
  dt[, D_K := calc_potassium_availability(A_K_CC, A_K_CEC,A_CEC_CO, A_PH_CC, A_OS_GV, A_CLAY_MI, B_LU_BRP, B_BT_AK)]
  
  # Calculate the PBI
  dt[, D_PBI := calc_phosphate_availability(A_P_PAL, A_P_PAE, A_P_WA,B_LU_BRP)]
  
    
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
  dt[, D_MG := calc_magnesium_availability(A_MG_CC,A_PH_CC,A_OS_GV,A_CEC_CO, A_K_CC,
                                           A_K_CEC,A_CLAY_MI,B_BT_AK,B_LU_BRP)]
  
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
  
  
  
  }
