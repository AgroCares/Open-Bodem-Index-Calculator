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
#' @param A_PH_CC (numeric) The acidity of the soil, determined in 0.01M CaCl2 (-)
#' 
#' @import data.table
#' 
#' @export
obic_field <- function(refid, B_LU_BRP,B_SC_WENR,B_GWL_CLASS,B_SOILTYPE_AGR,
                       A_SOM_LOI,A_CLAY_MI,A_PH_CC) {

  # add visual bindings
  D_SE = D_CR = D_BDS = D_RD = D_GA = NULL
  D_CP_STARCH = D_CP_POTATO = D_CP_SUGARBEET = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_CP_RUST = D_CP_RUSTDEEP = NULL
  
  # check input
  
  # collect input for field and crop rotation
  dt.field <- CJ(refid = refid, B_LU_BRP = B_LU_BRP)
  
  # collect input for soil properties
  dt.soil <- data.table(B_SC_WENR = B_SC_WENR,
                        B_GWL_CLASS = B_GWL_CLASS,
                        B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                        A_SOM_LOI = A_SOM_LOI,
                        A_CLAY_MI = A_CLAY_MI,
                        A_PH_CC = A_PH_CC
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
    
    
  
  
}



#' Calculate the Open Bodem Index score for one field
#' 
#' This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI) for a single field.
#' 
#' @param B_LU_BRP (numeric) a series with crop codes given the crop rotation plan (source: the BRP) 
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006).
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The
#' @param A_SILT_MI (numeric) The
#' @param A_PH_CC (numeric) The
#' @param A_CACO3_IF (numeric) The
#' @param A_N_RT (numeric) The
#' @param A_CN_FR (numeric) The
#' @param A_COM_FR (numeric) The
#' @param A_S_RT (numeric) The
#' @param A_N_PMN (numeric) The
#' @param A_P_AL (numeric) The
#' @param A_P_CC (numeric) The
#' @param A_P_WA (numeric) The
#' @param A_CEC_CO (numeric) The
#' @param A_CA_CO_PO (numeric) The
#' @param A_MG_CO_PO (numeric) The
#' @param A_K_CO_PO (numeric) The
#' @param A_K_CC (numeric) The
#' @param A_MG_CC (numeric) The
#' @param A_MN_CC (numeric) The
#' @param A_ZN_CC (numeric) The
#' @param A_CU_CC (numeric) The
#' @param A_EW_BCS (numeric) The presence of earth worms (score 0-1-2)
#' @param A_SC_BCS (numeric) The presence of compaction of subsoil (score 0-1-2)
#' @param A_GS_BCS (numeric) The presence of waterlogged conditions, gley spots (score 0-1-2)
#' @param A_P_BCS (numeric) The presence / occurrence of water puddles on the land, ponding (score 0-1-2)
#' @param A_C_BCS (numeric) The presence of visible cracks in the top layer (score 0-1-2)
#' @param A_RT_BCS (numeric) The presence of visible tracks / rutting or trampling on the land (score 0-1-2)
#' @param A_RD_BCS (integer) The rooting depth (score 0-1-2)
#' @param A_SS_BCS (integer) The soil structure (score 0-1-2)
#' @param A_CC_BCS (integer) The crop cover on the surface (score 0-1-2)
#' @param M_COMPOST (numeric) The frequency that compost is applied (every x years)
#' @param M_GREEN (boolean) measure. are catchcrops sown after main crop (option: yes or no)
#' @param M_NONBARE (boolean) measure. is parcel for 80 percent of the year cultivated and 'green' (option: yes or no)
#' @param M_EARLYCROP (boolean) measure. use of early crop varieties to avoid late harvesting (option: yes or no)
#' @param M_SLEEPHOSE (boolean) measure. is sleepslangbemester used for slurry application (option: yes or no)
#' @param M_DRAIN (boolean) measure. are under water drains installed in peaty soils (option: yes or no)
#' @param M_DITCH (boolean) measure. are ditched maintained carefully and slib applied on the land (option: yes or no)
#' @param M_UNDERSEED (boolean) measure. is grass used as second crop in between maize rows (option: yes or no) 
#' 
#' @import data.table
#' 
#' @export
obic_field_test <- function(B_SOILTYPE_AGR,B_GWL_CLASS,B_SC_WENR,B_HELP_WENR,B_AER_CBS,
                            B_LU_BRP, 
                            A_SOM_LOI, A_SAND_MI, A_SILT_MI, A_CLAY_MI,A_PH_CC,A_CACO3_IF,
                            A_N_RT,A_CN_FR,A_COM_FR, A_S_RT,A_N_PMN,
                            A_P_AL, A_P_CC, A_P_WA,
                            A_CEC_CO,A_CA_CO_PO, A_MG_CO_PO, A_K_CO_PO,
                            A_K_CC, A_MG_CC, A_MN_CC, A_ZN_CC, A_CU_CC,
                            A_C_BCS, A_CC_BCS,A_GS_BCS,A_P_BCS,A_RD_BCS,A_EW_BCS,A_SS_BCS,A_RT_BCS,A_SC_BCS,
                            M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED) {
  
  
  # check input
  
  # combine input into one data.table
  # field properties start with B, soil analysis with A, Soil Visual Assessment ends with BCS and management starts with M
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_AER_CBS = B_AER_CBS,
                   B_GWL_CLASS =  B_GWL_CLASS,
                   B_SC_WENR = B_SC_WENR,
                   B_HELP_WENR = B_HELP_WENR,
                   A_SOM_LOI = A_SOM_LOI, 
                   A_SAND_MI = A_SAND_MI, 
                   A_SILT_MI = A_SILT_MI, 
                   A_CLAY_MI = A_CLAY_MI,
                   A_PH_CC = A_PH_CC,
                   A_CACO3_IF = A_CACO3_IF,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   A_COM_FR = A_COM_FR, 
                   A_S_RT = A_S_RT,
                   A_N_PMN = A_N_PMN,
                   A_P_AL = A_P_AL,
                   A_P_CC = A_P_CC, 
                   A_P_WA = A_P_WA,
                   A_CEC_CO = A_CEC_CO,
                   A_CA_CO_PO = A_CA_CO_PO, 
                   A_MG_CO_PO = A_MG_CO_PO, 
                   A_K_CO_PO = A_K_CO_PO,
                   A_K_CC = A_K_CC, 
                   A_MG_CC = A_MG_CC, 
                   A_MN_CC = A_MN_CC, 
                   A_ZN_CC = A_ZN_CC, 
                   A_CU_CC = A_ZN_CC,
                   A_C_BCS = A_C_BCS, 
                   A_CC_BCS = A_CC_BCS,
                   A_GS_BCS = A_GS_BCS,
                   A_P_BCS = A_P_BCS,
                   A_RD_BCS = A_RD_BCS,
                   A_EW_BCS = A_EW_BCS,
                   A_SS_BCS = A_SS_BCS,
                   A_RT_BCS = A_RT_BCS,
                   A_SC_BCS = A_SC_BCS,
                   M_NONBARE = M_NONBARE, 
                   M_EARLYCROP = M_EARLYCROP, 
                   M_SLEEPHOSE = M_SLEEPHOSE,
                   M_DRAIN = M_DRAIN,
                   M_DITCH = M_DITCH,
                   M_UNDERSEED = M_UNDERSEED,
                   M_COMPOST = M_COMPOST,
                   M_GREEN = M_GREEN
                   )
  
  # step 1. preprocess
  
  # step 2. calculate indicators
  
  # step 3. calculate scores
  
  # step 4. add management recommendations
  
  
  # prepare output object for indicator values (for the moment, will be extended)
  out.ind <- data.table(I_C_N = 1, I_C_P = 1,I_C_K = 1,I_C_MG = 1,I_C_S = 1,I_C_PH = 1,I_C_CEC= 1,
                        I_C_CU = 1,I_C_ZN = 1,
                        I_P_CR = 0.5,I_P_SE= 0.5,I_P_MS = 0.5,I_P_BC = 0.5,I_P_DU = 0.5,I_P_CO = 0.5,
                        I_P_WRI = 0.5,I_P_CEC = 0.1,
                        I_B_DI = 0.9,I_B_SF = 0.8,
                        I_E_NGW = 0.4, I_E_NSW = 0.5,
                        I_M = 0.2, I_BCS = 0.8)
                        
  # prepare output object for scores (absolute and relative)
  out.score <- data.table(S_C_OBI_A = 0.6,
                          S_P_OBI_A = 0.6,
                          S_B_OBI_A = 0.6,
                          S_M_OBI_A = 0.6,
                          S_T_OBI_A = 0.6,
                          S_C_OBI_R = 0.6,
                          S_P_OBI_R = 0.6,
                          S_B_OBI_R = 0.6,
                          S_M_OBI_R = 0.6,
                          S_T_OBI_R = 0.6)
    
  # prepare output object for recommendations
  out.recom <- data.table(RM_C_1 = 'M1',
                          RM_C_2 = 'M8',
                          RM_C_3 = 'M7',
                          RM_P_1 = 'M1',
                          RM_P_2 = 'M1',
                          RM_P_3 = 'M1',
                          RM_B_1 = 'M100',
                          RM_B_2 = 'M1',
                          RM_B_3 = 'M11')
    
    
  # combine both outputs
  out <- data.table(out.ind,out.score,out.recom)
  
  # return output
  return(out)
}

