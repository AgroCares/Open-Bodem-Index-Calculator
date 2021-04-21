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
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CACO3_IF (numeric) The
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio
#' @param A_COM_FR (numeric) The
#' @param A_S_RT (numeric) The total Sulpher content of the soil (in mg S per kg)
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil)
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The P-CaCl2 content of the soil
#' @param A_P_WA (numeric) The P-content of the soil extracted with water
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analysed via Cobalt-hexamine extraction
#' @param A_CA_CO_PO (numeric) The The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The The occupation of the CEC with Mg (\%)
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_MG_CC (numeric) The plant available Mg content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_MN_CC (numeric) The plant available Mn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_ZN_CC (numeric) The plant available Zn content, extracted with 0.01M CaCl2 (ug / kg)
#' @param A_CU_CC (numeric) The plant available Cu content, extracted with 0.01M CaCl2 (ug / kg)
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
                            A_C_BCS = NA, A_CC_BCS,A_GS_BCS,A_P_BCS,A_RD_BCS,A_EW_BCS,A_SS_BCS,A_RT_BCS,A_SC_BCS,
                            M_COMPOST,M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE,M_DRAIN,M_DITCH,M_UNDERSEED) {
  
  
  # define variables used within the function
  D_SE = D_CR = D_BDS = D_RD = D_OC = D_OS_BAL = D_GA = D_NL = D_K = D_PBI = NULL
  D_CP_STARCH = D_CP_POTATO = D_CP_SUGARBEET = D_CP_GRASS = D_CP_MAIS = D_CP_OTHER = D_CP_RUST = D_CP_RUSTDEEP = NULL
  D_NLV = D_PH_DELTA = ccalc_ph_delta = D_MAN = D_SOM_BAL = D_P_DU = D_SLV = D_MG = D_CU = D_ZN = D_PMN = D_CEC = NULL
  D_AS =  D_BCS = D_P_WRI = D_WSI_DS = D_WSI_WS = D_NGW = D_NSW = D_P_WO = B_GWL_GLG = B_GWL_GHG = B_Z_TWO = NULL
  ID = NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = I_P_WRI = I_BCS = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = D_P_CO = I_B_DI = I_B_SF = I_B_SB = I_M = NULL
  I_P_DS = I_P_WS = I_P_CEC = D_P_CEC= I_P_WO = I_E_NGW = I_E_NSW = NULL
  leaching_to = NULL
  
  
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
  
  
  
  # Step 1 Pre-processing ------------------
  
  # check format B_SC_WENR and B_GWL_CLASS
  dt[, B_SC_WENR := format_soilcompaction(B_SC_WENR)]
  dt[, B_GWL_CLASS := format_gwt(B_GWL_CLASS)]
  
  # calculate soil sealing risk
  dt[, D_SE := calc_sealing_risk(A_SOM_LOI, A_CLAY_MI)]
  
  # calculate the crumbleability of the soil
  dt[, D_CR := calc_crumbleability(A_SOM_LOI, A_CLAY_MI,A_PH_CC)]
  
  # calculate bulk density
  dt[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI)]
  
  # Determine the root depth of the soil
  dt[, D_RD := calc_root_depth(B_LU_BRP)]
  
  # Calculate the amount of total organic carbon
  dt[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  
  # Calculate a simple organic matter balance
  dt[,D_OS_BAL := calc_sombalance(A_SOM_LOI, A_P_AL, A_P_WA, B_LU_BRP, M_COMPOST, M_GREEN)]
  
  # Calculate the grass age
  dt[, D_GA := calc_grass_age(ID, B_LU_BRP)]
  
  # Calculate the NLV
  dt[, D_NLV := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]

  # Calculate the potassium availability
  dt[, D_K :=  calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                           A_CEC_CO, A_K_CO_PO, A_K_CC)]
  
  # Calculate the PBI
  dt[, D_PBI := calc_phosphate_availability(B_LU_BRP, A_P_AL, A_P_CC, A_P_WA)]
  
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
  dt[, D_PH_DELTA := ccalc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, D_CP_STARCH,
                                    D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]
  
  # Determine the managment
  dt[, D_MAN := calc_management(A_SOM_LOI, B_LU_BRP, B_SOILTYPE_AGR, B_GWL_CLASS, D_SOM_BAL, 
                                D_CP_GRASS, D_CP_POTATO, D_CP_RUST, D_CP_RUSTDEEP, D_GA,
                                M_GREEN, M_NONBARE, M_EARLYCROP, M_SLEEPHOSE, M_DRAIN, M_DITCH, M_UNDERSEED)]
  
  # Calculate the wind erodibility 
  dt[, D_P_DU := calc_winderodibility(B_LU_BRP, A_CLAY_MI, A_SILT_MI)]
  
  # Calculate the sulphur supply
  dt[, D_SLV := calc_slv(A_S_RT, A_SOM_LOI, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS, D_BDS)]
  
  # Calculate the magnesium index
  dt[, D_MG := calc_magnesium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC, A_CEC_CO,
                                           A_K_CO_PO, A_MG_CC, A_K_CC)]
  
  # Calculate the Cu-index
  dt[, D_CU := calc_copper_availability(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, A_K_CC, A_MN_CC, A_CU_CC)]
  
  # Calculate the Zn-index
  dt[, D_ZN := calc_zinc_availability(B_LU_BRP, B_SOILTYPE_AGR, A_PH_CC, A_ZN_CC)]
  
  # Calculate the index for microbial activity
  dt[, D_PMN := calc_pmn(B_LU_BRP, B_SOILTYPE_AGR, A_N_PMN)]
  
  # Calculate the CEC as cationbuffer index
  dt[,D_CEC := calc_cec(A_CEC_CO)]
  
  # Calculate the aggregate stability given the CEC
  dt[,D_AS := calc_aggregatestability(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO)]
  
  # Calculate the score of the BodemConditieScore
  dt[, D_BCS := calc_bcs(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, D_PH_DELTA, A_EW_BCS, A_SC_BCS, A_GS_BCS,
                         A_P_BCS, A_C_BCS, A_RT_BCS, A_RD_BCS, A_SS_BCS, A_CC_BCS)]
  
  # Calculate water retention index 1. Plant Available Water
  dt[,D_P_WRI := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'plant available water')]
  
  # Calculate Drought Stress Risk
  dt[,D_WSI_DS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'droughtstress')]
  
  # Calculate Wetness Stress Risk
  dt[,D_WSI_WS := calc_waterstressindex(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'wettnessstress')]
  
  # calculate N leaching to groundwater (mgNO3/L)
  dt[,D_NGW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "gw")]
  
  # calculate N run-off to surface water (kgN/ha/year)
  dt[,D_NSW := calc_nleach(B_SOILTYPE_AGR, B_LU_BRP, B_GWL_CLASS, D_NLV, B_AER_CBS, leaching_to = "ow")]
  
  # calculate workability 
  dt[,D_P_WO := calc_workability(A_CLAY_MI, A_SILT_MI, B_LU_BRP, B_SOILTYPE_AGR, B_GWL_GLG, B_GWL_GHG, B_Z_TWO)]
  
  # Step 2 Add indicators ------------------
  
  # Evaluate nutrients ------------------------------------------------------
  
  # Nitrogen
  dt[, I_C_N := ind_nitrogen(D_NLV, B_LU_BRP)]
  
  # Phosphorus
  dt[, I_C_P := ind_phosphate_availability(D_PBI)]
  
  # Potassium
  dt[, I_C_K := ind_potassium(D_K,B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI)]
  
  # Magnesium
  dt[, I_C_MG := ind_magnesium(D_MG, B_LU_BRP, B_SOILTYPE_AGR)]
  
  # Sulphur
  dt[, I_C_S := ind_sulpher(D_SLV, B_LU_BRP, B_SOILTYPE_AGR, B_AER_CBS)]
  
  # pH
  dt[, I_C_PH := ind_ph(D_PH_DELTA)]
  
  # CEC
  dt[, I_C_CEC := ind_cec(D_CEC)]
  
  # Copper
  dt[, I_C_CU := ind_copper(D_CU,B_LU_BRP)]
  
  # Zinc
  dt[, I_C_ZN := ind_zinc(D_ZN)]
  
  
  # Evaluate physical -------------------------------------------------------
  
  # Crumbleability
  dt[, I_P_CR := ind_crumbleability(D_CR, B_LU_BRP)]
  
  # Sealing
  dt[, I_P_SE := ind_sealing(D_SE, B_LU_BRP)]
  
  # add indicator for droughtstress
  dt[, I_P_DS := ind_waterstressindex(D_WSI_DS)]
  
  # add indicator for wetness stress
  dt[, I_P_WS := ind_waterstressindex(D_WSI_WS)]
  
  # Dustiness
  dt[, I_P_DU := ind_winderodibility(D_P_DU)]
  
  # Compaction
  dt[, I_P_CO := ind_compaction(B_SC_WENR)]
  
  # Water Retention Index 1. Plant available water in topsoil
  dt[, I_P_WRI := ind_waterretention(D_P_WRI)]
  
  # aggregation stability 
  dt[, I_P_CEC := ind_aggregatestability(D_AS)]
  
  # workability
  dt[, I_P_WO := ind_workability(D_P_WO)]
  
  
  # Evaluate biological -----------------------------------------------------
  
  # Disease / pest resistance
  dt[, I_B_DI := ind_resistance(A_SOM_LOI)]
  
  # Soil life activity
  dt[, I_B_SF := ind_pmn(D_PMN)]
  
  
  # Evaluate data from Visual Soil Assessment -------------------------------
  
  # calculate the soil indicators
  bcs.par <- c('I_B_EW_BCS', 'I_P_SC_BCS', 'I_P_GS_BCS', 'I_P_P_BCS', 'I_P_C_BCS', 'I_P_RT_BCS', 'I_P_RD_BCS',
               'I_P_SS_BCS', 'I_P_CC_BCS')
  dt[,c(bcs.par) := calc_bcs(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI, D_PH_DELTA,
                             A_EW_BCS, A_SC_BCS, A_GS_BCS, A_P_BCS, A_C_BCS, A_RT_BCS, A_RD_BCS, A_SS_BCS, A_CC_BCS,
                             type = 'indicator')]
  
  # overwrite physical functions for compaction and aggregate stability when BCS is available
  dt[,D_P_CO := (3*A_EW_BCS + 3*A_SC_BCS + 3*A_RD_BCS  - 2*A_P_BCS - A_RT_BCS)/18]
  dt[,I_P_CO := fifelse(is.na(D_P_CO),I_P_CO,D_P_CO)]
    
  dt[,D_P_CEC := (3*A_EW_BCS + 3*A_SS_BCS - A_C_BCS)/12]
  dt[,I_P_CEC := fifelse(is.na(D_P_CEC),I_P_CEC,D_P_CEC)]  
  
  
  # Evaluate management ------------------------------------------------------
  dt[, I_M := ind_management(D_MAN, B_LU_BRP, B_SOILTYPE_AGR)]
  
  dt[, I_BCS := ind_bcs(D_BCS)]
  
  # Evaluate Environmental ------------------------------------------------------ 
  
  # N retention groundwater
  dt[, I_E_NGW := ind_nretention(D_NGW, leaching_to = "gw")]
  
  # N retention surfacewater
  dt[, I_E_NSW := ind_nretention(D_NSW, leaching_to = "ow")]
  
  
  
  
  # Step 3 Scoring
  
  
  
  
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

