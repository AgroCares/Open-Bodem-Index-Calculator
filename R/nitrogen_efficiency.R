#' Calculate nitrogen use efficiency and leaching based on N surplus
#' 
#' This function gives an indication of the nitrogen use efficiency, the function calculates the N surplus and the resulting N leaching
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soilBRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016) 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio
#' @param D_GA (numeric) The age of the grass if present
#' @param A_P_AL (numeric) The P-AL content of the soil
#' @param A_P_CC (numeric) The plant available P content, extracted with 0.01M CaCl2 (mg / kg)
#' @param A_P_WA (numeric) The P-content of the soil extracted with water (mg P2O5 / 100 ml soil)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ / kg), analysed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_K_CC (numeric) The plant available K content, extracted with 0.01M CaCl2 (mg / kg)(\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param M_GREEN (boolean) A soil measure. Are catch crops sown after main crop (optional, option: yes or no)
#' @param norm_fr (numeric) The fraction of the application norm utilized
#' 
#'         
#' @export
calc_n_efficiency <- function(B_LU_BRP,B_SOILTYPE_AGR,B_GWL_CLASS,B_AER_CBS,A_SOM_LOI,A_CLAY_MI,A_N_RT,A_CN_FR,D_GA,
                              A_P_AL,A_P_CC,A_P_WA,A_CEC_CO,A_K_CO_PO,A_K_CC,A_PH_CC,M_GREEN,norm_fr){
  
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),length(norm_fr))
  
  # Check B parameters
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype))
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = c('GtI','GtII','GtIII','GtIV','GtV','GtVI','GtVII','GtVIII',
                                                  'GtIIb','GtIIIb','GtVb'), empty.ok = FALSE)
  checkmate::assert_character(B_AER_CBS, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_AER_CBS, choices = c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                                                  'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                                                  'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                                                  'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                                                  'Veenkoloni\u00EBn en Oldambt','Veenkolonien en Oldambt','Bouwhoek en Hogeland'), empty.ok = FALSE)
  
  # Check SOM and CLAY
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE)
  
  # Check N values
  checkmate::assert_numeric(A_N_RT, lower = 0.1, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CN_FR, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_GA, lower = 0, upper = 99, len = arg.length)

  # check P input parameters for grassland 
  arg.length.grass <- max(length(A_P_AL),length(A_P_CC))
  if(arg.length.grass > 0){
    checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = FALSE, len = arg.length.grass)
    checkmate::assert_numeric(A_P_CC, lower = 0.1, upper = 100, any.missing = FALSE, len = arg.length.grass)
  }
  
  # check P input parameters for arable soils
  arg.length.arable <- length(A_P_WA)
  if(arg.length.arable > 0){
    checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = FALSE, len = arg.length.arable)  
  }
  
  # Check Potasium values
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 600, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)  
  
  # Check pH
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  
  checkmate::assert_numeric(norm_fr, lower = 0.1, upper = 1, any.missing = FALSE, len = arg.length)
  
  
  # Import data into table
  dt <- data.table(ID = 1,
                   B_LU_BRP = B_LU_BRP,
                   B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   B_GWL_CLASS = B_GWL_CLASS,
                   B_AER_CBS = B_AER_CBS,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_N_RT = A_N_RT,
                   A_CN_FR = A_CN_FR,
                   A_P_PAL = A_P_PAL,
                   A_P_CC = A_P_CC,
                   A_P_WA = A_P_WA,
                   A_CEC_CO = A_CEC_CO,
                   A_K_CO_PO = A_K_CO_PO, 
                   A_K_CC = A_K_CC,
                   A_PH_CC = A_PH_CC,
                   D_GA = D_GA,
                   M_GREEN = M_GREEN,
                   norm_fr = norm_fr
                   )
  
  
  # Read table of N leaching fraction
  nleach_table <- as.data.table(OBIC::nleach_table)
  nleach_table <- nleach_table[leaching_to_set == 'gw']
  
  
  # add soil type, crop categories and allowed N dose
  cols <- colnames(crops.obic)[grepl('^crop_cat|^crop_code|^nf_',colnames(crops.obic))]
  dt <- merge(dt, crops.obic[, mget(cols)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Re-categorize crop types
  dt[, croptype.nleach := crop_category]
  dt[crop_category == "natuur" | crop_category == "akkerbouw" , croptype.nleach := "akkerbouw"]
  dt[crop_category == "grasland" , croptype.nleach := "gras"]
  
  # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
  dt <- merge(dt, nleach_table[, list(bodem, gewas, B_GT, nf)], 
              by.x = c("soiltype.n", "croptype.nleach", "B_GWL_CLASS"), 
              by.y = c("bodem", "gewas", "B_GT"), sort = FALSE, all.x = TRUE)
  
  # select the allowed effective N dose (in Dutch: N-gebruiksnorm), being dependent on soil type and region
  sand.south <- c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant')
  dt[grepl('zand|dal',B_SOILTYPE_AGR), n_eff := nf_sand.other]
  dt[grepl('zand|dal',B_SOILTYPE_AGR) & B_AER_CBS %in% sand.south, n_eff := nf_sand.south]
  dt[grepl('klei',B_SOILTYPE_AGR), n_eff := nf_clay]
  dt[grepl('veen',B_SOILTYPE_AGR), n_eff := nf_peat]
  dt[grepl('loess',B_SOILTYPE_AGR), n_eff := nf_loess]
  
  # remove columns not needed any more
  cols <- colnames(dt)[grepl('^nf_',colnames(dt))]
  dt[,(cols) := NULL]
  
  
  # Calculate bulk density and OC content of the soil
  dt[, D_BDS := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI)]
  dt[, D_RD := calc_root_depth(B_LU_BRP)]
  dt[, D_OC := calc_organic_carbon(A_SOM_LOI, D_BDS, D_RD)]
  
  # Calculate crop rotation fractions
  dt[, D_CP_STARCH := calc_rotation_fraction(ID, B_LU_BRP, crop = "starch")]
  dt[, D_CP_POTATO := calc_rotation_fraction(ID, B_LU_BRP, crop = "potato")]
  dt[, D_CP_SUGARBEET := calc_rotation_fraction(ID, B_LU_BRP, crop = "sugarbeet")]
  dt[, D_CP_GRASS := calc_rotation_fraction(ID, B_LU_BRP, crop = "grass")]
  dt[, D_CP_MAIS := calc_rotation_fraction(ID, B_LU_BRP, crop = "mais")]
  dt[, D_CP_OTHER := calc_rotation_fraction(ID, B_LU_BRP, crop = "other")]
  
  
  # Calculate P, K and pH status
  dt[,D_PBI := calc_phosphate_availability(B_LU_BRP, A_P_PAL, A_P_CC,A_P_WA)]
  dt[,I_P := ind_phosphate_availability(D_PBI)]
  
  dt[,D_K := calc_potassium_availability(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                         A_CEC_CO, A_K_CO_PO, A_K_CC)]
  dt[,I_K := ind_potassium(D_K,B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI)]
  
  dt[,D_PH_DELTA := calc_ph_delta(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                                  D_CP_STARCH, D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER)]
  dt[,I_PH := ind_ph(D_PH_DELTA)]
  
  
  # Determine correction factor if P, K and pH are optimal
  dt[,cf_pkph := fifelse(I_P > 0.9 & I_K > 0.9 & I_PH > 0.9,0.9,1) ]
  
  # Calculate N sources
  dt[,deposition := 25] # Fixed value
  dt[,nlv := calc_nlv(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA)]
  dt[,fert := n_eff * norm_fr]
  
  # N uptake by catchcrop
  # Check values https://www.wur.nl/upload_mm/c/8/1/6b63d919-1690-4f07-981a-07b3b6a3e7f1_1705577_Oene%20Oenema%20bijlage%201.pdf
  dt[,catchcrop := fifelse(M_GREEN, 75,0)]
  
  # Calculate N surplus
  dt[,n_sp := max(0,((deposition + nlv + fert) - n_eff - catchcrop) * cf_pkph)]
  
  # compute (potential and soil derived) N leaching to groundwater D_NGW (mg NO3/L) 
  dt[, value := nf * n_sp]
  
  return(dt[,value])
  
}
