#' Calculate the capacity of soils to supply Magnesium
#' 
#' This function calculates an index for the availability of Magnesium in soil
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The acidity of the soil, measured in 0.01M CaCl2 (-)
#' @param A_CEC_CO (numeric) The cation exchange capacity of the soil (mmol+ per kg), analyzed via Cobalt-hexamine extraction
#' @param A_K_CO_PO (numeric) The occupation of the CEC with potassium (\%)
#' @param A_MG_CC (numeric) The plant available content of Mg in the soil (mg  Mg per kg) extracted by 0.01M CaCl2
#' @param A_K_CC (numeric) The plant available potassium, extracted with 0.01M CaCl2 (mg per kg), 
#' 
#' @import data.table
#' 
#' @examples
#' calc_magnesium_availability(B_LU_BRP = 265, B_SOILTYPE_AGR = 'dekzand',
#' A_SOM_LOI = 3.5,A_CLAY_MI = 8.5,A_PH_CC = 5.4, 
#' A_CEC_CO = 185,A_K_CO_PO = 4.5,A_MG_CC = 125,A_K_CC = 65)
#' 
#' @return 
#' An index representing the availability of Magnesium in a soil. A numeric value.
#' 
#' @export
calc_magnesium_availability <- function(B_LU_BRP,B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI,
                                        A_PH_CC, A_CEC_CO,A_K_CO_PO,A_MG_CC,A_K_CC) {
  
  # set variables to NULL
  A_MG_NC = A_PH_KCL = A_SLIB_MI = cF = A_K_CO = kg1 = kg2 = kg = mg_pred = mg_aim = NULL
  id = crop_code = soiltype = soiltype.n = crop_category = NULL
  
  # Load in the datasets for soil and crop types
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(A_MG_CC), length(A_PH_CC), length(A_SOM_LOI), length(A_CEC_CO), 
                    length(A_K_CO_PO), length(A_CLAY_MI), length(B_SOILTYPE_AGR), length(B_LU_BRP))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_MG_CC, lower = 1, upper = 1100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 1, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 1, upper = 600, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CO_PO, lower = 0.1, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
   
  # Settings
  param.re = 180 # protein content of first cut grassland in spring (g/kg)
  param.k = 33.9 # potassium content of first cut grass in spring (g/kg)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_PH_CC = A_PH_CC,
    A_CEC_CO = A_CEC_CO,
    A_K_CO_PO = A_K_CO_PO,
    A_MG_CC = A_MG_CC,
    A_K_CC = A_K_CC,
    value = NA_real_
  )
  
  # add crop names and soiltypes
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Calculate the Mg availability for arable land -----
  dt.arable <- dt[crop_category == "akkerbouw"]
  dt.arable[,value := A_MG_CC]
  
  # Calculate the Mg availability for maize land -----
  dt.maize <- dt[crop_category == "mais"]
  dt.maize[,value := A_MG_CC]
  
  # Calculate Mg availability for grassland on sandy and loamy soils -----
  dt.grass.sand <- dt[crop_category == "grasland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR)]
  dt.grass.sand[,value := A_MG_CC]
  
  # Calculate Mg availability for grassland on clay and peat soils ----- 
  dt.grass.other <- dt[crop_category == "grasland" & grepl('klei|veen',B_SOILTYPE_AGR)]
  
  # convert CaCl2 method for Mg to former NaCl method
  dt.grass.other[,A_MG_NC := A_MG_CC * 1.987 - 6.8]
  
  # estimate pH-kcl from pH-cacl2
  dt.grass.other[,A_PH_KCL := (A_PH_CC - 0.5262)/0.9288]
  
  # estimate slib via lutum-slib-ratio (Source: bemestingsadvies.nl)
  dt.grass.other[grepl('zeeklei|veen|moerige_klei',B_SOILTYPE_AGR),A_SLIB_MI := A_CLAY_MI / 0.67]
  dt.grass.other[grepl('rivierklei',B_SOILTYPE_AGR),A_SLIB_MI := A_CLAY_MI / 0.61]
  dt.grass.other[grepl('maasklei',B_SOILTYPE_AGR),A_SLIB_MI := A_CLAY_MI / 0.55]
  
  # additional temporary variable called cF (Source: Adviesbasis, 2002)
  dt.grass.other[A_SOM_LOI <= 3,cF:= 2.08]
  dt.grass.other[A_SOM_LOI > 3,cF:= 5.703 * A_SOM_LOI^-0.7996]
  
  # calculate A_K_CO in mg K/ kg grond
  dt.grass.other[,A_K_CO := A_CEC_CO * A_K_CO_PO * 0.01 * 39.098]
  
  # estimate K-index from K-CEC (A_K_CO, mg K/ kg grond) and K-PAE (mg K/ kg grond) (Source: NMI notitie 1436.N.11)
  dt.grass.other[,kg1 := (1.56 * A_K_CC - 17 + 0.29 * A_CEC_CO) * cF * 0.12046]
  dt.grass.other[,kg2 := A_K_CO * cF * 0.12046]
  dt.grass.other[,kg := 0.5 * (kg1 + kg2)]
  
  # remove columns not needed any more
  dt.grass.other[,c('kg1','kg2','cF'):=NULL]
  
  # calculate expected Mg-content in grass (g/kg) in the spring on peat soils (den Boer 2003)
  dt.grass.other[grepl('veen', B_SOILTYPE_AGR),mg_pred := pmax(3.3284 + 0.001058* A_MG_NC - 0.02059* kg -0.01163*A_CLAY_MI -0.2691* A_PH_KCL, 0)]
  
  # calculate expected Mg-content in grass (g/kg) in the spring on clay soils (den Boer 2003)
  dt.grass.other[grepl('klei',B_SOILTYPE_AGR),mg_pred := pmax(2.6688 + 0.001563* A_MG_NC - 0.01738* kg -0.04175* A_SOM_LOI -0.015128* A_SLIB_MI, 0)]
  
  # estimate optimum mg-content in grass in spring (Kemp, in Handboek Melkveehouderij)
  dt.grass.other[,mg_aim := (2.511 - 86.46/((param.k * param.re)^0.5))^2]
  
  # Mg index
  dt.grass.other[,value := pmin(100 * (mg_pred /2.0), 100)] 
  
  # nature parcels
  dt.nature <- dt[crop_category == "natuur"]
  dt.nature[,value := 0]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass.sand,dt.grass.other, dt.arable,dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  value <- ifelse(value < -1, -1, value)
  
  # return value: be aware, index is different for different land use and soil combinations
  return(value)
}





#' Calculate the indicator for Magnesium
#' 
#' This function calculates the indicator for the the Magnesium content of the soil by using the Mg-availability calculated by \code{\link{calc_magnesium_availability}}
#' 
#' @param D_MG (numeric) The value of Mg calculated by \code{\link{calc_magnesium_availability}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_SOILTYPE_AGR (character) The type of soil
#' 
#' @examples 
#' ind_magnesium(D_MG = 125, B_LU_BRP = 265, B_SOILTYPE_AGR = 'dekzand')
#' ind_magnesium(D_MG = c(125,35), B_LU_BRP = c(265,256), B_SOILTYPE_AGR = rep('dekzand',2))
#'  
#' @return 
#' The evaluated score for the soil function to supply magnesium for crop uptake. A numeric value.
#' 
#' @export
ind_magnesium <- function(D_MG,B_LU_BRP,B_SOILTYPE_AGR) {
  
  id = crop_code = soiltype = soiltype.n = crop_category = NULL
  
  # Load in the datasets for crop types
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(D_MG),length(B_LU_BRP))
  checkmate::assert_numeric(D_MG, lower = -1, upper = 1200, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # make data.table to save scores
  dt = data.table(
    id = 1:arg.length,
    D_MG = D_MG,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    value = NA_real_
  )
  
  # add crop names
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # Evaluate Mg availability for arable land and mais -----
  dt.arable <- dt[grepl('akkerbouw|mais',crop_category)]
  dt.arable[,value := evaluate_logistic(D_MG, b = 0.206, x0 = 45, v = 2.39)]
  
  # Evaluate Mg availability for grassland on sandy and loamy soils -----
  dt.grass.sand <- dt[crop_category == "grasland" & grepl('zand|loess|dalgrond',B_SOILTYPE_AGR)]
  dt.grass.sand[,value := evaluate_logistic(D_MG, b = 0.075, x0 = 80, v = 2)]
  
  # Evaluate Mg availability for grassland on clay and peat soils ----- 
  dt.grass.other <- dt[crop_category == "grasland" & grepl('klei|veen',B_SOILTYPE_AGR)]
  dt.grass.other[,value := evaluate_logistic(D_MG, b = 0.15, x0 = 75, v = 1)]
  
  # Evaluate Mg availability for nature parcels
  dt.nature <- dt[crop_category == "natuur"]
  dt.nature[,value := 1]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass.sand,dt.grass.other, dt.arable,dt.nature), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return output
  return(value)
}
