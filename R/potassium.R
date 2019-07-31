#' Calculate the K availability
#' 
#' This function calculates the K availability of a soil.
#' 
#' @param A_K_CC (numeric) The K-CaCl2 content of the soil in mg K / kg
#' @param A_CEC_CO (numeric) The CEC content of the soil (mmol+ per kg)
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_k_availability <- function(A_K_CC, A_K_CEC,A_CEC_CO, A_PH_CC, A_OS_GV, A_CLAY_MI, 
                                B_LU_BRP, B_BT_AK) {
  
  id = crop_category = soiltype.n = crop_code = soiltype = NULL
  b = cF = kindex1 = kindex2 = A_PH_KCL = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(A_OS_GV), length(A_CEC_CO), length(A_K_CEC), 
                    length(A_K_CC), length(A_CLAY_MI), length(B_BT_AK), length(B_LU_BRP))
  checkmate::assert_numeric(A_PH_CC, lower = 3, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 800, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CEC, lower = 0, upper = 20, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  
  # Collect the data
  dt <- data.table(
    id = 1:arg.length,
    A_K_CC = A_K_CC,
    A_CEC_CO = A_CEC_CO,
    A_K_CEC = A_K_CEC,
    a_PH_CC = A_PH_CC,
    A_OS_GV = A_OS_GV,
    A_CLAY_MI = A_CLAY_MI,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    value := NA_real_
  )
  
  # merge with crop and soil classification tables
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Calculate the K availability for grassland (CBGV, 2019)
  dt.grass <- dt[crop_category == 'grasland']
  
  # add K-index where CEC is maximized at 400 mmol+ / kg
  dt.grass[A_CEC_CO > 400, A_CEC_CO := 400]
  dt.grass[,value := 4 - exp(-0.08551 * A_K_CC + 0.5264 * log(A_K_CC) - 0.001607 * A_CEC_CO + 
                               0.1275 * log(A_CEC_CO) + 0.010836 * A_K_CC * log(A_CEC_CO))]

  # Calculate the K availability for maize (CBGV, 2019)
  dt.maize <- dt[crop_category == 'mais']
  dt.maize[,value := 1 - (120 - A_K_CC) / 120]
    
  # Calculate the K availability for arable crops (Ros & Bussink, 2011)
  dt.arable <- dt[crop_category == 'akkerbouw']
  
    # derive b-factor, texture dependent correction
    dt.arable[grepl('duin|rivier|maas|klei',soiltype) & A_OS_GV <= 10 & A_CLAY_MI <= 11, b := 1.513]
    dt.arable[grepl('duin|rivier|maas|klei',soiltype) & A_OS_GV <= 10 & A_CLAY_MI > 11, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]
    dt.arable[grepl('zeeklei',soiltype) & A_OS_GV > 10 & A_CLAY_MI <= 5, b := 1.513]
    dt.arable[grepl('zeeklei',soiltype) & A_OS_GV > 10 & A_CLAY_MI > 5, b := 0.60226 + 1.27576 /(1 + 0.09891 * A_CLAY_MI)]
    
    # pH-KCl needed (not higher than pH is 7)
    dt.arable[,A_PH_KCL := pmin(7,(A_PH_CC - 0.5262)/0.9288)]
    
    # correction factor for texture and OS (the so called F-factor)
    dt.arable[grepl('zand|dal|veen',soiltype), cF := 20 / (10 + A_OS_GV)]
    dt.arable[grepl('duin|rivier|maas|klei',soiltype) & A_OS_GV <= 10, cF := b /(0.15 * A_PH_KCL-0.05)]
    dt.arable[grepl('zeeklei',soiltype) & A_OS_GV > 10, cF := b]
    
    # calculate K-index based on PAE
    dt.arable[grepl('zand|dal|veen',soiltype),kindex1 := A_K_CC * cF * 0.12046]
    dt.arable[grepl('zand|dal|veen',soiltype),kindex2 := (A_K_CEC - 0.17 * A_CEC_CO) * cF * 0.12046]
    dt.arable[grepl('klei|loess',soiltype),kindex1 := (1.56 * A_K_CC - 17 + 0.29 * A_K_CEC) * cF * 0.12046]
    dt.arable[grepl('klei|loess',soiltype),kindex2 := A_K_CEC * cF * 0.12046]
    dt.arable[,value := 0.5 * (kindex1 + kindex2)]
    
  # Calculate the K availability for nature
  dt.nature <- dt[crop_category == 'natuur']
  dt.nature[,value := 0]
  
 
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.arable,dt.grass, dt.maize,dt.nature), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return value: be aware, index is different for different land use and soil combinations
  return(value)
  
  
}