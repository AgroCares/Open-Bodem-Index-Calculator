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
calc_k_availability <- function(A_K_CC, A_CEC_CO, B_LU_BRP) {
  
  id = crop_category = soiltype.n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(A_K_CC), length(A_CEC_CO),length(B_LU_BRP))
  checkmate::assert_numeric(A_K_CC, lower = 0, upper = 225, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect the data
  dt <- data.table(
    id = 1:arg.length,
    A_K_CC = A_K_CC,
    A_CEC_CO = A_CEC_CO,
    B_LU_BRP = B_LU_BRP,
    value := NA_real_
  )
  
  # merge with crop and soil classification tables
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Calculate the K availability for grassland
  dt.grass <- dt[crop_category == 'grasland']
  
  # add K-index where CEC is maximized at 400 mmol+ / kg
  dt.grass[A_CEC_CO > 400, A_CEC_CO := 400]
  dt.grass[,value := 4 - exp(-0.08551 * A_K_CC + 0.5264 * log(A_K_CC) - 0.001607 * A_CEC_CO + 
                               0.1275 * log(A_CEC_CO) + 0.010836 * A_K_CC * log(A_CEC_CO))]

  # Calculate the K availability for maize
  dt.maize <- dt[crop_category == 'mais']
  dt.maize[,value := 1 - (120 - A_K_CC) / 120]
    
  # Calculate the K availability for arable parcels
  dt.maize <- dt[crop_category == 'akkerbouw']
  
  
  # Calculate the K availability for nature
  dt.nature <- dt[crop_category == 'natuur']
  dt.nature[,value := 0]
  
  
  
  setorder(dt, id)
  
  
}