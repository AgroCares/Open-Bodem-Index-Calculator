#' Calculate the index for the CEC
#' 
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#' 
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' 
#' @import data.table
#' 
#' @export
calc_cec <- function(A_N_TOT, D_OC, B_LU_BRP, B_BT_AK, D_BDS, A_CN_RAT, D_GA) {
  
  a = c.ass = c.diss = id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_N_TOT), length(D_OC), length(B_LU_BRP), length(B_BT_AK), length(D_BDS), length(D_GA))
  checkmate::assert_numeric(A_N_TOT, lower = 0, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_OC, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(D_BDS, lower = 0, upper = 1500, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CN_RAT, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_GA, lower = 0, upper = 99, len = arg.length)
  
  # Settings
  param.a <- 20 # Age of organic matter
  param.b <- 2^((14.1 - 9)/ 9) # Temperature correction
  param.cn.micro <- 10 # CN ratio of micro organisms
  param.t <- 5 / 12 # 5 months a year
  param.diss.micro <- 2 # Dissimilation : assimilation ratio of micro organisms
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_N_TOT = A_N_TOT,
    D_OC = D_OC,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    D_BDS = D_BDS,
    A_CN_RAT = A_CN_RAT,
    D_GA = D_GA,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  

  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}

#' Calculate the indicator for the CEC
#' 
#' This function calculates the indicator for the the CEC  of the soil by using the CEC-index calculated by \code{\link{calc_cec}}
#' 
#' @param D_CEC (numeric) The value of NLV  calculated by \code{\link{calc_cec}}
#' 
#' @export
ind_cec <- function(D_CEC) {
  
  # Check inputs
  checkmate::assert_numeric(D_CEC, lower = -30, upper = 250, any.missing = FALSE)
  
  # Evaluate the nitrogen
  value <- OBIC::evaluate_parabolic(D_CEC, x.top = 120)
  
  # return output
  return(value)
}
