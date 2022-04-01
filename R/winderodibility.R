#' Calculate indicator for wind erodibility
#'
#' This function calculates the risk for wind erodibility of soils, derived from Van Kerckhoven et al. (2009) and Ros & Bussink (2013)
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#'
#' @examples 
#' calc_winderodibility(B_LU_BRP = 265, A_CLAY_MI = 4, A_SILT_MI = 15)
#' calc_winderodibility(B_LU_BRP = c(265,1019), A_CLAY_MI = c(4,18), A_SILT_MI = c(15,65))
#' 
#' @return 
#' The vulnerability of the soil for wind erosion. A numeric value.
#'   
#' @export
calc_winderodibility <- function(B_LU_BRP,A_CLAY_MI,A_SILT_MI) {
  
  id = crop_code = crop_n = loam = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_CLAY_MI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    A_CLAY_MI = A_CLAY_MI,
    A_SILT_MI = A_SILT_MI,
    value = NA_real_
  )
  
  # add crop names
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # calculate silt + clay = loam content
  dt[,loam := A_CLAY_MI + A_SILT_MI]
  
  # WEF function applicable over range 3-100%
  dt[loam <= 3,loam := 3]
  
  # Evaluate the wind erodibility factor (WEF)
  dt[,value := -0.286 * log(loam) + 1.3264]
  
  # set WEF on zero for all non-arable crops
  dt[crop_n != "akkerbouw",value := 0]
  
  # restrict values between 0 and 1
  dt[, value := pmax(pmin(value, 1), 0)]
  
  # return Wind Erodibility Factor
  setorder(dt, id)
  value <- dt[, value]
  
  # return
  return(value)
  
}
#' Calculate indicator for wind erodibility
#'
#' This function calculates the indicator for the resistance of the soil against wind erosion.
#'  
#' @param D_P_DU (numeric) The value for wind erodibility factor (WEF) as calculated by \code{\link{calc_winderodibility}}
#'
#' @examples 
#' ind_winderodibility(D_P_DU = 0.85)
#' ind_winderodibility(D_P_DU = c(0.15,0.6,0.9))
#'  
#' @return 
#' The evaluated score for the soil function to avoid soil damage due to wind erosion. A numeric value between 0 and 1.
#' 
#' @export
ind_winderodibility <- function(D_P_DU) {
  
  # Check inputs
  checkmate::assert_numeric(D_P_DU, lower = 0, upper = 1, any.missing = FALSE)
  
  # Evaluate the wind erodibility factor (WEF)
  value <- (1 - D_P_DU)
  
  # return value
  return(value)
  
}
