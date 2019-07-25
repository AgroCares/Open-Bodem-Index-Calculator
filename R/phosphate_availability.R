#' Calculate the phosphate availability (PBI)
#' 
#' This function calculates the phosphate availability. This value can be evaluated by \code{\link{ind_phosphate_availability}}
#' 
#' @param A_P_PAL (numeric) The P-AL content of the soil
#' @param A_P_PAE (numeric) The P-CaCl2 content of the soil
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_phosphate_availability <- function(A_P_PAL, A_P_PAE, B_LU_BRP) {
  
  # Load in the crops dataset
  crop_code = crop_phosphate = id = NULL
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check input
  arg.length <- max(length(A_P_PAL), length(A_P_PAE), length(B_LU_BRP))
  checkmate::assert_numeric(A_P_PAL, lower = 8, upper = 70, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_PAE, lower = 0.3, upper = 5, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect the data into a table
  dt <- data.table(
    id = 1:arg.length,
    A_P_PAL = A_P_PAL,
    A_P_PAE = A_P_PAE,
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  setkey(dt, B_LU_BRP)
  dt <- crops.obic[dt]
  setorder(dt, id)
  
  # Calculate the phosphate availability (PBI, unit?)
  dt[crop_phosphate == "gras", value := 2 + 2.5 * log(A_P_PAE) + 0.036 * A_P_PAL / A_P_PAE]
  dt[crop_phosphate == "mais", value := A_P_PAE + 0.05 * (A_P_PAL / A_P_PAE)]
  
  value <- dt[, value]
  
  return(value)

}

#' Calculate the indicator for the the phosphate availability
#' 
#' This function calculates the indicator for the phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' 
#' @param D_PBI (numeric) The value of phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' 
#' @export
ind_phosphate_availability <- function(D_PBI) {
  
  # Check inputs
  checkmate::assert_numeric(D_PBI, lower = 0, upper = 7, any.missing = FALSE)
  
  # Evaluate the phosphate availability
  value <- OBIC::evaluate_logistic(D_PBI, b = 1.3, x0 = 1.3, v = 0.35)
  
  # return output
  return(value)
}
