#' Calculate soil sealing risk
#' 
#' This function calculates the risks of soil sealing.  This value can be evaluated by \code{\link{ind_sealing}}
#' 
#' @param A_CLAY_MI (numeric) The percentage A_CLAY_MI present in the soil
#' @param A_OS_GV (numeric) The organic matter content of soil in percentage
#' 
#' @import data.table
#' 
#' @importFrom stats approxfun
#' 
#' @export
calc_sealing_risk <- function(A_CLAY_MI, A_OS_GV) {
  
  # Check input
  arg.length <- max(length(A_CLAY_MI), length(A_OS_GV))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  
  # Setup a table with all the information
  value.A_CLAY_MI = cor.A_OS_GV = value.A_CLAY_MI = NULL
  dt <- data.table(
    A_CLAY_MI = A_CLAY_MI,
    A_OS_GV = A_OS_GV, 
    value.A_CLAY_MI = NA_real_,
    cor.A_OS_GV = NA_real_,
    value = NA_real_
  )
  df.lookup <- data.frame(
    A_CLAY_MI = c(4, 6, 9, 10, 17, 25, 30, 100),
    value.A_CLAY_MI = c(7, 6, 3, 2, 4, 8, 9, 10),
    cor.A_OS_GV = c(0.4, 0.6, 0.8, 1, 0.7, 0.4, 0.3, 0)
  )
  
  # Calculate value.A_CLAY_MI
  fun.A_CLAY_MI <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$value.A_CLAY_MI, rule = 2)
  dt[is.na(value), value.A_CLAY_MI := fun.A_CLAY_MI(A_CLAY_MI)]
  
  # Create organic matter correction function and calculate correction for A_OS_GV
  fun.cor.A_OS_GV <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$cor.A_OS_GV, rule = 2)
  dt[is.na(value), cor.A_OS_GV := fun.cor.A_OS_GV(A_CLAY_MI)]
  
  # Calculate the value
  dt[is.na(value), value := value.A_CLAY_MI + cor.A_OS_GV * A_OS_GV]
  value <- dt[, value]
    
  return(value)
}

#' Calculate the soil sealing indicator
#' 
#' This function calculates the indicator for the soil sealing calculated by \code{\link{calc_sealing_risk}}
#' 
#' @param D_SE (numeric) The value of soil sealing calculated by \code{\link{calc_sealing_risk}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
ind_sealing <- function(D_SE, B_LU_BRP) {
  
  # Load in the crops dataset
  crop_code = crop_sealing = id = NULL
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  arg.length <- max(length(D_SE), length(B_LU_BRP))
  checkmate::assert_numeric(D_SE, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect data into a table
  dt <- data.table(
    id = 1:arg.length,
    D_SE = D_SE,
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  setkey(dt, B_LU_BRP)
  dt <- crops.obic[dt]
  setorder(dt, id)

  # Evaluate the sealing for grassland and all other crops
  dt[crop_sealing == "overig", value := OBIC::evaluate_logistic(x = D_SE, b = 1.5, x0 = 0.3, v = 0.35)]
  dt[crop_sealing == "gras", value := 1]
  
  value <- dt[, value]

  # return output
  return(value)
}
