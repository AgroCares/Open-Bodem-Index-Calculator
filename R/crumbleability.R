#' Calculate the crumbleability
#'
#' This function calculates the crumbleability. This value can be evaluated by \code{\link{ind_crumbleability}}
#' 
#' @param A_SOM_LOI (numeric) The organic matter content of soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_PH_CC (numeric) The pH of the soil, measured in 0.01M CaCl2
#' 
#' @import data.table
#' 
#' @importFrom stats approxfun
#'
#' @export
calc_crumbleability <- function(A_SOM_LOI, A_CLAY_MI, A_PH_CC) {
  
  # Check input
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_PH_CC, lower = 0, upper = 14, any.missing = FALSE, min.len = 1)

  # Setup a table with all the information
  cor.A_SOM_LOI = cor.A_PH_CC = value = value.A_CLAY_MI = NULL
  dt <- data.table(
    A_CLAY_MI = A_CLAY_MI,
    A_SOM_LOI = A_SOM_LOI, 
    A_PH_CC = A_PH_CC,
    value.A_CLAY_MI = NA_real_,
    cor.A_SOM_LOI = NA_real_,
    cor.A_PH_CC = NA_real_,
    value = NA_real_
  )
  df.lookup <- data.frame(
    A_CLAY_MI = c(4, 10, 17, 24, 30, 40, 100),
    value.A_CLAY_MI = c(10, 9, 8, 6.5, 5, 3.5, 1),
    cor.A_SOM_LOI = c(0, 0.06, 0.09, 0.12, 0.25, 0.35, 0.46),
    cor.A_PH_CC = c(0, 0, 0.15, 0.3, 0.7, 1, 1.5)
  )
  
  # Calculate value.A_CLAY_MI
  fun.A_CLAY_MI <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$value.A_CLAY_MI, rule = 2)
  dt[, value.A_CLAY_MI := fun.A_CLAY_MI(A_CLAY_MI)]
    
  # Create organic matter correction function and calculate correction for A_SOM_LOI
  fun.cor.A_SOM_LOI <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$cor.A_SOM_LOI, rule = 2)
  dt[, cor.A_SOM_LOI := fun.cor.A_SOM_LOI(A_CLAY_MI)]
    
  # Create pH correction function and calculate correction for pH
  fun.cor.A_PH_CC <- approxfun(x = df.lookup$A_CLAY_MI, y = df.lookup$cor.A_PH_CC, rule = 2)
  dt[A_PH_CC < 7, cor.A_PH_CC := fun.cor.A_PH_CC(A_CLAY_MI)]
  dt[A_PH_CC >= 7, cor.A_PH_CC := 0]
  
  # Calculate the value
  dt[, value := value.A_CLAY_MI + cor.A_SOM_LOI * A_SOM_LOI - cor.A_PH_CC * pmax(0, 7 - A_PH_CC)]
  
  # Limit the value to 1 - 10
  dt[value > 10, value := 10]
  dt[value < 1, value := 1]
  
  value <- dt[, value]
  
  return(value)
}

#' Calculate the indicator for crumbleability
#' 
#' This function calculates the indicator for crumbleability. The crumbleability is calculated by \code{\link{calc_crumbleability}}
#' 
#' @param D_CR (numeric) The value of crumbleability calculated by \code{\link{calc_crumbleability}}
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
ind_crumbleability <- function(D_CR, B_LU_BRP) {
  
  # Load in the crops dataset
  crop_code = crop_group = crop_crumbleability = lower = upper = NULL
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)

  # Check input
  arg.length = max(length(D_CR), length(B_LU_BRP))
  checkmate::assert_numeric(D_CR, lower = 0, upper = 20, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)

  # Combine information into a table
  dt <- data.table(
    value = NA_real_,
    D_CR = D_CR,
    B_LU_BRP = B_LU_BRP
  )
  setkey(dt, B_LU_BRP)
  dt <- crops.obic[dt]
  
  # combine with crumbleability range
  setkey(dt,crop_crumbleability)
  dt.eval.crumb <- as.data.table(OBIC::eval.crumbleability)
  setkey(dt.eval.crumb,crop_group)
  dt <- dt.eval.crumb[dt]
  
  # calculate a crop specific index
  dt[D_CR >= lower, value := 0.5 + 0.5 * (D_CR - lower)/(upper - lower)]
  dt[D_CR < lower, value := 0.5 * D_CR/lower]
  dt[,value := pmin(1,pmax(0, value))]
  
  # retrieve value
  value <- dt[, value]
  
  # return value
  return(value)
}

#' Coefficient table for evaluating crumbleability
#' 
#' This table contains the coefficients for evaluating the crumbleability. This table is used internally in \code{\link{ind_crumbleability}}
#' 
"eval.crumbleability"