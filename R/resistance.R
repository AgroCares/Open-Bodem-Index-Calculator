#' Calculate indicator for soil resistance
#'
#' This function calculates the indicator for the resistance of the soil against diseases and is indicated by the amount of soil life.
#' 
#' @param A_SOM_LOI (numeric) The organic matter content of the soil in percentage
#' 
#' @export
ind_resistance <- function(A_SOM_LOI) {
  
  # Check inputs
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Evaluate the soil resistance
  value <- OBIC::evaluate_logistic(A_SOM_LOI, b = 1.2, x0 = 1.7, v = 0.4)
  
  return(value)
  
}
