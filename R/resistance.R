#' Calculate indicator for soil resistance
#'
#' This function calculats the indicator for the resistance of the soil against diseases and is inidcatd by the amount of soil life.
#' 
#' @param A_OS_GV (numeric) The organic matter content of the soil in percentage
#' 
#' @export
ind_resistance <- function(A_OS_GV) {
  
  # Check inputs
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE)
  
  # Evaluate the soil resistance
  value <- OBIC::evaluate_logistic(A_OS_GV, b = 1.2, x0 = 1.7, v = 0.4)
  
  return(value)
  
}
