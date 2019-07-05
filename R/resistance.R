#' Evaluate the soil resistance
#'
#' This function evaluates the resistance of the soil
#' 
#' @param om (numeric) The organic matter content of the soil in percentage
#' 
#' @export
eval_resistance <- function(om) {
  
  # Check inputs
  checkmate::assert_numeric(om, lower = 0, upper = 100, any.missing = FALSE)
  
  # Evaluate the soil resistance
  eval.resistance <- OBIC::evaluate_logistic(om, b = 1.2, x0 = 1.7, v = 0.4)
  
  return(eval.resistance)
  
}
