#' Evaluate using the general logistice function
#' 
#' This function evaluates the calculated values from an indicator using a general logistic function
#' 
#' @param x (numeric) The values of a calc function to be converted to an evaluation
#' @param b (numeric) The growth rate
#' @param x0 (numeric) The offset of the x-axis
#' @param v (numeric) Affects the growth rate near the maximum
#' 
#' @references \url{https://en.wikipedia.org/wiki/Generalised_logistic_function}
#' 
#' @export
evaluate_logistic <- function(x, b, x0, v) {
  
  # Settings
  A <- 0 # Lower asympote
  K <- 1 # Upper asympote
  C <- 1
  
  # General logisitc function
  y <- A + ((K - A) / (C + exp(-b * (x - x0)))^(1 / v))
  
  return(y)
  
}
