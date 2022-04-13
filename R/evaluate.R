#' Evaluate using the general logistic function
#' 
#' This function evaluates the calculated values from an indicator using a general logistic function
#' 
#' @param x (numeric) The values of a calc function to be converted to an evaluation
#' @param b (numeric) The growth rate
#' @param x0 (numeric) The offset of the x-axis
#' @param v (numeric) Affects the growth rate near the maximum
#' @param increasing (boolean) Should the evaluation increase (\code{TRUE}) with x or decrease (\code{FALSE})?
#' 
#' @references \url{https://en.wikipedia.org/wiki/Generalised_logistic_function}
#' 
#' @examples 
#' evaluate_logistic(x = 5, b = 2, x0 = 3, v = 2.6)
#' evaluate_logistic(x = c(0.1,0.5,1.5,3.5), b = 2, x0 = 3, v = 2.6)
#' 
#' @return 
#' A transformed variable after applying a logistic evaluation function. A numeric value.
#' 
#' @export
evaluate_logistic <- function(x, b, x0, v, increasing = TRUE) {
  
  # Settings
  if (increasing) {
    A <- 0 # Lower asympote
    K <- 1 # Upper asympote
  } else {
    A <- 1 # Lower asympote
    K <- 0 # Upper asympote
  }
  C <- 1
  
  # General logistic function
  y <- A + ((K - A) / (C + exp(-b * (x - x0)))^(1 / v))
  
  return(y)
  
}

#' Evaluate using parabolic function with 
#' 
#' This function evaluates the calculated values from an indicator using a parabolic function. After the optimum is reached the it stays at its plateau.
#' 
#' @param x (numeric) The values of a calc function to be converted to an evaluation
#' @param x.top (numeric) The value at which x reaches the plateau
#' 
#' @examples 
#' evaluate_parabolic(x = 5, x.top = 8)
#' evaluate_parabolic(x = c(0.1,0.5,1.5,3.5), x.top = 6.5)
#' 
#' @return 
#' A transformed variable after applying a parabolic evaluation function. A numeric value.
#' 
#' @export
evaluate_parabolic <- function(x, x.top) {
  
  # Setting
  a <- 1 / x.top^2
  b <- x.top
  
  # Calcute the values
  y <- 1 - a * (x - b) ^2
  
  # Set plateaus
  y <- ifelse(x >= x.top, 1, y)
  y <- ifelse(y < 0, 0, y)
  
  return(y)
  
}


#' Helper function to weight and correct the risk and scores
#' 
#' @param x The risk or score value to be weighted
#' 
#' @examples 
#' cf_ind_importance(x = 0.5)
#' cf_ind_importance(x = c(0.1,0.5,1.5))
#' 
#' @return 
#' A transformed variable after applying a inverse weighing function so that lower values will gain more impact when applied in a weighed.mean function. A numeric value.
#' 
#' @export
cf_ind_importance <- function(x) {
    y <- 1 / (x  + 0.2)
  
  return(y)
}
