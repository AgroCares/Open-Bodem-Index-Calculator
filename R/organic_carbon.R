#' Calculate amount of organic carbon
#' 
#' This function calculates the amount of organic carbon in the soil
#' 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil
#' @param D_BDS (numeric) The bulk density of the soil
#' @param D_RD (numeric) The root depth of the crop
#' 
#' @examples 
#' calc_organic_carbon(A_SOM_LOI = 4.3, D_BDS = 1100, D_RD = 0.2)
#' calc_organic_carbon(A_SOM_LOI = c(1,4.3), D_BDS = c(1100,1300), D_RD = c(0.2,0.6))
#' 
#' @return 
#' The total amount of Carbon in the soil (kg C / ha). A numeric value.
#' 
#' @export
calc_organic_carbon <- function(A_SOM_LOI, D_BDS, D_RD) {
  
  # Check inputs
  arg.length <- max(length(A_SOM_LOI), length(D_BDS), length(D_RD))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_BDS, lower = 100, upper = 1900, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_RD, lower = 0, upper = 2, any.missing = FALSE, len = arg.length)
  
  # Calculate the value
  ha <- 100 * 100
  A_OS_FR <- A_SOM_LOI / 100
  value <- 0.58 * A_OS_FR * ha *  D_RD * D_BDS
  
  return(value)
}
