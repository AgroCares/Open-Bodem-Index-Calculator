#' Calculate the K availability
#' 
#' This function calculates the K availability of a soil.
#' 
#' @param k.cacl2 (numeric) The K-CaCl2 content of the soil in mg K / kg
#' @param cec (numeric) The CEC content of the soil
#' @param crop (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_k_availability <- function(k.cacl2, cec, crop) {
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  arg.length <- max(length(k.cacl2), length(cec))
  checkmate::assert_numeric(k.cacl2, lower = 0, upper = 225, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cec, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)
  
  # Collect the data
  dt <- data.table(
    id = 1:arg.length,
    k.cacl2 = k.cacl2,
    cec = cec,
    crop = crop
  )
  setkey(dt, crop)
  dt <- crops.obic[dt]
  setorder(dt, id)
  
  # Calculate the K availability

  k.available <- 4 - exp(-0.08551 * k.cacl2 + 0.5264 * log(k.cacl2) - 0.001607 * cec + 0.1275 * log(cec) + 0.010836 * k.cacl2 * log(cec))
  
}