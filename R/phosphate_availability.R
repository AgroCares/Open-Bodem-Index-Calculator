#' Calculate the phosphate availability (PBI)
#' 
#' This function calculates the phosphate availability. This value can be evaluated by \code{\link{eval_phosphate_availability}}
#' 
#' @param p_al (numeric) The P-AL content of the soil
#' @param p_cacl2 (numeric) The P-CaCl2 content of the soil
#' @param crop (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_phosphate_availability <- function(p_al, p_cacl2, crop) {
  
  # Load in the crops dataset
  crop_code = crop_phosphate = id = NULL
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check input
  arg.length <- max(length(p_al), length(p_cacl2), length(crop))
  checkmate::assert_numeric(p_al, lower = 8, upper = 70, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(p_cacl2, lower = 0.3, upper = 5, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(crop, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(crop, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect the data into a table
  dt <- data.table(
    id = 1:arg.length,
    p_al = p_al,
    p_cacl2 = p_cacl2,
    crop = crop,
    value = NA_real_
  )
  setkey(dt, crop)
  dt <- crops.obic[dt]
  setorder(dt, id)
  
  # Calculate the phosphate availability (PBI, unit?)
  dt[crop_phosphate == "gras", value := 2 + 2.5 * log(p_cacl2) + 0.036 * p_al / p_cacl2]
  dt[crop_phosphate == "mais", value := p_cacl2 + 0.05 * (p_al / p_cacl2)]
  
  value <- dt[, value]
  
  return(value)

}

#' Evaluate the phosphate availability
#' 
#' This function evaluates the phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' 
#' @param value.phosphate.availability (numeric) The value of phosphate availability calculated by \code{\link{calc_phosphate_availability}}
#' 
#' @export
eval_phosphate_availability <- function(value.phosphate.availability) {
  
  # Check inputs
  checkmate::assert_numeric(value.phosphate.availability, lower = 0, upper = 7, any.missing = FALSE)
  
  # Evaluate the phosphate availability
  eval.phosphate.availability <- OBIC::evaluate_logistic(value.phosphate.availability, b = 1.3, x0 = 1.3, v = 0.35)
  
  # return output
  return(eval.phosphate.availability)
}
