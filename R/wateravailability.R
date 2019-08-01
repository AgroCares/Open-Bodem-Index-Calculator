#' Calculate the Water Stress Index
#' 
#' This function calculates the Water Stress Index (estimating the yield depression as a function of water deficiency or surplus)
#' source is the HELP-framework (STOWA, 2005)
#' 
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
calc_waterstressindex <- function(B_HELP_WENR, B_LU_BRP) {
  
  id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  waterstress.obic <- as.data.table(OBIC::waterstress.obic)
  setkey(waterstress.obic, soilunit, crop_type)
  
  # Check input
  arg.length <- max(length(B_HELP_WENR), length(B_LU_BRP))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_HELP_WENR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_HELP_WENR, choices = unique(waterstress.obic$soiltype), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_HELP_WENR = B_HELP_WENR,
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  # merge with crop and waterstress tables
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, waterstress.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # return water related 
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}

#' Calculate the indicator for NLV
#' 
#' This function calculates the indicator for the the nitrogen content of the soil by using the NLV calculated by \code{\link{calc_nlv}}
#' 
#' @param D_NLV (numeric) The value of NLV  calculated by \code{\link{calc_nlv}}
#' 
#' @export
ind_waterstress <- function(D_NLV) {
  
  # Check inputs
  checkmate::assert_numeric(D_NLV, lower = -30, upper = 250, any.missing = FALSE)
  
  # Evaluate the nitrogen
  value <- OBIC::evaluate_parabolic(D_NLV, x.top = 120)
  
  # return output
  return(value)
}


