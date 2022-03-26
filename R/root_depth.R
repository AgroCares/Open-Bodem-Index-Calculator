#' Determine the root depth of the soil for this crop
#' 
#' This function determines the depth of the soil
#' 
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @details 
#' This is a helper function to estimate the rooting depth of crops, as being used for calculations for soil nutrient supplies.
#' Be aware, this is not the real rooting depth; it rather represents the sampling depth of the soils collected for routine soil analsyis.
#' 
#' @examples 
#' calc_root_depth(B_LU_BRP = 256)
#' calc_root_depth(B_LU_BRP = c(256,265,1019,992))
#' 
#' @return 
#' The root depth of a crop corresponding to the sampling depth analyzed by agricultural labs. A numeric value.
#' 
#' @export
calc_root_depth <- function(B_LU_BRP) {
  
  crop_category = crop_code = id = NULL
  
  # Load data
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect the data into a table
  dt <- data.table(
    id = 1:length(B_LU_BRP),
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  setorder(dt, id)
  
  # Determine depth
  dt[crop_category %in% c("akkerbouw", "mais", "natuur"), value := 0.25]
  dt[crop_category %in% c("grasland"), value := 0.1]
  
  value <- dt[, value]
  return(value)  
}

