#' Determine the root depth of the soil for this crop
#' 
#' This function determines the depth of the soil
#' 
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
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

