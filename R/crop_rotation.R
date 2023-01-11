#' Calculates the fraction in the crop rotation
#' 
#' This function calculates the fraction present in the crop rotation
#' 
#' @param ID (numeric) The ID of the field
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param crop (character) The crop to check for. For relevant crop categories, see details.
#' 
#' @import data.table
#'
#' @details 
#' This function calculates the fraction present in the crop rotation for specific crop categories. 
#' These categories include "starch", "potato", "sugarbeet", "grass", "mais", "alfalfa","catchcrop","cereal","clover",'nature', rapeseed',"other","rustgewas",and "rustgewasdiep".
#' 
#' @examples 
#' calc_rotation_fraction(ID = rep(1,4), B_LU_BRP = c(265,1910,1935,1033),crop = 'potato')
#' calc_rotation_fraction(ID = rep(1,4), B_LU_BRP = c(265,1910,1935,1033),crop = 'grass')
#'
#' @return 
#' The fraction of specific crop types within the crop rotation sequence. A numeric value.
#'  
#' @export
calc_rotation_fraction <- function(ID, B_LU_BRP, crop) {
  
  crop_code = crop_rotation = this_id = this_frc = sel = NULL
  
  # Load data
  crops.obic <- as.data.table(OBIC::crops.obic)
  
  # Check inputs
  arg.length = max(length(ID), length(B_LU_BRP))
  checkmate::assert_atomic_vector(ID, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_choice(crop, choices = c("starch", "potato", "sugarbeet", "grass", "mais", 
                                             "alfalfa","catchcrop","cereal","clover",'nature',
                                             'rapeseed',"other","rustgewas","rustgewasdiep"))
  
  # Collect the data in a table
  dt <- data.table(
    this_id = 1:arg.length,
    ID = ID,
    B_LU_BRP = B_LU_BRP
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_rotation)], by.x = "B_LU_BRP", by.y = "crop_code")
  setorder(dt, this_id)
  
  # do some clustering for group 'rustgewas' or 'deep rustgewas' as well ass reclassify for pH
  if(crop=='rustgewas'){
    dt[crop_rotation %in% c('catchgrop','clover','grass','alfalfa','cereal','rapeseed'), crop_rotation := 'rustgewas']
  }
  if(crop=='rustgewasdiep'){
    dt[crop_rotation %in% c('clover','alfalfa','rapeseed'), crop_rotation := 'rustgewasdiep']
  }
  if(!crop %in% c('rustgewasdiep','rustgewas')){
    dt[crop_rotation %in% c("alfalfa","catchcrop","cereal","clover",'nature','rapeseed',
                            "rustgewas","rustgewasdiep"), crop_rotation := 'other']
  }
  
  # Calculate the fraction for this crop
  dt[, sel := ifelse(crop_rotation == crop, 1, 0)]
  dt[, this_frc := sum(sel) / .N, by = ID]
  
  value <- dt[, this_frc]
  return(value)
}
