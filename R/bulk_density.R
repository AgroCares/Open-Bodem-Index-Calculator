#' Calculate the bulk density
#' 
#' This function calculates the bulk density of the soil based on texture and organic matter
#' 
#' @param A_OS_GV (numeric) The percentage organic matter in the soil
#' @param B_BT_AK (character) The agricultural type of soil
#' 
#' @import data.table
#' 
#' @export
calc_bulk_density <- function(A_OS_GV, B_BT_AK) {
  
  soiltype = NULL
  
  # Load in the data
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_OS_GV), length(B_BT_AK))
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_BT_AK, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect data into a table
  dt <- data.table(
    A_OS_GV = A_OS_GV,
    B_BT_AK = B_BT_AK,
    value = NA_real_
  )
  
  dt[grepl('zand|dal|loess', B_BT_AK), value := (1 / (0.02525 * A_OS_GV + 0.6541)) * 1000]
  dt[grepl('klei|veen', B_BT_AK), value :=  (0.00000067*A_OS_GV^4 - 0.00007792*A_OS_GV^3 + 0.00314712*A_OS_GV^2 - 0.06039523*A_OS_GV + 1.33932206) * 1000]
  
  value <- dt[, value]
  return(value)
  
}