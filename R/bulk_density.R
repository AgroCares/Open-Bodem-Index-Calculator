#' Calculate the bulk density
#' 
#' This function calculates the bulk density of the soil based on texture and organic matter
#' 
#' @param A_OS_GV (numeric) The percentage organic matter in the soil
#' @param B_BT_AK (character) The agricultural type of soil
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' 
#' @import data.table
#' 
#' @export
calc_bulk_density <- function(A_OS_GV, B_BT_AK, A_CLAY_MI = NULL) {
  
  soiltype = dens.sand = dens.clay = cf = NULL
  
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
    A_CLAY_MI = A_CLAY_MI,
    value = NA_real_
  )
  
  # estimate soil density depending on soil type (CBAV, 2019)
  dt[grepl('zand|dal|loess', B_BT_AK), value := (1 / (0.02525 * A_OS_GV + 0.6541)) * 1000]
  dt[grepl('klei|veen', B_BT_AK), value :=  (0.00000067*A_OS_GV^4 - 0.00007792*A_OS_GV^3 + 0.00314712*A_OS_GV^2 - 0.06039523*A_OS_GV + 1.33932206) * 1000]
  
  # make fluent transition between two soil types possible (similar to pedotransferfunctions used) 
  if('A_CLAY_MI' %in% colnames(dt)){
    
    # calculate soil texture dependent density
    dt[, dens.sand := (1 / (0.02525 * A_OS_GV + 0.6541)) * 1000]
    dt[, dens.clay :=  (0.00000067*A_OS_GV^4 - 0.00007792*A_OS_GV^3 + 0.00314712*A_OS_GV^2 - 0.06039523*A_OS_GV + 1.33932206) * 1000]
    
    # fraction clay correction
    dt[, cf := pmin(1, A_CLAY_MI/25)]
    
    # clay dependent density
    dt[, value := cf * dens.clay + (1-cf) * dens.sand]
  }
  
  # return value
  value <- dt[, value]
  return(value)
  
}