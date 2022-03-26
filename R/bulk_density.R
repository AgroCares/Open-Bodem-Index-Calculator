#' Calculate the bulk density
#' 
#' This function calculates the bulk density of the soil based on texture and organic matter
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' 
#' @import data.table
#' 
#' @examples
#' calc_bulk_density(B_SOILTYPE_AGR = 'zeeklei', A_SOM_LOI = 6.5, A_CLAY_MI = 28)
#' calc_bulk_density(B_SOILTYPE_AGR = 'dekzand', A_SOM_LOI = 3.5)
#' calc_bulk_density(B_SOILTYPE_AGR = c('dekzand','rivierklei'), A_SOM_LOI = c(3.5,8.5))
#' 
#' @return 
#' The bulk density of an arable soil (kg / m3). A numeric value.
#' 
#' @export
calc_bulk_density <- function(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI = NULL) {
  
  soiltype = dens.sand = dens.clay = cf = NULL
  
  # Load in the data
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_SOM_LOI), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect data into a table
  dt <- data.table(
    A_SOM_LOI = A_SOM_LOI,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_CLAY_MI = A_CLAY_MI,
    value = NA_real_
  )
  
  # estimate soil density depending on soil type (CBAV, 2019)
  dt[grepl('zand|dal|loess', B_SOILTYPE_AGR), value := (1 / (0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  dt[grepl('klei|veen', B_SOILTYPE_AGR), value :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206) * 1000]
  
  # make fluent transition between two soil types possible (similar to pedotransferfunctions used) 
  if('A_CLAY_MI' %in% colnames(dt)){
    
    # calculate soil texture dependent density
    dt[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541)) * 1000]
    dt[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206) * 1000]
    
    # fraction clay correction
    dt[, cf := pmin(1, A_CLAY_MI/25)]
    
    # clay dependent density
    dt[, value := cf * dens.clay + (1-cf) * dens.sand]
  }
  
  # return value
  value <- dt[, value]
  return(value)
  
}