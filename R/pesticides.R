#' Calculate risk of pesticide leaching
#' 
#' This function calculates the volume of water (in mm / ha) that infiltrates to the groundwater given the crop rotation plan.
#' 
#' 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_gw_recharge}}
#' 
#' @export
calc_pesticide_leaching <- function(B_SOILTYPE_AGR,A_SOM_LOI,A_SAND_MI,A_SILT_MI,A_CLAY_MI,D_PSP) {
  
  # check inputs
  
  
  # import data in table
  dt <- data.table(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   D_PSP = D_PSP
                   )
  
  
  ## Minimal situation
  # Calcualte bulk density
  dt[,BD_MIN := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI)]
  
  # Calculate volume fraction of water
  dt[,vfw_min := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI = 1,type = 'water holding capacity')]
  
  # Calculate water flux
  dt[,B_WATER_FLUX_MIN := D_PSP / 365 / vfw_min / 100]
  
  # Calculate pesticide leaching fraction
  dt[,pest_leach_min := exp((-0.34/60 * (vfw_min + BD_MIN * 1/100 * 10)/B_WATER_FLUX_MIN))]
  
  
  ## Current situation
  # Calcualte bulk density
  dt[,BD := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI)]
  
  # Calculate volume fraction of water
  dt[,vfw := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'water holding capacity')]
  
  # Calculate water flux
  dt[,B_WATER_FLUX := D_PSP / 365 / vfw / 100]
  
  # Calculate pesticide leaching fraction
  dt[,pest_leach := exp((-0.34/60 * (vfw + BD * A_SOM_LOI/100 * 10)/B_WATER_FLUX))]
  
  dt[,D_PEST := log(pest_leach)/log(pest_leach_min)]
  
  return(dt[,D_PEST])
  
}



