#' Calculate risk of pesticide leaching
#' 
#' This function calculates the risk of pesticide leaching from a soil. The risk is calculated by comparing the current leached fraction with a worst case scenario
#'  
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_psp}}
#' @param M_PESTICIDES_DST (boolean) measure. Use of DST for pesticides (option: yes or no)
#' @param M_MECHWEEDS (boolean) measure. Use of mechanical weed protection (option: yes or no)
#' 
#' @export
calc_pesticide_leaching <- function(B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_SAND_MI, A_SILT_MI, D_PSP, M_PESTICIDES_DST,M_MECHWEEDS) {
  
  # add visual bindings
  soils.obic = BD_MIN = vfw_min = B_WATER_FLUX_MIN = pest_leach_min = BD = NULL
  vfw = B_WATER_FLUX = pest_leach = D_PESTICIDE = I_PESTICIDE = SOM_MIN = NULL
  
  # check inputs
  arg.length <- max(length(B_SOILTYPE_AGR),length(A_SOM_LOI),length(A_CLAY_MI),length(A_SAND_MI),length(A_SILT_MI),length(D_PSP),
                    length(M_PESTICIDES_DST), length(M_MECHWEEDS))
  
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(OBIC::soils.obic$soiltype))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SAND_MI, lower = 0.1, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_SILT_MI, lower = 0.1, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_PSP, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_PESTICIDES_DST, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_MECHWEEDS, any.missing = FALSE, len = arg.length)
  
  # import data in table
  dt <- data.table(B_SOILTYPE_AGR = B_SOILTYPE_AGR,
                   A_SOM_LOI = A_SOM_LOI,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   D_PSP = D_PSP,
                   M_PESTICIDES_DST = M_PESTICIDES_DST,
                   M_MECHWEEDS = M_MECHWEEDS
                   )
  
  ## Minimal situation
  # Determine minimal OM level based on soiltype
  dt[B_SOILTYPE_AGR == 'loess', SOM_MIN := 1.2]
  dt[B_SOILTYPE_AGR == 'dekzand', SOM_MIN := 1.2]
  dt[B_SOILTYPE_AGR == 'zeeklei', SOM_MIN := 1.0]
  dt[B_SOILTYPE_AGR == 'rivierklei', SOM_MIN := 1.3]
  dt[B_SOILTYPE_AGR == 'moerige_klei', SOM_MIN := 3.4]
  dt[B_SOILTYPE_AGR == 'veen', SOM_MIN := 1.9]
  dt[B_SOILTYPE_AGR == 'maasklei', SOM_MIN := 1.3]
  dt[B_SOILTYPE_AGR == 'duinzand', SOM_MIN := 0.7]
  dt[B_SOILTYPE_AGR == 'dalgrond', SOM_MIN := 2.1]
  
  # Calcualte bulk density
  dt[,BD_MIN := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI = rep(SOM_MIN,arg.length),A_CLAY_MI)/1000]
  
  # Calculate volume fraction of water
  dt[,vfw_min := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI = SOM_MIN,type = 'water holding capacity')]
  
  # Calculate water flux
  dt[,B_WATER_FLUX_MIN := D_PSP / 365 / vfw_min / 100]
  
  # Calculate pesticide leaching fraction
  dt[,pest_leach_min := exp((-0.34/60 * (vfw_min + BD_MIN * 1/100 * 10)/B_WATER_FLUX_MIN))]
  
  
  ## Current situation
  # Calculate bulk density
  dt[,BD := calc_bulk_density(B_SOILTYPE_AGR,A_SOM_LOI,A_CLAY_MI)/1000]
  
  # Calculate volume fraction of water
  dt[,vfw := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'water holding capacity')]
  
  # Calculate water flux
  dt[,B_WATER_FLUX := D_PSP / 365 / vfw / 100]
  
  # Calculate pesticide leaching fraction
  dt[,pest_leach := exp((-0.34/60 * (vfw + BD * A_SOM_LOI/100 * 10)/B_WATER_FLUX))]
  
  # Correct for reduction in pesticide use
  dt[(M_PESTICIDES_DST == TRUE) | (M_MECHWEEDS == TRUE), pest_leach := pest_leach * 0.75]
  
  # Calculate pesticide leaching risk
  dt[,D_PESTICIDE := pest_leach/pest_leach_min]
  
  return(dt[,D_PESTICIDE])
  
}


#' Calculate an indicator score for pesticide leaching
#' 
#' This function calculates the indicator value for pesticide leaching from a soil
#' 
#' 
#' @param D_PESTICIDE The fraction of pesticide leached compared to the worst case scenario
#' 
#' @export
ind_pesticide_leaching <- function(D_PESTICIDE) {

  
  # Calculate indicator score
  I_PESTICIDE <- evaluate_logistic(D_PESTICIDE, 28, 0.80, 1.5, increasing = FALSE)
  
  
 return(I_PESTICIDE)
  
}
