#' Calculate the index for the microbial biological activity
#' 
#' This function assesses the microbial biological activity (of microbes and fungi) via the Potentially Mineralizable N pool, also called PMN (or SoilLife by Eurofins in the past).
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_N_PMN (numeric) The potentially mineralizable N pool (mg N / kg soil) 
#' 
#' @import data.table
#' 
#' @export
calc_pmn <- function(B_LU_BRP, B_SOILTYPE_AGR,A_N_PMN) {
  
  # set variables to NULL
  id = cf = crop_code = crop_category = soiltype = soiltype.n = NULL
  
  # Load in the datasets for soil and crop types
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check inputs
  arg.length <- max(length(A_N_PMN), length(B_LU_BRP), length(B_SOILTYPE_AGR))
  checkmate::assert_numeric(A_N_PMN, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_N_PMN = A_N_PMN,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    cf = 1,
    value = NA_real_
  )
  
  # add crop names and soiltypes
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # temporal normalization of measured data on sector and soil averaged BOBI measurements
  dt[crop_category %in% c('mais','grasland') & soiltype.n == 'klei', cf := 137 / 120]
  dt[crop_category %in% c('mais','grasland') & soiltype.n == 'veen', cf := 1]
  dt[crop_category %in% c('mais','grasland') & soiltype.n == 'zand', cf := 84 / 89]
  dt[crop_category %in% 'akkerbouw' & soiltype.n == 'zand', cf := 37 / 45]
  dt[crop_category %in% 'akkerbouw' & soiltype.n == 'klei', cf := 22 / 42]
  
  # Calculate PMN index for soil fertility
  dt[, value := A_N_PMN * cf]

  # PMN values above 500 are very unlikely, so maximize the PMN-index
  dt[value > 500, value := 500]
  
  # select output value for PMN
  setorder(dt, id)
  value <- dt[,value]
  
  # return value
  return(value)
}

#' Calculate the indicator for microbial biological activity
#' 
#' This function calculates the indicator that assess the microbial biological activity of the soil by using the PMN calculated by \code{\link{calc_pmn}}
#' 
#' @param D_PMN (numeric) The value of PMN  calculated by \code{\link{calc_pmn}}
#' 
#' @export
ind_pmn <- function(D_PMN) {
  
  # Check inputs
  checkmate::assert_numeric(D_PMN, lower = 0, upper = 501, any.missing = FALSE)
  
  # Evaluate the PMN index
  value <- evaluate_logistic(D_PMN, b = 0.2, x0 = 20, v = 1.2)
    
  # return output
  return(value)
}
