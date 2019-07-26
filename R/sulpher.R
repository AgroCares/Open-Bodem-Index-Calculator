#' Calculate the SLV
#' 
#' This function calculates a S-balance given the SLV (Sulpher supplying capacity) of a soil
#' 
#' @param A_S_TOT (numeric) The total Sulpher content of the soil in mg S / kg
#' @param D_OC (numeric) The organic carbon content of the soil in kg C / ha
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_BT_AK (character) The type of soil
#' @param D_BDS (numeric) The bulk density of the soil in kg / m3
#' 
#' @import data.table
#' 
#' @export
calc_slv <- function(A_S_TOT, D_OC, B_LU_BRP, B_BT_AK, D_BDS) {
  
  a = c.ass = c.diss = id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets
  # sven, check crop_code tabel, moet gras-mais-bouwland zijn
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_S_TOT), length(D_OC), length(B_LU_BRP), length(B_BT_AK), length(D_BDS))
  # add checks Sven
  
  # Settings
  param.a <- 20 # Age of organic matter
  param.b <- 2^((14.1 - 9)/ 9) # Temperature correction
  param.cs.micro <- 100 # CS ratio of micro organisms
  param.t <- 5 / 12 # 5 months a year
  param.diss.micro <- 2 # Dissimilation : assimilation ratio of micro organisms
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_S_TOT = A_S_TOT,
    D_OC = D_OC,
    B_LU_BRP = B_LU_BRP,
    B_BT_AK = B_BT_AK,
    D_BDS = D_BDS,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Calculate SLV for grass (sven: check units, deze formule: Stot = g/kg en dichtheid in g/cm3)
  dt.grass <- dt[crop_n == "gras"]
  dt.grass[, slv := 17.8 * A_S_TOT * 1000 * D_BDS]
  
  # Calculate SLV for maize for 0-30 cm depth (sven: check units)
  dt.maize <- dt[crop_n == "mais"]
  dt.maize[, slv := 41.2 * A_S_TOT * 1000 * D_BDS * 3]
  
  # Calculate the SLV for arable land
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[, c.diss := D_OC * (1 - exp(4.7 * ((param.a + param.b * param.t)^-0.6 - param.a^-0.6)))]
  dt.arable[, c.ass := c.diss / param.diss.micro]
  dt.arable[, slv := ((c.diss + c.ass) / A_CN_RAT) - (c.ass / param.cn.micro)]
  dt.arable[slv > 100, slv := 100]
  
  # add S-balans componenten
  dt.arable[,sup := 50]
  dt.arable[,smest := 5]
  dt.arable[,sdep := 10]
  dt.arable[,swater := 25]
  dt.arable[,value := sup - (smest + slv + sdep + swater)]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.maize,dt.arable), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return value (different index for grass & maize versus arable land)
  return(value)
}

#' Calculate the indicator for SLV
#' 
#' This function calculates the indicator for the the S-index by using the SLV calculated by \code{\link{calc_slv}}
#' 
#' @param D_NLV (numeric) The value of SLV  calculated by \code{\link{calc_slv}}
#' 
#' @export
ind_sulpher <- function(D_SLV) {
  
  # Check inputs
  checkmate::assert_numeric(D_SLV, lower = -30, upper = 250, any.missing = FALSE)
  
  # Evaluate the S-balance-index
  value <- OBIC::evaluate_parabolic(D_SLV, x.top = 120)
  
  # return output
  return(value)
}
