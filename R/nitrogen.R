#' Calculate the NLV
#' 
#' This function calculates the NLV (nitrogen producing capacity) for the soil
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_N_RT (numeric) The organic nitrogen content of the soil in mg N / kg
#' @param A_CN_FR (numeric) The carbon to nitrogen ratio
#' @param D_OC (numeric) The organic carbon content of the soil in kg C / ha
#' @param D_BDS (numeric) The bulk density of the soil in kg / m3
#' @param D_GA (numeric) The age of the grass if present
#' 
#' @import data.table
#' 
#' @export
calc_nlv <- function(B_LU_BRP, B_SOILTYPE_AGR, A_N_RT, A_CN_FR, D_OC, D_BDS, D_GA) {
  
  a = c.ass = c.diss = id = crop_code = soiltype = soiltype.n = crop_n = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  
  # Check input
  arg.length <- max(length(A_N_RT), length(D_OC), length(B_LU_BRP), length(B_SOILTYPE_AGR), length(D_BDS), length(D_GA))
  checkmate::assert_numeric(A_N_RT, lower = 0, upper = 30000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_OC, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(D_BDS, lower = 0, upper = 1500, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CN_FR, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_GA, lower = 0, upper = 99, len = arg.length)
  
  # Settings
  param.a <- 20 # Age of organic matter
  param.b <- 2^((14.1 - 9)/ 9) # Temperature correction
  param.cn.micro <- 10 # CN ratio of micro organisms
  param.t <- 5 / 12 # 5 months a year
  param.diss.micro <- 2 # Dissimilation : assimilation ratio of micro organisms
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_N_RT = A_N_RT,
    A_CN_FR = A_CN_FR,   
    D_OC = D_OC,
    D_BDS = D_BDS,
    D_GA = D_GA,
    value = NA_real_
  )
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  
  # Calculate NLV for grass
  dt.grass <- dt[crop_n == "gras"]
  dt.grass[soiltype.n == "zand" & D_GA < 4, a := 30.79]
  dt.grass[soiltype.n == "zand" & D_GA >= 4 & D_GA < 7, a := 28.36]
  dt.grass[soiltype.n == "zand" & D_GA >= 7 & D_GA < 10, a := 27.78]
  dt.grass[soiltype.n == "zand" & D_GA >= 10, a := 26.57]
  dt.grass[soiltype.n == "klei" & D_GA < 4, a := 34.25]
  dt.grass[soiltype.n == "klei" & D_GA >= 4 & D_GA < 7, a := 31.54]
  dt.grass[soiltype.n == "klei" & D_GA >= 7 & D_GA < 10, a := 30.90]
  dt.grass[soiltype.n == "klei" & D_GA >= 10, a := 29.56]
  
  dt.grass[soiltype.n == "zand", value := 78 + a * (A_N_RT/1000) ^ 1.0046]
  dt.grass[soiltype.n == "klei", value := 31.7 + a * (A_N_RT/1000) ^ 1.0046]
  dt.grass[soiltype.n == "zand" & value > 200, value := 200]
  dt.grass[soiltype.n == "klei" & value > 250, value := 250]
  dt.grass[soiltype.n == "veen", value := 250]
  
  # Calculate the NLV for arable land
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[, c.diss := D_OC * (1 - exp(4.7 * ((param.a + param.b * param.t)^-0.6 - param.a^-0.6)))]
  dt.arable[, c.ass := c.diss / param.diss.micro]
  dt.arable[, value := ((c.diss + c.ass) / A_CN_FR) - (c.ass / param.cn.micro)]
  dt.arable[value > 250, value := 250]
  dt.arable[value < -30, value := -30]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}

#' Calculate the indicator for NLV
#' 
#' This function calculates the indicator for the the nitrogen content of the soil by using the NLV calculated by \code{\link{calc_nlv}}
#' 
#' @param D_NLV (numeric) The value of NLV  calculated by \code{\link{calc_nlv}}
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' 
#' @export
ind_nitrogen <- function(D_NLV, B_LU_BRP) {
  
  # Load in the datasets for crop types
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  crop_code = crop_n = id = NULL
  
  # Check inputs
  arg.length <- max(length(D_NLV),length(B_LU_BRP))
  checkmate::assert_numeric(D_NLV, lower = -30, upper = 250, any.missing = FALSE)
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # make data.table to save scores
  dt = data.table(
    id = 1:arg.length,
    D_NLV = D_NLV,
    B_LU_BRP = B_LU_BRP,
    value = NA_real_
  )
  
  # add crop names
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # Evaluate N availability for arable land -----
  dt.arable <- dt[crop_n == "akkerbouw"]
  dt.arable[,value := OBIC::evaluate_parabolic(D_NLV, x.top = 100)]
  
  # Evaluate N availability for grassland  ----- 
  dt.grass <- dt[crop_n == "gras"]
  dt.grass[,value := OBIC::evaluate_parabolic(D_NLV, x.top = 140)]
  
  # Combine both tables and extract values
  dt <- rbindlist(list(dt.grass, dt.arable), fill = TRUE)
  setorder(dt, id)
  value <- dt[, value]
  
  # return output
  return(value)
}
