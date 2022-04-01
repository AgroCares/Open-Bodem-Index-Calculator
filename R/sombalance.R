#' Calculate simple organic matter balance
#'
#' This function calculates a simple organic matter balance, as currently used in agricultural practice in the Netherlands.For more details, see www.os-balans.nl
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%)
#' @param A_P_AL (numeric) The P-AL content of the soil (in mg P2O5 per 100g)
#' @param A_P_WA (numeric) The P-water content of the soil (in mg P2O5 per Liter)
#' @param M_COMPOST (numeric) The frequency that compost is applied (every x years)
#' @param M_GREEN (boolean) measure. are catch crops sown after main crop (option: TRUE or FALSE)
#' 
#' @examples 
#' calc_sombalance(B_LU_BRP = 1019,A_SOM_LOI = 4, A_P_AL = 35, A_P_WA = 40, 
#' M_COMPOST = 4, M_GREEN = TRUE)
#' calc_sombalance(1019,4, 35, 40, 4, TRUE)
#' calc_sombalance(c(256,1024,1019),c(4,5,6), c(35,35,35), c(40,42,45), c(4,4,3), c(TRUE,FALSE,TRUE))
#' 
#' @return 
#' The estimated soil organic matter balance in kg EOS per ha per year. A numeric value.
#' 
#' @export
calc_sombalance <- function(B_LU_BRP,A_SOM_LOI, A_P_AL, A_P_WA, M_COMPOST, M_GREEN) {
  
  id = c.diss = crop_code = crop_name = crop_n = cropinput = mdose = compost = catchcrop = NULL
  crop_eos = crop_eos_residue = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_P_AL), length(A_P_WA))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(A_P_AL, lower = 1, upper = 250, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_P_WA, lower = 1, upper = 250, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(M_COMPOST, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(M_GREEN,any.missing = FALSE, len = arg.length)
  
  # make data.table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,    
    A_SOM_LOI = A_SOM_LOI,
    A_P_AL = A_P_AL,
    A_P_WA = A_P_WA,
    M_COMPOST = M_COMPOST,
    M_GREEN = M_GREEN,
    value = NA_real_
  )
  
  # merge with crop table
  dt <- merge(dt, crops.obic[, list(crop_code, crop_n,crop_name,crop_eos, crop_eos_residue)], by.x = "B_LU_BRP", by.y = "crop_code")
  
  # ensure crop name is lower case
  dt[,crop_name := tolower(crop_name)]
  
  # carbon decomposition (kg EOS per year)
  dt[A_SOM_LOI > 23, c.diss := 3145.513]
  dt[A_SOM_LOI <= 23, c.diss := A_SOM_LOI * 0.1 * 0.3 * (-0.37 * log(A_SOM_LOI) + 1.8398) * (10 ^ 6) * 0.0487 * A_SOM_LOI ^ -0.453 * 0.57]
  
  # EOS input (kg / ha) via crop roots and residues
  dt[,cropinput := pmax(0,crop_eos,na.rm=T) + pmax(0,crop_eos_residue,na.rm=T)]
  
  # EOS input (kg / ha) via manure, maximized given allowed p-dose from manure policy
  # assuming slurry from dairy (50 g EOS/kg en 1.5 g P2O5/kg) and pigs (26 g EOS/kg; 3.9 g P/kg)
  
  # manure input in grassland systems, assuming 100% dairy slurry
  slurry_EOS_Pratio <- 50/1.5
  dt[crop_n=='gras' & A_P_AL <= 16,mdose := 120 * slurry_EOS_Pratio]
  dt[crop_n=='gras' & A_P_AL > 16 & A_P_AL <= 27,mdose := 100 * slurry_EOS_Pratio]
  dt[crop_n=='gras' & A_P_AL > 27 & A_P_AL <= 50,mdose := 90 * slurry_EOS_Pratio]
  dt[crop_n=='gras' & A_P_AL > 50,mdose := 80 * slurry_EOS_Pratio]
  
  # manure input in arable systems, assuming 70% dairy slurry and 30% pig slurry, 85% organic
  slurry_EOS_Pratio <- (0.3 * 26 / 3.9 + 0.7 * 50 / 1.5)
  dt[crop_n=='akkerbouw' & A_P_WA <= 25,mdose := 0.85 * 120 * slurry_EOS_Pratio]
  dt[crop_n=='akkerbouw' & A_P_WA > 25 & A_P_WA <= 36,mdose := 0.85 * 75 * slurry_EOS_Pratio]
  dt[crop_n=='akkerbouw' & A_P_WA > 36 & A_P_WA <= 55,mdose := 0.85 * 60 * slurry_EOS_Pratio]
  dt[crop_n=='akkerbouw' & A_P_WA > 55,mdose := 0.85 * 50 * slurry_EOS_Pratio]
  
  # EOS input via compost to arable soils
  dt[crop_n == 'akkerbouw' & M_COMPOST == 0, compost := 0]
  dt[crop_n == 'akkerbouw' & M_COMPOST != 0, compost :=  15 * 218 / M_COMPOST]
  dt[crop_n != 'akkerbouw', compost := 0]
  
  # EOS input via catch crops (and mandatory crops)
  dt[M_GREEN == TRUE, catchcrop := 850]
  dt[M_GREEN != TRUE, catchcrop := 0]
  dt[grepl('mais|aardappel',crop_name), catchcrop := 850]
  
  # calculate simple eos balance (kg EOS / ha / yr)
  dt[,value := cropinput + mdose + compost + catchcrop - c.diss]

  # setorder
  setorder(dt,id)
  
  # return only the value
  value <- dt[,value]
  
  # return value
  return(value)
  
}
