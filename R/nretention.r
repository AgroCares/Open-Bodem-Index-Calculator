#' Calculate the N leaching
#' 
#' This function calculates the potential N leaching of a soil.
#' 
#' ## To BE FIXED:
# 1) Not all B_GT types occured in the actual datasets are covered in ADI table.
# -->"unknown" need to be handled somewhow. 
# 2) Set a condition when NLV is negative

#' @param B_BT_AK (character) The type of soil
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GT (character) The groundwater table class
#' 
#' @param D_NLV (numeric) The N-leverend vermogen (kg N ha-1 jr-1) calculated by \code{\link{calc_nlv}}
#' 
#' 
#' @import data.table
#' 
#' @export
calc_nleach <- function(B_BT_AK, B_LU_BRP, B_GT, D_NLV){
  
  # Here, insert a line e.g. xx = xx = xx = NULL
  # id = croptype.nleach = cat_nleach = NULL ??
 
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  ## Read ADI table
  # (This need to be changed, so that RData is called from the package)
  load('adi_table.RData')
  
  # Check input
  arg.length <- max(length(B_BT_AK),length(B_LU_BRP), length(B_GT),
                    length(D_NLV))
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GT,any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GT, choices = unique(adi_table$Grondwatertrap))
  checkmate::assert_numeric(D_NLV, lower = -30, upper = 250, len = arg.length)
  # TO ADD:  a line to check if all B_GT categories occured in the dataset are included in the ADI table (column 'Grondwatertrap') 
  
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_BT_AK = B_BT_AK,
    B_LU_BRP = B_LU_BRP, 
    B_GT = B_GT,
    D_NLV = D_NLV,
    value = NA_real_
  )
  
  dt <- merge(dt, crops.obic[, list(crop_code, crop_category)], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
  
  # Categorize 'natuur' and 'akkerbouw' as 'ov.BL'
  dt[, croptype.nleach := crop_category]
  dt[crop_category == "natuur" | crop_category == "akkerbouw" , croptype.nleach := "ov.BL"]
  
  # concatenate soil type ('soiltype.n'), crop type ('croptype.nleach'), and grondwatertrap ('B_GT)
  dt[, cat_nleach := paste(soiltype.n, croptype.nleach, B_GT, sep = "_")]
  adi_table[, cat_nleach := paste(Grondsoort, Grondgebruik, Grondwatertrap, sep = "_")]
  
  # merge ADI values into 'dt', based on soil type x crop type x grondwatertrap
  dt <- merge(dt, subset(adi_table, select = c(cat_nleach, ADI)), by = 'cat_nleach', sort = F)
  # To check: conditional joining in 'ph.R'is written as follows (this is probably more efficient way to join)
  # dt.53 <- dt.ph.delta[dt.53, on=list(table == table, lutum.low <= A_CLAY_MI, lutum.high > A_CLAY_MI, om.low <= A_OS_GV, om.high > A_OS_GV)]
  
  # compute (potential) N leaching D_NUIT (gN/ha/yr)
  dt[, value := D_NLV * (1. - ADI)]
  
  
  # Extract relevant variable and return
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}




#' Calculate the indicator for N retention
#' 
#' This function calculates the indicator for the N retention of the soil by using the N leaching calculated by \code{\link{calc_nleach}}
#' 
#' @param D_NLEACH (numeric) The value of N leaching calculated by \code{\link{calc_nleach}}

#' @export
ind_nretention <- function(D_NLEACH){
  # Check inputs
  checkmate::assert_numeric(D_NLEACH, lower = 0 , upper = 250, any.missing = FALSE)
  
  # Evaluate the N retention
  value <- OBIC::evaluate_logistic(x = D_NLEACH, b = 0.05, x0 = 28, v = 0.3, increasing = FALSE)
  
  return(value)
}
