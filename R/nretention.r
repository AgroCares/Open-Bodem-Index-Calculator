#' Calculate the N leaching
#' 
#' This function calculates the potential N leaching of a soil.
#' 
#' @param B_BT_AK (character) The type of soil
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GT (character) The groundwater table class
#' 
#' @param D_NLV (numeric) The N-leverend vermogen (kg N ha-1 jr-1) calculated by \code{\link{calc_nlv}}
#' @param wtype (character) whether it computes N leaching to groundwater ("gw") or to surface water ("ow")
#' 
#' 
#' @import data.table
#' 
#' @export
calc_nleach <- function(B_BT_AK, B_LU_BRP, B_GT, D_NLV, wtype){
  
  soiltype = crop_code = crop_category = soiltype.n = croptype.nleach = cat_nleach = GT_Nleach = NULL
  nleach_gw_table = nleach_ow_table = bodem = gewas = gt = filled = id = NULL
 
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Read table of N leaching fraction to groundwater
  #if(wtype == 'gw'){nleach_gw_table <- as.data.table(OBIC::nleach_gw_table)}
  if(wtype == 'gw'){load("data/nleach_gw_table.RData")} #temp
  
  # Read table of N leaching fraction to surfacewater
  #if(wtype == 'ow'){nleach_ow_table <- as.data.table(OBIC::nleach_ow_table)}
  if(wtype == 'ow'){load("data/nleach_ow_table.RData")} #temp
  
  # Check input
  arg.length <- max(length(B_BT_AK),length(B_LU_BRP), length(B_GT),
                    length(D_NLV))
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GT,any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_NLV, lower = -30, upper = 250, len = arg.length) 
  checkmate::assert_character(wtype,any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(wtype, choices = c("gw", "ow"), empty.ok = FALSE)

  
  
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
  
  # Re-categorize crop types
  dt[, croptype.nleach := crop_category]
  dt[crop_category == "natuur" | crop_category == "akkerbouw" , croptype.nleach := "akkerbouw"]
  dt[crop_category == "grasland" , croptype.nleach := "gras"]
  
  # Rename grondwatertrap categories 
  dtcon <- data.table(B_GT = c("GtI", "GtII", "GtIII", "GtIV", "GtV",  "GtVI", "GtVII", "GtVIII", "unknown"),
                      GT_Nleach = c( "GT1", "GT2", "GT3", "GT4", "GT5", "GT6", "GT7", "GT7", "unknown"))
  dt <- merge(dt, dtcon, by = "B_GT")
  
  
  # concatenate soil type ('soiltype.n'), crop type ('croptype.nleach'), and grondwatertrap ('GT_Nleach)
  dt[, cat_nleach := tolower(paste(soiltype.n, croptype.nleach, GT_Nleach, sep = "_"))]
  if(wtype == 'gw'){nleach_gw_table[, cat_nleach := tolower(paste(bodem, gewas, gt, sep = "_"))]}
  if(wtype == 'ow'){nleach_ow_table[, cat_nleach := tolower(paste(bodem, gewas, gt, sep = "_"))]}
  
  # merge fraction of N leaching into 'dt', based on soil type x crop type x grondwatertrap
  if(wtype == 'gw'){dt <- merge(dt, nleach_gw_table[, list(cat_nleach, filled)], by = 'cat_nleach', sort = F, all.x = T)}
  if(wtype == 'ow'){dt <- merge(dt, nleach_ow_table[, list(cat_nleach, filled)], by = 'cat_nleach', sort = F, all.x = T)}
  
  # compute (potential) N leaching to groundwater D_NUIT_GW (mgNO3/L/) or D_NUIT_OW (kgN/ha/year)
  dt[, value := D_NLV * filled]
  # when Groundwatertrap is unknown, set N leaching as 0 <-- to be checked if this is okay,
  dt[B_GT == 'unknown', value := 0.]
  # When NLV is negative (= net immobilization), no leaching is assumed
  dt[D_NLV < 0, value := 0.]
  
  # Extract relevant variable and return
  setorder(dt, id)
  value <- dt[, value]
  
  return(value)
}




#' Calculate the indicator for N retention for groundwater
#' 
#' This function calculates the indicator for the N retention of the soil by using the N leaching to groundwater calculated by \code{\link{calc_nleach}}
#' 
#' @param D_NUIT_GW (numeric) The value of N leaching calculated by \code{\link{calc_nleach}}
#'
#' @export
ind_nretention_gw <- function(D_NUIT_GW){
  
  # Check inputs
  checkmate::assert_numeric(D_NUIT_GW, lower = 0 , upper = 250, any.missing = FALSE)
  
  # Evaluate the N retention for groundwater
  value <- OBIC::evaluate_logistic(x = D_NUIT_GW, b = 0.1, x0 = 20, v = 0.4, increasing = FALSE) # this makes ca. 0.1 when D_NUIT_GW = 50 

  return(value)
}

#' Calculate the indicator for N retention for surface water
#' 
#' This function calculates the indicator for the N retention of the soil by using the N run-off to surface water calculated by \code{\link{calc_nleach}}
#' 
#' @param D_NIT_OW (numeric) The value of N run-off calculated by \code{\link{calc_nleach}}
#'
#' @export
ind_nretention_ow <- function(D_NUIT_OW){
  
  # Check inputs
  checkmate::assert_numeric(D_NUIT_OW, lower = 0 , upper = 250, any.missing = FALSE)
  
  # Evaluate the N retention for surfacewater
  value <- OBIC::evaluate_logistic(x = D_NUIT_OW, b = 0.15, x0 = 5, v = 0.3, increasing = FALSE) # this reaches almost 0 when D_NUIT_GW = 40 (emperical maximum)
  
  return(value)
}

#' Table with fractions of excess N which leaches out to groundwater
#' 
#' This table contains the fractions of N overshot which leaches out to groundwater, per soil type, crop type, and groundwater table
#' 
#' @format A data.frame with 99 rows and 10 columns:
#' \describe{
#'   \item{gt}{grondwatertrap}
#'   \item{gewas}{crop type}
#'   \item{bodem}{soil type}
#'   \item{nloss}{Original values of N leaching fraction to groundwater (mgNO3/L per kgN overschot/ha/yr)}
#'   \item{ghg}{Lower value for groundwater table}
#'   \item{glg}{Upper value for groundwater table}
#'   \item{nloss2}{Original values + manually assigned values (for GT1) of N leaching fraction to groundwater}
#'   \item{pred}{Predicted value of N leaching fraction to groundwater}
#'   \item{filled}{Original values + predicted values (for NA cells) of N leaching fraction to groundwater}
#'   \item{orifill}{whether the value is the original ("model"), filled ("gevuld"), or manually assigned ("voorgeschreven"))}
#' }
#' 
#' 
"nleach_gw_table"

#' Table with fractions of excess N which runs off to surface water
#' 
#' This table contains the fractions of N overshot which runs off to groundwater, per soil type, crop type, and groundwater table
#' 
#' @format A data.frame with 99 rows and 10 columns:
#' \describe{
#'   \item{gt}{grondwatertrap}
#'   \item{gewas}{crop type}
#'   \item{bodem}{soil type}
#'   \item{nloss}{Original values of N run-off fraction to surface water (kg N drain/ha/year per kg N overschot/ha/yaer)}
#'   \item{ghg}{Lower value for groundwater table}
#'   \item{glg}{Upper value for groundwater table}
#'   \item{nloss2}{Original values + manually assigned values (for GT1) of N leaching fraction to groundwater}
#'   \item{pred}{Predicted value of N leaching fraction to groundwater}
#'   \item{filled}{Original values + predicted values (for NA cells) of N leaching fraction to groundwater}
#'   \item{orifill}{whether the value is the original ("model"), filled ("gevuld"), or manually assigned ("voorgeschreven"))}
#' }
#' 
#' 
"nleach_ow_table"
