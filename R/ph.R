#' Calculate the difference between pH and optimum
#' 
#' This functions calculates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param B_SOILTYPE_AGR (character) The agricultural type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_CLAY_MI (numeric) The percentage A_CLAY_MI present in the soil
#' @param A_PH_CC (numeric) The pH-CaCl2 of the soil
#' @param D_CP_STARCH (numeric) The fraction of starch potatoes in the crop plan
#' @param D_CP_POTATO (numeric) The fraction of potatoes (excluding starch potatoes) in the crop plan
#' @param D_CP_SUGARBEET (numeric) The fraction of sugar beets in the crop plan
#' @param D_CP_GRASS (numeric) The fraction of grass in the crop plan
#' @param D_CP_MAIS (numeric) The fraction of mais in the crop plan
#' @param D_CP_OTHER (numeric) The fraction of other crops in the crop plan
#' 
#' @references Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3
#' 
#' @import data.table
#' 
#' @examples 
#' calc_ph_delta(B_LU_BRP = 265, B_SOILTYPE_AGR = "rivierklei", A_SOM_LOI = 5,
#' A_CLAY_MI = 20,A_PH_CC = 6, D_CP_STARCH = 0,D_CP_POTATO = 0.3,D_CP_SUGARBEET = 0.2,
#' D_CP_GRASS = 0,D_CP_MAIS = 0.2,D_CP_OTHER = 0.3)
#' calc_ph_delta(265, "rivierklei", 5,20,6, 0,0.3,0.2,0,0.2,0.3) 
#' 
#' @return 
#' The difference between the actual and desired optimum soil pH. A numeric value.
#' 
#' @export
calc_ph_delta <- function(B_LU_BRP, B_SOILTYPE_AGR, A_SOM_LOI, A_CLAY_MI, A_PH_CC,
                          D_CP_STARCH, D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER) {
  
  lutum.low = lutum.high = om.low = om.high = potato.low = potato.high = sugarbeet.low = sugarbeet.high = ph.optimum = NULL
  id = soiltype.ph = crop_code = crop_name = NULL
  
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  crops.obic <- as.data.table(OBIC::crops.obic)
  dt.ph.delta <- as.data.table(OBIC::tbl.ph.delta)
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(B_SOILTYPE_AGR), length(A_SOM_LOI), length(A_CLAY_MI), length(D_CP_STARCH), length(D_CP_POTATO), 
                    length(D_CP_SUGARBEET), length(D_CP_GRASS), length(D_CP_MAIS), length(D_CP_OTHER), length(B_LU_BRP))
  checkmate::assert_numeric(A_PH_CC, lower = 2, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_STARCH, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_POTATO, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_SUGARBEET, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_GRASS, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_MAIS, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_OTHER, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  cp.total <- D_CP_STARCH + D_CP_POTATO + D_CP_SUGARBEET + D_CP_GRASS + D_CP_MAIS + D_CP_OTHER
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code))
  if (any(cp.total != 1)) {
     #stop(paste0("The sum of the fraction of cp is not 1, but ", min(cp.total)))
  }
  
  # Collect information in table
  dt <- data.table(
    id = 1:arg.length,
    B_LU_BRP = B_LU_BRP,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_CLAY_MI = A_CLAY_MI,
    A_PH_CC = A_PH_CC,
    D_CP_STARCH = D_CP_STARCH,
    D_CP_POTATO = D_CP_POTATO,
    D_CP_SUGARBEET = D_CP_SUGARBEET,
    D_CP_GRASS = D_CP_GRASS,
    D_CP_MAIS = D_CP_MAIS,
    D_CP_OTHER = D_CP_OTHER,
    table = NA_character_
  )
  
  # Join soil type used for this function and croptype 
  dt <- merge(dt, soils.obic, by.x = "B_SOILTYPE_AGR", by.y = "soiltype")
  dt <- merge(dt, crops.obic[, list(crop_code, crop_name)], by.x = "B_LU_BRP", by.y = "crop_code")

  
  # Define which table to be used
  dt[soiltype.ph == 1, table := "5.1"]
  dt[soiltype.ph == 2, table := "5.3"] 
  dt[D_CP_STARCH > 0.1, table := "5.2"]
  dt[D_CP_GRASS + D_CP_MAIS >= 0.5, table := "mh"] # grasland / melkveehouderij
  dt[D_CP_GRASS + D_CP_MAIS >= 0.5 & grepl('klaver',crop_name), table := "mh_kl"] # grasland met klaver # this is now only for crop_code 800 (Rolklaver) and 2653 (Graszaad (inclusief klaverzaad))
  
  dt[, D_CP_POTATO := D_CP_STARCH + D_CP_POTATO]
  
  # Join conditionally the tables with optimum pH to data
  dt.53 <- dt[table == "5.3"]
  dt.53 <- dt.ph.delta[dt.53, on=list(table == table, lutum.low <= A_CLAY_MI, lutum.high > A_CLAY_MI, om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]
  dt.512 <- dt[table %in% c("5.1", "5.2")]
  dt.512 <- dt.ph.delta[dt.512, on=list(table == table, potato.low <= D_CP_POTATO, potato.high > D_CP_POTATO, sugarbeet.low <= D_CP_SUGARBEET, sugarbeet.high > D_CP_SUGARBEET,om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]
  dt.mh <- dt[table == "mh"]
  dt.mh <- dt.ph.delta[dt.mh, on=list(table == table, om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]
  dt.mh_kl <- dt[table == "mh_kl"]
  dt.mh_kl <- dt.ph.delta[dt.mh_kl, on=list(table == table, om.low <= A_SOM_LOI, om.high > A_SOM_LOI)]  
  
  dt <- rbindlist(list(dt.53, dt.512, dt.mh, dt.mh_kl), fill = TRUE)
  
  # Calculate the difference between the measured pH and the optimum pH
  dt[, ph.delta := ph.optimum - A_PH_CC]
  dt[ph.delta < 0, ph.delta := 0]
  
  # Extract the ph.delta
  setorder(dt, id)
  ph.delta <- dt[, ph.delta]
  
  return(ph.delta)
  
}

#' Calculate the indicator for pH
#' 
#' This function calculates the indicator for the pH of the soil by the difference with the optimum pH. This is calculated in \code{\link{calc_ph_delta}}.
#' 
#' @param D_PH_DELTA (numeric) The pH difference with the optimal pH.
#' 
#' @examples 
#' ind_ph(D_PH_DELTA = 0.8)
#' ind_ph(D_PH_DELTA = c(0.2,0.6,0.8,1.5))
#'  
#' @return 
#' The evaluated score for the soil function to buffer pH within optimum range for crop growth. A numeric value between 0 and 1.
#' 
#' @export
ind_ph <- function(D_PH_DELTA) {
  
  # Check inputs
  checkmate::assert_numeric(D_PH_DELTA, lower = 0, upper = 5, any.missing = FALSE)
  
  # Evaluate the pH
  value <- 1 - OBIC::evaluate_logistic(x = D_PH_DELTA, b = 9, x0 = 0.3, v = 0.4, increasing = TRUE)
  
  return(value)
  
}

