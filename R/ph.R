#' Calculate the difference between pH and optimum
#' 
#' This functions calculates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#' 
#' @param A_PH_CC (numeric) The pH-CaCl2 of the soil
#' @param B_BT_AK (character) The type of soil
#' @param A_CLAY_MI (numeric) The percentage A_CLAY_MI present in the soil
#' @param A_OS_GV (numeric) The organic matter content of soil in percentage
#' @param D_CP_STARCH (numeric) The fraction of starch potatoes in the crop plan
#' @param D_CP_POTATO (numeric) The fraction of potatoes (excluding starch potatoes) in the crop plan
#' @param D_CP_SUGARBEET (numeric) The fraction of sugar beets in the crop plan
#' @param D_CP_GRASS (numeric) The fracgtion of grass in the crop plan
#' @param D_CP_MAIS (numeric) The fraction of mais in the crop plan
#' @param D_CP_OTHER (numeric) The fraction of other crops in the crop plan
#' 
#' @references \href{https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/pH-en-bekalking/Advisering-pH-en-bekalking.htm}{Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3}
#' 
#' @import data.table
#' 
#' @export
calc_ph_delta <- function(A_PH_CC, B_BT_AK, A_CLAY_MI, A_OS_GV, D_CP_STARCH, D_CP_POTATO, D_CP_SUGARBEET, D_CP_GRASS, D_CP_MAIS, D_CP_OTHER) {
  
  lutum.low = lutum.high = om.low = om.high = potato.low = potato.high = sugarbeet.low = sugarbeet.high = ph.optimum = id = soiltype.ph = NULL
  
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  dt.ph.delta <- as.data.table(OBIC::tbl.ph.delta)
  
  # Check inputs
  arg.length <- max(length(A_PH_CC), length(B_BT_AK), length(A_OS_GV), length(A_CLAY_MI), length(D_CP_STARCH), length(D_CP_POTATO), length(D_CP_SUGARBEET), length(D_CP_GRASS), length(D_CP_MAIS), length(D_CP_OTHER))
  checkmate::assert_character(B_BT_AK, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_STARCH, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_POTATO, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_SUGARBEET, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_GRASS, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_MAIS, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_CP_OTHER, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  cp.total <- D_CP_STARCH + D_CP_POTATO + D_CP_SUGARBEET + D_CP_GRASS + D_CP_MAIS + D_CP_OTHER
  if (any(cp.total != 1)) {
    stop(paste0("The sum of the fraction of cp is not 1, but ", min(cp.total)))
  }
  
  # Collect information in table
  dt <- data.table(
    id = 1:arg.length,
    A_PH_CC = A_PH_CC,
    B_BT_AK = B_BT_AK,
    A_OS_GV = A_OS_GV,
    A_CLAY_MI = A_CLAY_MI,
    D_CP_STARCH = D_CP_STARCH,
    D_CP_POTATO = D_CP_POTATO,
    D_CP_SUGARBEET = D_CP_SUGARBEET,
    D_CP_GRASS = D_CP_GRASS,
    D_CP_MAIS = D_CP_MAIS,
    D_CP_OTHER = D_CP_OTHER,
    table = NA_character_
  )
  
  # Join soil type used for this function
  dt <- merge(dt, soils.obic, by.x = "B_BT_AK", by.y = "soiltype.n")
  
  # Define which table to be used
  dt[soiltype.ph == 1, table := "5.1"]
  dt[soiltype.ph == 2, table := "5.3"]
  dt[D_CP_STARCH > 0.1, table := "5.2"]
  dt[D_CP_GRASS + D_CP_MAIS >= 0.5, table := "5.3"]
  
  dt[, D_CP_POTATO := D_CP_STARCH + D_CP_POTATO]
  
  # Join conditionally the tables with optimum pH to data
  dt.53 <- dt[table == "5.3"]
  dt.53 <- dt.ph.delta[dt.53, on=list(table == table, lutum.low <= A_CLAY_MI, lutum.high > A_CLAY_MI, om.low <= A_OS_GV, om.high > A_OS_GV)]
  dt.512 <- dt[table %in% c("5.1", "5.2")]
  dt.512 <- dt.ph.delta[dt.512, on=list(table == table, potato.low <= D_CP_POTATO, potato.high > D_CP_POTATO, sugarbeet.low <= D_CP_SUGARBEET, sugarbeet.high > D_CP_SUGARBEET,om.low <= A_OS_GV, om.high > A_OS_GV)]
  dt <- rbindlist(list(dt.53, dt.512), fill = TRUE)
  
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
#' @export
ind_ph <- function(D_PH_DELTA) {
  
  # Check inputs
  checkmate::assert_numeric(D_PH_DELTA, lower = 0, upper = 5, any.missing = FALSE)
  
  # Evaluate the pH
  value <- OBIC::evaluate_logistic(x = D_PH_DELTA, b = 9, x0 = 0.3, v = 0.4, increasing = TRUE)
  
  return(value)
  
}

#' Table with optimal pH for different crop plans
#' 
#' This table contains the optimal pH for different crop plans and soil types
#' 
#' @format A data.frame with 84 rows and 10 columns:
#' \describe{
#'   \item{table}{The original table from Hanboek Bodem en Bemesting}
#'   \item{lutum.low}{Lower value for A_CLAY_MI}
#'   \item{lutum.high}{Upper value for A_CLAY_MI}
#'   \item{om.low}{Lower value for organic matter}
#'   \item{om.high}{Upper value for organic matter}
#'   \item{potato.low}{Lower value for fraction potatoes in crop plan}
#'   \item{potato.high}{Upper value for fraction potatoes in crop plan}
#'   \item{sugarbeet.low}{Lower value for fraction potatoes in crop plan}
#'   \item{sugarbeet.high}{Upper value for fraction potatoes in crop plan}
#'   \item{ph.optimum}{The optimal pH for this range}   
#' }
#' 
#' #' @references \href{https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/pH-en-bekalking/Advisering-pH-en-bekalking.htm}{Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3}
#' 
"tbl.ph.delta"
