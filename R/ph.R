#' Calculate the difference between pH and optimum
#' 
#' This functions calculates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#' 
#' @param ph.cacl2 (numeric) The pH-CaCl2 of the soil
#' @param soiltype (character) The type of soil
#' @param lutum (numeric) The percentage lutum present in the soil
#' @param om (numeric) The organic matter content of soil in percentage
#' @param cp.starch (numeric) The fraction of starch potatoes in the crop plan
#' @param cp.potato (numeric) The fraction of potatoes (excluding starch potatoes) in the crop plan
#' @param cp.sugarbeet (numeric) The fraction of sugar beets in the crop plan
#' @param cp.grass (numeric) The fracgtion of grass in the crop plan
#' @param cp.mais (numeric) The fraction of mais in the crop plan
#' @param cp.other (numeric) The fraction of other crops in the crop plan
#' 
#' @references \href{https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/pH-en-bekalking/Advisering-pH-en-bekalking.htm}{Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3}
#' 
#' @import data.table
#' 
#' @export
calc_ph_delta <- function(ph.cacl2, soiltype, om, lutum, cp.starch, cp.potato, cp.sugarbeet, cp.grass, cp.mais, cp.other) {
  
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  setkey(soils.obic, soiltype)
  dt.ph.delta <- as.data.table(OBIC::tbl.ph.delta)
  
  # Check inputs
  arg.length <- max(length(ph.cacl2), length(soiltype), length(om), length(lutum), length(cp.starch), length(cp.potato), length(cp.sugarbeet), length(cp.grass), length(cp.mais), length(cp.other))
  checkmate::assert_character(soiltype, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(soiltype, choices = unique(soils.obic$soiltype))
  checkmate::assert_numeric(om, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(lutum, lower = 0, upper = 100, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.starch, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.potato, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.sugarbeet, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.grass, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.mais, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.other, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  cp.total <- cp.starch + cp.potato + cp.sugarbeet + cp.grass + cp.mais + cp.other
  if (any(cp.total != 1)) {
    stop(paste0("The sum of the fraction of cp is not 1, but ", min(cp.total)))
  }
  
  # Collect information in table
  soiltype.ph = lutum.low = lutum.high = om.low = om.high = potato.low = potato.high = sugarbeet.low = sugarbeet.high = ph.optimum = id = NULL
  dt <- data.table(
    id = 1:arg.length,
    ph.cacl2 = ph.cacl2,
    soiltype = soiltype,
    om = om,
    lutum = lutum,
    cp.starch = cp.starch,
    cp.potato = cp.potato,
    cp.sugarbeet = cp.sugarbeet,
    cp.grass = cp.grass,
    cp.mais = cp.mais,
    cp.other = cp.other,
    table = NA_character_
  )
  
  # Join soil type used for this function
  dt <- merge(dt, soils.obic, by = "soiltype")
  
  # Define which table to be used
  dt[soiltype.ph == 1, table := "5.1"]
  dt[soiltype.ph == 2, table := "5.3"]
  dt[cp.starch > 0.1, table := "5.2"]
  dt[cp.grass + cp.mais >= 0.5, table := "5.3"]
  
  dt[, cp.potato := cp.starch + cp.potato]
  
  # Join conditionally the tables with optimum pH to data
  dt.53 <- dt[table == "5.3"]
  dt.53 <- dt.ph.delta[dt.53, on=list(table == table, lutum.low <= lutum, lutum.high > lutum, om.low <= om, om.high > om)]
  dt.512 <- dt[table %in% c("5.1", "5.2")]
  dt.512 <- dt.ph.delta[dt.512, on=list(table == table, potato.low <= cp.potato, potato.high > cp.potato, sugarbeet.low <= cp.sugarbeet, sugarbeet.high > cp.sugarbeet, om.low <= om, om.high > om)]
  dt <- rbindlist(list(dt.53, dt.512), fill = TRUE)
  
  # Calculate the difference between the measured pH and the optimum pH
  dt[, ph.delta := ph.optimum - ph.cacl2]
  dt[ph.delta < 0, ph.delta := 0]
  
  # Extract the ph.delta
  setorder(dt, id)
  ph.delta <- dt[, ph.delta]
  
  return(ph.delta)
  
}

#' Evaluate the difference between de optimum pH and ph of the soil
#' 
#' This function evaluates the pH of the soil by the difference with the optimum pH. This is calculated in \code{\link{calc_ph_delta}}.
#' 
#' @param value.ph.delta (numeric) The pH difference with the optimal pH.
#' 
#' @export
eval_ph <- function(value.ph.delta) {
  
  # Check inputs
  checkmate::assert_numeric(value.ph.delta, lower = 0, upper = 5, any.missing = FALSE)
  
  # Evaluate the pH
  eval.ph <- OBIC::evaluate_logistic(x = value.ph.delta, b = 9, x0 = 0.3, v = 0.4, increasing = TRUE)
  
  return(eval.ph)
  
}

#' Table with optimal pH for different crop plans
#' 
#' This table contains the optimal pH for different crop plans and soil types
#' 
#' @format A data.frame with 84 rows and 10 columns:
#' \describe{
#'   \item{table}{The original table from Hanboek Bodem en Bemesting}
#'   \item{lutum.low}{Lower value for lutum}
#'   \item{lutum.high}{Upper value for lutum}
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
