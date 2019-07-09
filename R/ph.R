#' Calculate the difference between pH and optimum
#' 
#' This functions calculates the difference between the measured pH and the optimal pH according to the Bemestingsadvies
#' 
#' @param ph.cacl2 (numeric) The pH-CaCl2 of the soil
#' @param cp.starch (numeric) The fraction of starch potatoes in the crop plan
#' @param cp.potato (numeric) The fraction of potatoes (excluding starch potatoes) in the crop plan
#' @param cp.sugarbeet (numeric) The fraction of sugar beets in the crop plan
#' @param cp.mais (numeric) The fraction of mais in the crop plan
#' @param cp.other (numeric) The fraction of other crops in the crop plan
#' 
#' @references \href{https://www.handboekbodemenbemesting.nl/nl/handboekbodemenbemesting/Handeling/pH-en-bekalking/Advisering-pH-en-bekalking.htm}{Handboek Bodem en Bemesting tabel 5.1, 5.2 en 5.3}
#' 
#' @import data.table
#' 
#' @export
calc_ph_delta <- function(ph.cacl2, cp.starch, cp.potato, cp.sugarbeet, cp.mais, cp.other) {
  
  # Check inputs
  arg.length <- max(length(ph.cacl2), length(cp.starch), length(cp.potato), length(cp.sugarbeet), length(cp.mais), length(cp.other))
  checkmate::assert_numeric(ph.cacl2, lower = 0, upper = 14, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.starch, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.potato, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.sugarbeet, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.mais, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(cp.other, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  cp.total <- cp.starch + cp.potato + cp.sugarbeet + cp.mais + cp.other
  if (cp.total != 1) {
    stop(paste0("The sum of the fraction of cp is not 1, but ", cp.total))
  }
  
  # Use tables for starch potatoes if present in crop plan
  if (cp.starch > 0.1) {
    pr.starch <- TRUE
  } else {
    pr.starch <- FALSE
  }
  

}