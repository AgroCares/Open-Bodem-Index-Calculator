#' Score the indicators for the OBI
#' 
#' This wrapper function contains the functions to weight the evaluations.
#' 
#' @param dt.ind (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_score <- function(dt.ind) {
  
  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  S_C = S_P = S_B = S_T = NULL
  
  # Score the chemical indicators
  dt.ind[, S_C := -999]
  
  # Score the physical inidcators
  dt.ind[, S_P := -999]
  
  # Score the biology
  dt.ind[, S_B := -999]
  
  # Calculate the total score
  dt.ind[, S_T := -999]
  
  return(dt.ind)
}