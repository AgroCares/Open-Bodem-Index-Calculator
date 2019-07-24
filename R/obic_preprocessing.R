#' Preprocess for the OBI
#' 
#' This wrapper function contains the functions to preprocess the data of agricultural fields.
#' 
#' @param dt (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_preprocessing <- function(dt) {
  
  # Check inputs
  checkmate::assert_data_table(dt)
  
  # Calculations
  # TO DO
  
  return(dt)
}