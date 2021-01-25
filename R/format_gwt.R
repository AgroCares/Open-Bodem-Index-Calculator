#' Convert possible B_GT values to standardised values
#' 
#' This function formats ground water table information so it can be understood by other OBIC functions
#' 
#' @param B_GT (sting) Ground water table classes
#' 
#' @import data.table
#' 
#' @export

format_gwt <- function(B_GT) {
  dt <- data.table(B_GT = B_GT)
  
  # Check if B_GT values are appropriate
  checkmate::assert_subset(B_GT,
                           empty.ok = FALSE,
                           choices = 
                             c(gsub("Gt", "",unique(OBIC::nleach_table$B_GT)),
                               "-",
                               unique(OBIC::nleach_table$B_GT)
                               )
  )
  
  # Add "Gt" to B_GT if string don't already start with Gt. This method does not allow for mixed gwt data with and without Gt prefix
  # if(all(grepl("^Gt|^-$", dt$B_GT)) == FALSE ) {
    # dt[B_GT != '-',B_GT := paste0('Gt',B_GT)]
    #dt[B_GT == '-',B_GT := 'unknown'] # this may not be required
  # }
  
  # if value starts with I or V, prefix Gt to it. This way messy gwt data both with and without Gt prefix can be used as input.
  dt[, B_GT := gsub("^I", "GtI", B_GT)]
  dt[, B_GT := gsub("^V", "GtV", B_GT)]
  
  # Return B_GT
  B_GT <- dt[,B_GT]
  
  return(B_GT)
}