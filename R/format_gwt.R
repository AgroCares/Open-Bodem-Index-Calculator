#' Convert possible B_GT values to standardised values
#' 
#' This function formats ground water table information so it can be understood by other OBIC functions
#' 
#' @param B_GWL_CLASS (character) Ground water table classes
#' 
#' @import data.table
#' 
#' @export
format_gwt <- function(B_GWL_CLASS) {
  
  # options for B_GWL_CLASS
  bgwlclass <- c('I','II','IIb','III','IIIb','IV','V','Vb','VI','VI','VII','VIII',
                 'GtI','GtII','GtIIb','GtIII','GtIIIb','GtIV','GtV','GtVb','GtVI',
                 'GtVI','GtVII','GtVIII','-')
  
  # Check if B_GT values are appropriate
  checkmate::assert_subset(B_GT, empty.ok = FALSE, choices = bgwlclass)
  
  # if value starts with I or V, add prefix Gt to it.
  B_GWL_CLASS <- gsub("^I", "GtI", B_GWL_CLASS)
  B_GWL_CLASS <- gsub("^V", "GtI", B_GWL_CLASS)
 
  # Return B_GT
  return(B_GWL_CLASS)
}