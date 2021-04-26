#' Convert possible B_GT values to standardized values
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
  checkmate::assert_subset(B_GWL_CLASS, empty.ok = FALSE, choices = bgwlclass)
  
  # if value starts with I or V, add prefix Gt to it.
  B_GWL_CLASS <- gsub("^I", "GtI", B_GWL_CLASS)
  B_GWL_CLASS <- gsub("^V", "GtV", B_GWL_CLASS)
  
  # Return B_GT
  return(B_GWL_CLASS)
}

#' Convert possible B_SC_WENR values to standardized values
#' 
#' This function converts numeric values for B_SC_WENR to values used by other OBIC functions if numeric values are entered.
#' 
#' @param B_SC_WENR (numeric and/or character) Data on soil compaction risk that may have to be converted to string
#' 
#' @import data.table
#' 
#' @export
format_soilcompaction <- function(B_SC_WENR) {
  
  # allowed inputs
  bsc.num  <- c('1', '2', '3', '4', "5", '10', '11', '401', '901', '902')
  bsc.char <- c("Zeer beperkt", "Beperkt", "Matig", "Groot", "Zeer groot",
                "Beperkt door veenlagen", "Van nature dicht", "Glastuinbouw, niet beoordeeld",
                "Bebouwing en infrastructuur", "Water")
               
  # convert to character
  B_SC_WENR <- as.character(B_SC_WENR)
  
  # check inputs
  checkmate::assert_subset(B_SC_WENR, empty.ok = FALSE, choices = c(bsc.num,bsc.char))
  
  # which of the input values are numeric
  var.sel <- match(B_SC_WENR,bsc.num,nomatch = 0)
  
  # replace numeric values with strings
  B_SC_WENR[B_SC_WENR %in% bsc.num] <- bsc.char[var.sel]
  
  # return value
  return(B_SC_WENR)
}
