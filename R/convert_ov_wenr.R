#' Convert possible B_OV_WENR values to standardised values
#' 
#' This function converts numeric values for OV_WENR to values used by other OBIC functions if numeric values are entered.
#' 
#' @param B_OV_WENR (numeric and/or character) Data on soil compaction risk that may have to be converted to string
#' 
#' @import data.table
#' 
#' @export
convert_B_OV_WENR <- function(B_OV_WENR) {
  
  B_OV_WENR_new = NULL
  
  # Create data table
  dt <- data.table(
    B_OV_WENR = as.character(B_OV_WENR), 
    B_OV_WENR_new = NA_character_
  )
  
    checkmate::assert_subset(dt$B_OV_WENR, empty.ok = FALSE, choices = c('1', '2', '3', '4', "5", '10', '11', '401', '901', '902',
                                                                      "Zeer beperkt", 'Beperkt', 'Matig', 'Groot',
                                                                      'Zeer groot', 'Bebouwing en infrastructuur',
                                                                      'Water', 'Glastuinbouw, niet beoordeeld', 'Beperkt door veenlagen',
                                                                      'Van nature dicht'))

    # Convert numbers to appropriate strings
    dt[B_OV_WENR=='1',B_OV_WENR_new := 'Zeer beperkt']
    dt[B_OV_WENR=='2',B_OV_WENR_new := 'Beperkt']
    dt[B_OV_WENR=='3',B_OV_WENR_new := 'Matig']
    dt[B_OV_WENR=='4',B_OV_WENR_new := 'Groot']
    dt[B_OV_WENR=="5",B_OV_WENR_new := 'Zeer groot']
    dt[B_OV_WENR=="901",B_OV_WENR_new := 'Bebouwing en infrastructuur']
    dt[B_OV_WENR=="902",B_OV_WENR_new := 'Water']
    dt[B_OV_WENR=="401",B_OV_WENR_new := 'Glastuinbouw, niet beoordeeld']
    dt[B_OV_WENR=="10",B_OV_WENR_new := 'Beperkt door veenlagen']
    dt[B_OV_WENR=="11",B_OV_WENR_new := 'Van nature dicht']
    
    # fill missing new values with strings from old
    dt[is.na(B_OV_WENR_new), B_OV_WENR_new:= B_OV_WENR]
    
    # set B_OV_WENR to converted values
    B_OV_WENR <- dt[,B_OV_WENR_new]

  
  # return value
  return(B_OV_WENR)
}
