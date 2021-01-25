#' Convert possible B_OV_WENR values to standardised values
#' 
#' This function convertes numeric values for OV_WENR to values used by other OBIC functions.
#' 
#' @param B_OV_WENR () Data on soil compaction risk that may have to be converted to string
#' 
#' @import data.table
#' 
#' @export

convert_WENR <- function(B_OV_WENR) {
  # Create data table
  dt <- data.table(B_OV_WENR = B_OV_WENR)
  if(is.numeric(B_OV_WENR)){
    checkmate::assert_subset(B_OV_WENR,
                             empty.ok = FALSE,
                             choices = 
                               c(1, 2, 3, 4, 5, 10, 11, 401, 901, 902)
                             )
    # Convert numbers to appropriate strings
    dt[B_OV_WENR==1,B_OV_WENR := 'Zeer beperkt']
    dt[B_OV_WENR==2,B_OV_WENR := 'Beperkt']
    dt[B_OV_WENR==3,B_OV_WENR := 'Matig']
    dt[B_OV_WENR==4,B_OV_WENR := 'Groot']
    dt[B_OV_WENR==5,B_OV_WENR := 'Zeer groot']
    dt[B_OV_WENR==901,B_OV_WENR := 'Bebouwing en infrastructuur']
    dt[B_OV_WENR==902,B_OV_WENR := 'Water']
    dt[B_OV_WENR==401,B_OV_WENR := 'Glastuinbouw, niet beoordeeld']
    dt[B_OV_WENR==10,B_OV_WENR := 'Beperkt door veenlagen']
    dt[B_OV_WENR==11,B_OV_WENR := 'Van nature dicht']
  }
  
  B_OV_WENR <- dt[,B_OV_WENR]
  
  # return value
  return(B_OV_WENR)
}
