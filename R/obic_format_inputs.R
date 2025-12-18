#' Convert possible B_SC_WENR values to standardized values
#' 
#' This function converts numeric values for B_SC_WENR to values used by other OBIC functions if numeric values are entered.
#' 
#' @param B_SC_WENR (numeric and/or character) Data on soil compaction risk that may have to be converted to string
#' 
#' @import data.table
#' 
#' @examples 
#' format_soilcompaction(c('10', '11'))
#' format_soilcompaction(c('2', '3',"Matig", "Groot"))
#' 
#' @return 
#' A standardized B_SC_WENR value as required for the OBIC functions. A character string.
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

#' Convert possible B_AER_CBS values to standardized values
#' 
#' This function formats information of Agricultural Economic Region so it can be understood by other OBIC functions
#' 
#' @param B_AER_CBS (character) The agricultural economic region in the Netherlands (CBS, 2016)
#' 
#' @import data.table
#' 
#' @examples 
#' format_aer(c("LG13","LG12"))
#' format_aer(c("LG13","LG12",'Rivierengebied'))
#' 
#' @return 
#' A standardized B_AER_CBS value as required for the OBIC functions. A character string.
#' 
#' @export
format_aer <- function(B_AER_CBS) {
  
  # convert UTF-8 encoded strings to latin1 if required
  if('UTF-8' %in% Encoding(B_AER_CBS)) {
    B_AER_CBS <- iconv(B_AER_CBS, from = '', to = 'latin1')
  }
  
  # options for B_AER_CBS
  aer.text <- c('Zuid-Limburg','Zuidelijk Veehouderijgebied','Zuidwest-Brabant',
                'Zuidwestelijk Akkerbouwgebied','Rivierengebied','Hollands/Utrechts Weidegebied',
                'Waterland en Droogmakerijen','Westelijk Holland','IJsselmeerpolders',
                'Centraal Veehouderijgebied','Oostelijk Veehouderijgebied','Noordelijk Weidegebied',
                'Veenkoloni\xEBn en Oldambt', "Veenkolonien en Oldambt",
                'Bouwhoek en Hogeland')
  
  # options for B_AER_CBS
  aer.code <- c("LG14","LG13","LG12","LG11","LG10","LG09","LG08","LG07","LG06","LG05","LG04","LG03","LG02","LG01")
  
  # all input options
  aer.all <- c(aer.text,aer.code)
  
  # Check if AER_CBS values are appropriate
  checkmate::assert_subset(B_AER_CBS, empty.ok = FALSE, choices = aer.all)
  
  # which of the input values are database codes
  var.sel <- match(B_AER_CBS,aer.code,nomatch = 0)
  
  # remove and overwrite Veenkoloni\xEBn en Oldambt
  aer.text <- aer.text[-13]
  B_AER_CBS[B_AER_CBS %in% 'Veenkoloni\xEBn en Oldambt'] <- "Veenkolonien en Oldambt" 
  
  # replace numeric values with strings
  B_AER_CBS[B_AER_CBS %in% aer.code] <- aer.text[var.sel]
  
  # Return B_AER_CBS
  return(B_AER_CBS)
}
