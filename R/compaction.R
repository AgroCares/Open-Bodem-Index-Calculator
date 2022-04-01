#' Calculate indicator for subsoil compaction
#'
#' This function calculates the indicator for the risk for soil compaction of the subsoil.
#' derived from van den Akker et al. (2013) Risico op ondergrondverdichting in het landelijk gebied in kaart, 
#' Alterra-rapport 2409, Alterra, Wageningen University and Research Centre,
#' 
#' @param B_SC_WENR (character) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' 
#' @examples 
#' ind_compaction(B_SC_WENR = 'Zeer groot')
#' ind_compaction(B_SC_WENR = c('Zeer groot','Van nature dicht'))
#' 
#' @return 
#' The evaluated score for the soil function for subsoil compaction. A numeric value between 0 and 1.
#' 
#' @references 
#' Akker et al. (2013) Risico op ondergrondverdichting in het landelijk gebied in kaart, 
#' Alterra-rapport 2409, Alterra, Wageningen University and Research Centre.
#' 
#' @export
ind_compaction <- function(B_SC_WENR) {
  
  # Check inputs
  checkmate::assert_character(B_SC_WENR, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(B_SC_WENR, choices = c("Bebouwing en infrastructuur","Groot","Zeer groot","Matig","Water",
                                                    "Glastuinbouw, niet beoordeeld","Beperkt door veenlagen","Van nature dicht" ,
                                                    "Beperkt", "Zeer beperkt"), empty.ok = FALSE)
  
  # make data.table
  dt <- data.table(
    id = 1:length(B_SC_WENR),
    B_SC_WENR = tolower(B_SC_WENR),
    value = NA_real_
  )
  
  # reclassify non arable or non grassland soils (no risk)
  dt[grepl('bebouwing|water|glastuinbouw',B_SC_WENR),value := 1]
  
  # reclassify arable soils
  dt[grepl('zeer beperkt',B_SC_WENR), value := 1]
  dt[grepl('^beperkt',B_SC_WENR), value := 0.8]
  dt[grepl('matig',B_SC_WENR),value := 0.6]
  dt[grepl('^groot',B_SC_WENR), value := 0.4]
  dt[grepl('zeer groot|nature', B_SC_WENR), value := 0.2]
  
  # Evaluate the risk on soil compaction
  value <- dt[,value]
  
  # return value
  return(value)
  
}
