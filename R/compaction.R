#' Calculate indicator for subsoil compaction
#'
#' This function calculats the indicator for the risk for soil compaction of the subsoil.
#' derived from van den Akker et al. (2013) Risico op ondergrondverdichting in het landelijk gebied in kaart, 
#' Alterra-rapport 2409, Alterra, Wageningen University and Research Centre,
#' 
#' @param B_OV_WENR (numeric) The risk for subsoil compaction as derived from risk assessment study of Van den Akker (2006)
#' 
#' @export
ind_compaction <- function(B_OV_WENR) {
  
  id = NULL
  
  # Check inputs
  checkmate::assert_character(B_OV_WENR, any.missing = FALSE, min.len = 1)
  
  # make data.table
  dt <- data.table(
    id = 1:length(B_OV_WENR),
    B_OV_WENR = tolower(B_OV_WENR),
    value = NA_real_
  )
  
  # reclassify non arable or non grassland soils (no risk)
  dt[grepl('bebouwing|water|glastuinbouw',B_OV_WENR),value := 1]
  
  # reclassify arable soils
  dt[grepl('zeer beperkt',B_OV_WENR), value := 1]
  dt[grepl('^beperkt',B_OV_WENR), value := 0.8]
  dt[grepl('matig',B_OV_WENR),value := 0.6]
  dt[grepl('^groot',B_OV_WENR), value := 0.4]
  dt[grepl('zeer groot|nature', B_OV_WENR), value := 0.2]
  
  # Evaluate the risk on soil compaction
  value <- dt[,value]
  
  # return value
  return(value)
  
}
