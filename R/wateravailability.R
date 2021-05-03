#' Calculate the Water Stress Index
#' 
#' This function calculates the Water Stress Index (estimating the yield depression as a function of water deficiency or surplus)
#' 
#' @param B_HELP_WENR (character) The soil type abbreviation, derived from 1:50.000 soil map
#' @param B_LU_BRP (numeric) The crop code (gewascode) from the BRP
#' @param B_GWL_CLASS (character) The groundwater table class
#' @param WSI (character) The type of Water Stress Index is required. Options: droughtstress, wetnessstress and the (combined) waterstress
#' 
#' @references STOWA (2005) Uitbreiding en Actualisering van de HELP-tabellen ten behoeve van het Waternood instrumentarium
#' 
#' @import data.table
#' 
#' @export
calc_waterstressindex <- function(B_HELP_WENR, B_LU_BRP, B_GWL_CLASS, WSI = 'waterstress') {
  
  id = crop_code = soiltype = soiltype.n = crop_n = NULL
  cropname = soilunit = crop_waterstress = droughtstress = waterstress = wetnessstress = NULL
  
  # Load in the datasets
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  waterstress.obic <- as.data.table(OBIC::waterstress.obic)
  setkey(waterstress.obic, cropname, soilunit)
  
  # update B_GWL_CLASS to more generic ones
  B_GWL_CLASS.wi <- gsub("b$","", B_GWL_CLASS)
  
  # Check input
  arg.length <- max(length(B_HELP_WENR), length(B_LU_BRP), length(B_GWL_CLASS))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_character(B_GWL_CLASS,any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = c('-',unique(waterstress.obic$gt)), empty.ok = FALSE)
  checkmate::assert_character(B_HELP_WENR, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_HELP_WENR, choices = c('unknown',unique(waterstress.obic$soilunit)), empty.ok = FALSE)
  checkmate::assert_character(WSI, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(WSI, choices = c('droughtstress','wetnessstress','waterstress'), empty.ok = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_HELP_WENR = B_HELP_WENR,
    B_LU_BRP = B_LU_BRP,
    B_GWL_CLASS = B_GWL_CLASS.wi
  )
  
  # merge with crop and waterstress tables
  dt <- merge(dt, crops.obic[, list(crop_code, crop_waterstress )], by.x = "B_LU_BRP", by.y = "crop_code")
  dt <- merge(dt, waterstress.obic,
              by.x = c('B_HELP_WENR','crop_waterstress','B_GWL_CLASS'), 
              by.y = c('soilunit','cropname','gt'),
              all.x = TRUE)
  
  # water stress risks included
  cols <- c('droughtstress','wetnessstress','waterstress')
  
  # no WSI calculated for 'nature' and 'catchcrops', as well for situation where HELP code is unknown
  dt[is.na(droughtstress),(cols) := 0]
  
  # add checks : average estimated risk indicators when multiple situation occur
  if(!nrow(dt)==arg.length){dt <- dt[,lapply(.SD,mean,na.rm=TRUE),by=list(id), .SDcols=cols]}
  
  # return default Water Stress Index
  setorder(dt, id)
  value <- dt[, waterstress]
  
  # change output when a particul WSI is required
  if(WSI == 'droughtstress') {value <- dt[, droughtstress]}
  if(WSI == 'wetnessstress') {value <- dt[, wetnessstress]}
  
  # return value
  return(value)
}

#' Calculate the Water Stress Index
#' 
#' This function calculates the risk for yield depression due to drought, an excess of water or a combination of both. The WSI is calculated by \code{\link{calc_waterstressindex}}
#' 
#' @param D_WSI (numeric) The value of WSI calculated by \code{\link{calc_waterstressindex}}
#' 
#' @export
ind_waterstressindex <- function(D_WSI) {
  
  # Check inputs
  checkmate::assert_numeric(D_WSI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Evaluate the WSI
  value <- evaluate_logistic(D_WSI / 100,20,0.18,0.78,increasing = F)
  
  # return output
  return(value)
}


