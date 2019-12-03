#' Evaluate effects of measures 
#' 
#' This function quantifies the effects of 11 soil measures on the OBI score
#' 
#' @param dt.score (data.table) 
#' 
#' @import data.table
#' 
#' @export
obic_evalmeasure <- function(dt.score) {
  
  # Check inputs
  checkmate::assert_data_table(dt.score)
  
  # set variables as NULL
  soiltype = soiltype.n = crop_waterstress = crop_maatregel = crop_code = ID = NULL
  OBICvariable = maatregel_nr = Dremp_S = app = app1 = app2 = sector = grondsoort = indicator = NULL
  Ef_M_v = tresshold = score.m = weight = grp = score.mp = Prio_M = m.effect = FS = TH = NULL
  
  # make local copy of dt.score
  dt.score <- copy(dt.score)
  
  # add local databases for joining properties -----
  
    # wegingsfactoren voor integratie per bodemfunctie type
    w <- as.data.table(OBIC::weight.obic)
    
    # measures database
    maatregel.obic <- as.data.table(OBIC::maatregel.obic)
    
    # crop categories
    crops.obic <- as.data.table(OBIC::crops.obic)
    
    # soil categories
    soils.obic <- as.data.table(OBIC::soils.obic)
    
  
  return(dt.recom)
}


#' Recommend measurements for better soil managment
#' 
#' This function gives recommendations better soil managament based on the OBI score
#' 
#' @param dt.recom (data.table) 
#' @param extensive (boolean) whether the output table includes evaluation scores of each  measures (TRUE) or only names of top 3 measures
#' 
#' @import data.table
#' 
#' @export
obic_recommendations <- function(dt.recom, extensive = FALSE) {
  
 
  
  return(dt.recom)
  
}