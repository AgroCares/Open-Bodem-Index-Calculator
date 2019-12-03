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

  # adapt local databases before joining -----
    
    # Make an additional category for loess
    soils.obic[soiltype == 'loess', soiltype.n := 'loess'] 
    # merge with dt.score
    dt.score <- merge(dt.score, soils.obic[, list(soiltype, soiltype.n)], by.x = "B_BT_AK", by.y = "soiltype")
    
    # add crop_maatregel to crops.obic
    crops.obic[grepl('grasland|natuur', crop_waterstress), crop_maatregel := "melkveehouderij"]
    crops.obic[grepl('granen|suikerbiet|aardappel|mais|overig', crop_waterstress), crop_maatregel := "akkerbouw"]
    crops.obic[grepl('groenten|bloembollen', crop_waterstress), crop_maatregel := "groente"]
    crops.obic[grepl('boomteelt|fruit', crop_waterstress), crop_maatregel := "boomteelt"]
    # merge with dt.score
    dt.score <- merge(dt.score, crops.obic[, list(crop_code, crop_maatregel)], by.x = "B_LU_BRP", by.y = "crop_code")
    setkey(dt.score, ID)
    
    
  
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