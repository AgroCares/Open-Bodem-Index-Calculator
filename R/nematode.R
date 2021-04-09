#' Calculate indicator for plant parasitic nematodes
#'
#' This function calculates the indicator for the presence of plant parasitic nematodes. All nematodes present in a sample are used.
#' A subset of nematodes is weighted in the set regardless of their presence.
#' 
#' @param A_NEMA (data.table) Long data table with the counted nematodes of a parcel.
#' 
#' @import data.table
#' 
#' @export
ind_nematodes <- function(A_NEMA){
  nema.obic <- as.data.table(OBIC::nema.obic)
  
  geel = rood = species = standaard = b = v = count = nem_score = NULL
  
  checkmate::assert_data_table(A_NEMA)
  checkmate::assert_numeric(A_NEMA[,count])
  checkmate::assert_subset(x = A_NEMA[,species],
                           choices = c("Ditylenchus spp."             ,"Ditylenchus dipsaci"          ,"Ditylenchus destructor"       ,"Xiphinema spp."           ,   
                                       "Longidorus spp."              ,"(Para)Trichodoridae spp."     ,"Trichodorus similis"          ,"Trichodorus primitivus"   ,   
                                       "Trichodorus viruliferus"      ,"Trichodorus sparsus"          ,"Trichodorus cylindricus"      ,"Trichodorus hooperi"      ,   
                                       "Paratrichodorus teres"        ,"Paratrichodorus pachydermus"  ,"Paratrichodorus anemones"     ,"Paratrichodorus nanus"    ,   
                                       "Aphelenchoides spp."          ,"Aphelenchoides fragariae"     ,"Aphelenchoides ritzemabosi"   ,"Aphelenchoides subtenuis" ,   
                                       "Criconematidae spp."          ,"Subanguina spp."              ,"Rotylenchus spp."             ,"Paratylenchus spp."       ,   
                                       "Paratylenchus bukowinensis"   ,"Meloidogyne spp."             ,"Meloidogyne chitwoodi/fallax" ,"Meloidogyne chitwoodi"    ,   
                                       "Meloidogyne fallax"           ,"Meloidogyne minor"            ,"Meloidogyne incognita"        ,"Meloidogyne javanica"     ,   
                                       "Meloidogyne artiellia"        ,"Meloidogyne arenaria"         ,"Meloidogyne ardenensis"       ,"Meloidogyne naasi"        ,   
                                       "Meloidogyne hapla"            ,"Cysteaaltjes"                 ,"Hemicycliophora spp."         ,"Pratylenchus spp."        ,   
                                       "Pratylenchus penetrans"       ,"Pratylenchus crenatus"        ,"Tylenchorhynchus spp."        ,"Helicotylenchus spp."     ,   
                                       "Pratylenchus neglectus"       ,"Pratylenchus pratensis"       ,"Pratylenchus thornei"         ,"Pratylenchus flakkensis"  ,   
                                       "Pratylenchus fallax"          ,"Pratylenchus pinguicaudatus"  ,"Pratylenchus pseudopratensis" ,"Pratylenchus vulnus"      ,   
                                       "Pratylenchus dunensis"        ,"Pratylenchus zeae"))
  
  # merge dd and nema.obic and remove non standard non counted nematodes from dd
  dd <- merge.data.table(nema.obic, A_NEMA, by = 'species', all.x = TRUE)
  dd <- dd[standaard == TRUE|!is.na(count)]
  
  # Check if all standard nematodes are present
  if(checkmate::anyMissing(dd[,count])){
    errorCondition('at least one of the "standard" nematodes seems to be a missing value, its assumed this nematode is counted and is equal to 0.')
  } 
  # Calculate score for each individual nematode species
  dd[,nem_score := OBIC::evaluate_logistic(dd[,count], b = dd[,b], x0 = dd[,geel], v = dd[,v], increasing = FALSE)]
  # Set scores where count = 0 to 1
  dd[count == 0, nem_score:=1]
  
  value <- mean(dd[,nem_score])
  return(value)
} 
