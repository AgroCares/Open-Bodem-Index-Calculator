#' Calculate indicator for plant parasitic nematodes
#'
#' This function calculates the indicator for the presence of plant parasitic nematodes. All nematodes present in a sample are used.
#' A subset of nematodes is weighted in the set regardless of their presence.
#' 
#' @param A_NEMA (data.table) Long data table with the counted nematodes of a parcel.
#' 
#' @export
ind_nematodes <- function(A_NEMA){
  obic.nema <- read.csv('DEV/obic.nema.csv')
  setDT(obi.nema)
  
  checkmate::assert_data_table(A_NEMA)
  dd <- merge.data.table(obic.nema, A_NEMA, by = 'species')
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