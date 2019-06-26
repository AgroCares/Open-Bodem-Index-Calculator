#' Waardeer de verkruimelbaarheid
#'
#' Met deze functie wordt de verkruimelbaarheid van de bodem gewaardeerd
#' 
#' @param lutum (numeric) Het percentage lutum aanwezig in de bodem
#' @param leem (numeric) Het percentage leem aanwezig in de bodem
#' @param os (numeric) Het percentage organisch stofgehalte in de bodem
#' @param ph (numeric) Het pH-gehalte van de bodem
#'
#' @export
verkruimelbaarheid <- function(lutum, leem, os, ph) {

  waardering.lutum <- OBIC::waardering_lutum(lutum = lutum)


  return(waardering.lutum)
}

#' Waardering lutum
#' 
#' Waardeer de bodem voor lutum
#' 
#' @param lutum (numeric) Het percentage lutum aanwezig in de bodem
#' 
#' @import stats checkmate
#' 
#' @export
waardering_lutum <- function(lutum) {
  
  # Check lutum input
  checkmate::check_numeric(lutum, lower = 0, upper = 100, any.missing = FALSE, min.len = 2)
  
  # Stel de waarderings functie voor lutum op
  waardering.lutum.tbl <- data.frame(
    lutum = c(4, 10, 17, 24, 30, 40, 100),
    basiswaarde = c(10, 9, 8, 6.5, 5, 3.5, 1)
  )
  waardering.lutum.fun <- approxfun(x = waardering.lutum.tbl$lutum, y = waardering.lutum.tbl$basiswaarde, rule = 2)
  
  # Bereken waardering voor lutum
  waardering.lutum <- waardering.lutum.fun(lutum)
  
  return(waardering.lutum)
}