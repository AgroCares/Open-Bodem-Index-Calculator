#' Waardeer de verkruimelbaarheid
#'
#' Met deze functie wordt de verkruimelbaarheid van de bodem gewaardeerd
#' 
#' @param lutum (numeric) Het percentage lutum aanwezig in de bodem
#' @param leem (numeric) Het percentage leem aanwezig in de bodem
#' @param os (numeric) Het percentage organisch stofgehalte in de bodem
#' @param ph (numeric) Het pH-gehalte van de bodem
#' 
#' @import data.table
#' 
#' @importFrom stats approxfun
#'
#' @export
verkruimelbaarheid <- function(lutum, leem, os, ph) {
  
  # Check invoer
  checkmate::assert_numeric(lutum, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(leem, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(os, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(ph, lower = 0, upper = 14, any.missing = FALSE, min.len = 1)

  # Verzamel de data in een data.table
  correctie.os = correctie.ph = waardering.lutum = NULL # due to NSE notes in R CMD check https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  dt <- data.table(
    lutum = lutum,
    leem = leem,
    os = os, 
    ph = ph
  )
  
  # Als lutum kleiner is dan 4 of groter dan 40 geef minimale en maximale waarde
  dt[lutum < 4, waardering := 10]
  dt[lutum > 40, waardering := 1]
  
  # Stel de waarderingsfunctie voor lutum op en bereken waardering voor lutum
  waardering.lutum.tbl <- data.frame(
    lutum = c(4, 10, 17, 24, 30, 40, 100),
    basiswaarde = c(10, 9, 8, 6.5, 5, 3.5, 1)
  )
  waardering.lutum.fun <- approxfun(x = waardering.lutum.tbl$lutum, y = waardering.lutum.tbl$basiswaarde, rule = 2)
  dt[is.na(waardering), waardering.lutum := waardering.lutum.fun(lutum)]
    
  # Stel de correctiefunctie voor os op
  correctie.os.tbl <- data.frame(
    basiswaarde = c(10, 9, 8, 6.5, 5, 3.5, 1),
    correctie = c(0, 0.06, 0.09, 0.12, 0.25, 0.35, 0.46)
  )
  correctie.os.fun <- approxfun(x = correctie.os.tbl$basiswaarde, y = correctie.os.tbl$correctie, rule = 2)
  dt[is.na(waardering), correctie.os := correctie.os.fun(lutum)]
    
  # Stel de correctiefunctie op voor pH
  correctie.ph.tbl <- data.frame(
    basiswaarde = c(10, 9, 8, 6.5, 5, 3.5, 1),
    correctie = c(0, 0, 0.15, 0.3, 0.7, 1, 1.5)
  )
  correctie.ph.fun <- approxfun(x = correctie.ph.tbl$basiswaarde, y = correctie.ph.tbl$correctie, rule = 2)
  dt[is.na(waardering) & ph < 7, correctie.ph := correctie.ph.fun(waardering.lutum)]
  dt[is.na(waardering) & ph >= 7, correctie.ph := 0]

  # Bereken de waardering
  dt[is.na(waardering), waardering := waardering.lutum + correctie.os * os - correctie.ph]
  waardering <- dt[, waardering]
  
  return(waardering)
}
