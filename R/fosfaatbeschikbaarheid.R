#' Fosfaatbeschikbaarheid
#' 
#' Geeft een waardering voor de fosfaatbeschikbaarheid
#' 
#' @param p_al (numeric) Het P-AL gehalte van de bodem
#' @param p_cacl2 (numeric) Het P-CaCl2 gehalte van de bodem
#' @param gewas (character) Het type gewas dat geteelt wordt. Op dit moment is de keuze `gras`, `mais`
#' 
#' @import data.table
#' 
#' @export
fosfaatbeschikbaarheid <- function(p_al, p_cacl2, gewas) {
  
  # Check invoer
  arg.length <- max(length(p_al), length(p_cacl2), length(gewas))
  checkmate::assert_numeric(p_al, lower = 8, upper = 70, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(p_cacl2, lower = 0.3, upper = 5, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(gewas, choices = c("gras", "mais"), empty.ok = FALSE)
  
  # Verzamel de data in een data.table
  dt <- data.table(
    p_al = p_al,
    p_cacl2 = p_cacl2,
    gewas = gewas,
    pbi = NA_real_,
    waardering = NA_real_
  )
  
  # Bereken PBI voor grasland
  dt[gewas == "gras", pbi := 2 + 2.5 * log(p_cacl2) + 0.036 * p_al / p_cacl2]
  dt[gewas == "mais", pbi := p_cacl2 + 0.05 * (p_al / p_cacl2)]
  
  # Bereken de waardering voor fosfaatbeschikbaarheid
  dt[pbi < 1.5, waardering := 0.33]
  dt[pbi >= 1.5 & pbi < 2.6, waardering := 0.66]
  dt[pbi >= 2.6, waardering := 1]
  
  waardering <- dt[, waardering]
  return(waardering)
}