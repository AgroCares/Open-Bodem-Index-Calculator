#' Calculate soil sealing
#' 
#' This function calculates the risks of soil sealing.  This value can be evaluated by \code{\link{eval_sealing}}
#' 
#' @param lutum (numeric) The percentage lutum available of the soil
#' @param om (numeric) The organic matter content of soil in percentage
#' 
#' @import data.table
#' 
#' @importFrom stats approxfun
#' 
#' @export
calc_sealing <- function(lutum, om) {
  
  # Check input
  arg.length <- max(length(lutum), length(om))
  checkmate::assert_numeric(lutum, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(om, lower = 0, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  
  # Setup a table with all the information
  cor.om = value = value.lutum = NULL
  dt <- data.table(
    lutum = lutum,
    om = om, 
    value.lutum = NA_real_,
    cor.om = NA_real_,
    value = NA_real_
  )
  
  # Calculate value.lutum
  df.lutum <- data.frame(
    lutum = c(4, 6, 9, 10, 17, 25, 30, 100),
    value.lutum = c(7, 6, 3, 2, 4, 8, 9, 10)
  )
  fun.lutum <- approxfun(x = df.lutum$lutum, y = df.lutum$value.lutum, rule = 2)
  dt[is.na(value), value.lutum := fun.lutum(lutum)]
  
  # Create organic matter correction function and calculate correction for om
  df.cor.om <- data.frame(
    value.lutum = c(7, 6, 3, 2, 4, 8, 9, 10),
    cor.om = c(0.4, 0.6, 0.8, 1, 0.7, 0.4, 0.3, 0)
  )
  fun.cor.om <- approxfun(x = df.cor.om$value.lutum, y = df.cor.om$cor.om, rule = 2)
  dt[is.na(value), cor.om := fun.cor.om(value.lutum)]
  
  # Calculate the value
  dt[is.na(value), value := value.lutum + cor.om * om]
  value <- dt[, value]
    
  return(value)
}

#' Evaluate the soil sealing
#' 
#' This function evaluates the soil sealing calculated by \code{\link{calc_sealing}}
#' 
#' @param value.sealing (numeric) The value of soil sealing calculated by \code{\link{calc_sealing}}
#' @param crop (numeric) The crop code (gewascode) from the BRP
#' 
#' @import data.table
#' 
#' @export
eval_sealing <- function(value.sealing, crop) {
  
  # Load in the crops dataset
  crop_code = crop_sealing = id = NULL
  crops.obic <- as.data.table(OBIC::crops.obic)
  setkey(crops.obic, crop_code)
  
  # Check inputs
  arg.length <- max(length(value.sealing), length(crop))
  checkmate::assert_numeric(value.sealing, lower = 0, upper = 10, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(crop, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(crop, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  
  # Collect data into a table
  dt <- data.table(
    id = 1:arg.length,
    value.sealing = value.sealing,
    crop = crop,
    eval.sealing = NA_real_
  )
  setkey(dt, crop)
  dt <- crops.obic[dt]
  setorder(dt, id)

  # Evaluate the sealing
  dt[crop_sealing == "overig", eval.sealing := OBIC::evaluate_logistic(x = value.sealing, b = 1.5, x0 = 0.3, v = 0.35)]
  dt[crop_sealing == "gras", eval.sealing := 1]
  
  eval.sealing <- dt[, eval.sealing]

  return(eval.sealing)
}
