#' Evaluate the structure of the soil
#' 
#' This function combines the evalutions for the subject of soil structre and gives an evaluation for it
#' 
#' @param eval.crumbleability (numeric) The evaluation value of crumbleability by \code{\link{ind_crumbleability}}
#' @param eval.sealing (numeric) the evaluation value of sealing by \code{\link{ind_sealing}}
#' 
#' @export
eval_structure <- function(eval.crumbleability, eval.sealing) {
  
  # Check inputs
  arg.length <- max(length(eval.crumbleability), length(eval.sealing))
  checkmate::check_numeric(eval.crumbleability, lower = 0, upper  = 1, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::check_numeric(eval.sealing, lower = 0, upper  = 1, any.missing = FALSE, min.len = 1, len = arg.length)
  
  # Set the weights
  weight.crumbleability <- 0.5
  weight.sealing <- 0.5
  weight.total <- sum(weight.crumbleability, weight.sealing)
  
  # Evaluate the soil structure
  eval.structure <- weight.crumbleability * eval.crumbleability + weight.sealing * eval.sealing
  eval.structure <- eval.structure / weight.total
  
  return(eval.structure)
}
