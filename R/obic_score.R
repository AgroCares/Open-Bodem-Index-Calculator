#' Score the indicators for the OBI
#' 
#' This wrapper function contains the functions to perform the scoring
#' 
#' @param dt.ind (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_score <- function(dt.ind) {
  
  ID = null
  
  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  # Score on a absolute scale
  dt.score.abs <- score_absolute(dt.ind)
  
  # Score on a relative scale
  dt.score <- score_relative(dt.score.abs)

  # Aggregate per field
  col.sel <- colnames(dt.score)[grepl("ID|^I_|^S_", colnames(dt.score))]
  dt.score <- dt.ind[, lapply(.SD, mean), by = ID, .SDcols = col.sel]
  
  return(dt.score)
}


#' Score the indicators for the OBI on an absolute scale
#' 
#' This function coalculates the absolute score for the OBI baed on the indicators
#' 
#' @param dt.ind (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
score_absolute <- function(dt.ind) {
  
  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  S_A_C = S_A_P = S_A_B = S_A_M = S_A_T =  NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_OM = I_B_SF = I_B_SB = I_M = NULL
  I_P_CEC = NULL
  
  # Score the chemical indicators
  dt.ind[, S_A_C := (1/9)*I_C_N + (1/9)*I_C_P + (1/9)*I_C_K + (1/9)*I_C_MG + (1/9)*I_C_S + (1/9)*I_C_PH + (1/9)*I_C_CEC + (1/9)*I_C_CU + (1/9)*I_C_ZN ]
  
  # Score the physical indicators
  dt.ind[, S_A_P :=  0.2*I_P_CR + 0.2*I_P_SE + 0*I_P_MS + 0*I_P_BC + 0.2*I_P_DU + 0.2*I_P_CO + 0.2 * I_P_CEC]
  
  # Score the biology
  dt.ind[, S_A_B := 1*I_B_DI + 0*I_B_OM + 0*I_B_SF + 0*I_B_SB]
  
  # Score the management
  dt.ind[, S_A_M := I_M]
  
  # Calculate the total score
  dt.ind[, S_A_T := 0.333*S_A_C + 0.333*S_A_P + 0.333*S_A_B + 0.1*S_A_M]
  
  return(dt.ind)
}

#' Score the scoring for the OBI on a relative scale
#' 
#' This function calculates the relative scoring based on the score calculated by \code{\link{score_absolute}
#' 
#' @param dt.score.abs (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
score_relative <- function(dt.score.abs) {
  
  # Check inputs
  checkmate::assert_data_table(dt.score.abs)
  
  
  
  return(dt.score)
}