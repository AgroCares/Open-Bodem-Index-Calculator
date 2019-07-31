#' Score the indicators for the OBI
#' 
#' This wrapper function contains the functions to weight the evaluations.
#' 
#' @param dt.ind (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_score <- function(dt.ind) {
  
  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  S_C = S_P = S_B = S_M = S_T = ID =  NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_OM = I_B_SF = I_B_SB = I_M = NULL
  
  # Score the chemical indicators
  dt.ind[, S_C := 0.2*I_C_N + 0.2*I_C_P + 0*I_C_K + 0.2*I_C_MG + 0.2*I_C_S + 0.2*I_C_PH + 0*I_C_CEC + 0*I_C_CU + 0*I_C_ZN ]
  
  # Score the physical inidcators
  dt.ind[, S_P :=  0*I_P_CR + 0.333*I_P_SE + 0*I_P_MS + 0*I_P_BC + 0.333*I_P_DU + 0.333*I_P_CO]
  
  # Score the biology
  dt.ind[, S_B := 1*I_B_DI + 0*I_B_OM + 0*I_B_SF + 0*I_B_SB]
  
  # Score the management
  dt.ind[, S_M := I_M]
  
  # Calculate the total score
  dt.ind[, S_T := 0.333*S_C + 0.333*S_P + 0.333*S_B + 0.1*S_M]
  
  # Aggregate per field
  col.sel <- colnames(dt.ind)[grepl("ID|^I_|^S_", colnames(dt.ind))]
  dt.ind <- dt.ind[, lapply(.SD, mean), by = ID, .SDcols = col.sel]
  
  return(dt.ind)
}