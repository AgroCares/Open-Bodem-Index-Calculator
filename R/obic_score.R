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
  
  # make local copy
  dt.ind <- copy(dt.ind)
  
  # define variables used within the function
  S_C = S_P = S_B = S_M = S_T = ID =  NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_SF = I_B_SB = I_M = NULL
  I_P_CEC = I_P_WRI = NULL
  
  # Load in the datasets
  w <- as.data.table(OBIC::weight.obic)
  
  # Score the chemical indicators
  dt.ind[, S_C := 	w$W_C_N * I_C_N + w$W_C_P * I_C_P + w$W_C_K * I_C_K + 
					w$W_C_MG * I_C_MG + w$W_C_S * I_C_S + w$W_C_PH * I_C_PH + 
					w$W_C_CEC * I_C_CEC + w$W_C_CU * I_C_CU + w$W_C_ZN * I_C_ZN]
  
  # Score the physical indicators
   dt.ind[, S_P :=  w$W_P_CR * I_P_CR + w$W_P_SE * I_P_SE + w$W_P_MS * I_P_MS +  
					w$W_P_BC * I_P_BC + w$W_P_DU * I_P_DU + w$W_P_CO * I_P_CO + 
					w$W_P_CEC * I_P_CEC + w$W_P_WRI * I_P_WRI]
  
  # Score the biology
  dt.ind[, S_B := w$W_B_DI * I_B_DI + w$W_B_SF * I_B_SF + w$W_B_SB * I_B_SB]
  
  
  # Score the management
  dt.ind[, S_M := I_M]
  
  # Calculate the total score
  dt.ind[, S_T := 0.3*S_C + 0.3*S_P + 0.3*S_B + 0.1*S_M]
  
  # Aggregate per field over the last 10 years
  col.sel <- colnames(dt.ind)[grepl("ID|^I_|^S_", colnames(dt.ind))]
  dt.ind <- dt.ind[, lapply(.SD, mean), by = ID, .SDcols = col.sel]
  
  # return only the indices and the scores
  return(dt.ind)
}

#' Weight of indicators to calculate integrated scores
#' 
#' This table defines the weighting factors (ranging between 0 and 1) of indicator values to calculate integrated scores. 
#' This table is used internally in \code{\link{obic_score}}
#' 
"weight.obic"