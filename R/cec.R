#' Calculate the index for the CEC-occupation
#' 
#' This function calculates the CEC-buffer index for a soil
#' 
#' @param A_CEC_CO (numeric) The cation exchange capacity (mmol+ / kg) 
#' @param A_K_CEC (numeric) The occupation of the CEC with K (\%)
#' @param A_CA_CEC (numeric) The occupation of the CEC with Mg (\%)
#' @param A_MG_CEC (numeric) The occupation of the CEC with Ca (\%)
#' @param A_OS_GV (numeric) The organic matter content of soil in percentage
#' @param B_BT_AK (character) The type of soil
#' @param advice (character) Optional parameter to select CEC index for soil structure or fertility. Options: fertility_index or structure_index
#'    
#' @import data.table
#' 
#' @export
calc_cec <- function(A_CEC_CO,A_K_CEC,A_CA_CEC,A_MG_CEC, advice) {
  
  id = fertility_index = structure_index = NULL
  
  # Check inputs
  arg.length <- max(length(A_CEC_CO), length(A_K_CEC), length(A_CA_CEC), length(A_MG_CEC))
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_K_CEC, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CA_CEC, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_MG_CEC, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(advice, any.missing = FALSE, min.len = 1, len = 1)
  checkmate::assert_subset(advice, choices = c('fertility_index','structure_index'), empty.ok = FALSE)
  checkmate::assert_subset(B_BT_AK, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_OS_GV, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_CEC_CO = A_CEC_CO,
    A_K_CEC = A_K_CEC,
    A_CA_CEC = A_CA_CEC,
    A_MG_CEC = A_MG_CEC,
    B_BT_AK = B_BT_AK,
    A_OS_GV = A_OS_GV,
    fertility_index = NA_real_,
    structure_index = NA_real_
    )
  
  # Calculate CEC index for soil fertility
  dt[, fertility_index := A_CEC_CO]
  
  # Calculate CEC index for structure on sandy soils (normalized to value beween 0-1)
  dt[grepl('zand|dal',B_BT_AK), structure_index := sqrt((A_CA_CEC - 80)^2 + (A_MG_CEC - 8)^2 + (A_K_CEC - 3.5)^2) / 125]
  
  # Calculate CEC index for structure on non-sandy soils (normalized to value beween 0-1)
  dt[!grepl('zand|dal',B_BT_AK), structure_index := sqrt((A_CA_CEC - 85)^2 + (A_MG_CEC - 8)^2 + (A_K_CEC - 3.5)^2) / 125]
  
  # Aggregate stability not relevant measure for peat soils
  dt[A_OS_GV > 25, structure_index := 1]
  
  # Restrict the value to be <= 1
  dt[, structure_index := pmin(1, structure_index)]
  
  # select output value given required advice
  setorder(dt, id)
  dt[, value := mget(advice)]
  value <- dt[,value]
  return(value)
}

#' Calculate the indicator for the CEC
#' 
#' This function calculates the indicator for the the CEC  of the soil by using the CEC-index calculated by \code{\link{calc_cec}}
#' 
#' @param D_CEC (numeric) The value of CEC calculated by \code{\link{calc_cec}}
#' @param advice (character) Optional parameter to select CEC index for soil structure or fertility. Options: fertility_index or structure_index
#' 
#' @export
ind_cec <- function(D_CEC,advice = 'fertility_index') {
  
  # Check inputs
  if(advice == 'fertility_index'){checkmate::assert_numeric(D_CEC, lower = 0, upper = 1000, any.missing = FALSE)} 
  if(advice == 'structure_index'){checkmate::assert_numeric(D_CEC, lower = 0, upper =  1, any.missing = FALSE)} 
  
  # Evaluate the CEC for agricultural production, given impact soil fertility (Goselink & Van Erp, 1999)
  if(advice == 'fertility_index'){value <- pmin(D_CEC * 0.01, 1)}
  
  # Evaluate the CEC for soil structure aspects (as used by Eurofins)
  if(advice == 'structure_index'){value <- 1 - D_CEC}
  
  # return output
  return(value)
}
