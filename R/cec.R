#' Calculate a soil fertility index based on the CEC
#' 
#' This function calculates the capacity of the soil to buffer cations
#' 
#' @param A_CEC_CO (numeric) The cation exchange capacity (mmol+ / kg) 
#'    
#' @import data.table
#' 
#' @export
calc_cec <- function(A_CEC_CO) {
  
  id = NULL
  
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  
  # Check inputs
  arg.length <- max(length(A_CEC_CO))
  checkmate::assert_numeric(A_CEC_CO, lower = 0, upper = 1000, any.missing = FALSE, len = arg.length)
 
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_CEC_CO = A_CEC_CO,
    value = NA_real_
    )
  
  # Calculate CEC index for soil cation buffer capacity
  dt[, value := A_CEC_CO]
  
  # select output value
  value <- dt[,value]
  
  # return 
  return(value)
}

#' Calculate aggregate stability index based on occupation CEC
#' 
#' This function calculates an aggregate stability index given the CEC and its occupation with major cations.
#' 
#' @param B_SOILTYPE_AGR (character) The type of soil
#' @param A_SOM_LOI (numeric) The organic matter content of soil in percentage
#' @param A_K_CO_PO (numeric) The occupation of the CEC with K (\%)
#' @param A_CA_CO_PO (numeric) The occupation of the CEC with Ca (\%)
#' @param A_MG_CO_PO (numeric) The occupation of the CEC with Mg (\%)
#'    
#' @import data.table
#' 
#' @export
calc_aggregatestability <- function(B_SOILTYPE_AGR,A_SOM_LOI,A_K_CO_PO,A_CA_CO_PO,A_MG_CO_PO) {
  
  id = NULL
  
  # Load in the datasets
  soils.obic <- as.data.table(OBIC::soils.obic)
  
  # Check inputs
  arg.length <- max(length(B_SOILTYPE_AGR),  length(A_SOM_LOI),length(A_K_CO_PO), length(A_CA_CO_PO), length(A_MG_CO_PO))
  checkmate::assert_numeric(A_K_CO_PO, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_CA_CO_PO, lower = 0, upper = 400, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(A_MG_CO_PO, lower = 0, upper = 50, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(B_SOILTYPE_AGR, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_SOILTYPE_AGR, choices = unique(soils.obic$soiltype), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    B_SOILTYPE_AGR = B_SOILTYPE_AGR,
    A_SOM_LOI = A_SOM_LOI,
    A_K_CO_PO = A_K_CO_PO,
    A_CA_CO_PO = A_CA_CO_PO,
    A_MG_CO_PO = A_MG_CO_PO,
    value = NA_real_
  )
  
  # Calculate aggregate stability on on sandy soils (normalized to value beween 0-1)
  dt[grepl('zand|dal',B_SOILTYPE_AGR), value := sqrt((A_CA_CO_PO - 80)^2 + (A_MG_CO_PO - 8)^2 + (A_K_CO_PO - 3.5)^2) / 125]
  
  # Calculate aggregate stability on non-sandy soils (normalized to value beween 0-1)
  dt[!grepl('zand|dal',B_SOILTYPE_AGR), value := sqrt((A_CA_CO_PO - 85)^2 + (A_MG_CO_PO - 8)^2 + (A_K_CO_PO - 3.5)^2) / 125]
  
  # Aggregate stability not relevant measure for peat soils
  dt[A_SOM_LOI > 25, value := 0]
  
  # Restrict the value to be <= 1
  dt[, value := pmin(1, value)]
  
  # select output value given required advice
  setorder(dt, id)

  # select output value
  value <- dt[,value]
  
  # return 
  return(value)
}

#' Calculate the indicator for soil fertility given the CEC
#' 
#' This function estimate how much cations can be buffer by soil, being calculated by \code{\link{calc_cec}}
#' 
#' @param D_CEC (numeric) The value of CEC calculated by \code{\link{calc_cec}}
#' 
#' @export
ind_cec <- function(D_CEC) {
  
  # Check inputs
  checkmate::assert_numeric(D_CEC, lower = 0, upper = 1000, any.missing = FALSE) 
 
  # Evaluate the CEC for agricultural production, given impact soil cation buffer capacity (Goselink & Van Erp, 1999)
  value <- pmin(D_CEC * 0.01, 1)
  
  # Return output
  return(value)
}

#' Calculate the indicator aggregate stability 
#' 
#' This function calculates the indicator for the the aggregate stability of the soil by using the index calculated by \code{\link{calc_aggregatestability}}
#' 
#' @param D_AS (numeric) The value of aggregate stability calculated by \code{\link{calc_aggregatestability}}
#' 
#' @export
ind_aggregatestability <- function(D_AS) {
  
  # Check inputs
  checkmate::assert_numeric(D_AS, lower = 0, upper =  1, any.missing = FALSE) 
  
  # Evaluate the CEC for soil aggregation aspects (as used by Eurofins)
  value <- 1 - D_AS
  
  # return output
  return(value)
}