#' Calculate a soil fertility index based on the CEC
#' 
#' This function calculates the capacity of the soil to buffer cations
#' 
#' @param A_CEC_CO (numeric) The cation exchange capacity (mmol+ / kg) 
#'    
#' @import data.table
#' 
#' @examples
#' calc_cec(A_CEC_CO = 85)
#' calc_cec(A_CEC_CO = c(85,125,326))
#' 
#' @return 
#' The capacity of the soil to buffer cations. A numeric value.
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
#' @examples 
#' calc_aggregatestability(B_SOILTYPE_AGR = 'dekzand', A_SOM_LOI = 3.5, 
#' A_K_CO_PO = 6,A_CA_CO_PO = 83 ,A_MG_CO_PO = 9)
#' calc_aggregatestability(B_SOILTYPE_AGR = c('dekzand','rivierklei'), A_SOM_LOI = c(3.5,6.5), 
#' A_K_CO_PO = c(6,9),A_CA_CO_PO = c(83,75) ,A_MG_CO_PO = c(9,4))
#' 
#' @return 
#' The aggregate stability index of a soil given the Cation Exchange Capacity and its composition with major cations. A numeric value.
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
#' @examples 
#' ind_cec(D_CEC = 85)
#' ind_cec(D_CEC = c(85,135,385))
#' 
#' @return
#' The evaluated score for the soil function to buffer cations. A numeric value between 0 and 1. 
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
#' @examples 
#' ind_aggregatestability(D_AS = 0.3)
#' ind_aggregatestability(D_AS = c(0.3,0.6,0.9))
#' 
#' @return
#' The evaluated score for the soil function aggregate stability. A numeric value between 0 and 1. 
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