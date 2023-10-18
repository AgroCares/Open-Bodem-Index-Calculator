#' Calculate groundwater recharge of a soil
#' 
#' This function calculates an index score for groundwater storage based on precipitation surplus, infiltration at saturation, sealing risk, drainage and subsoil compaction
#' 
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_psp}}
#' @param D_WRI_K (numeric) The value for top soil permeability (cm/d) as calculated by \code{\link{calc_permeability}}
#' @param I_P_SE (numeric) The indicator value for soil sealing
#' @param I_P_CO (numeric) The indicator value for occurrence of subsoil compaction
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' @param B_GWL_CLASS (character) The groundwater table class
#' 
#' @examples 
#' ind_gw_recharge(B_LU_BRP = 265,D_PSP = 200, D_WRI_K = 10, I_P_SE = 0.6, I_P_CO = 0.9, 
#' B_DRAIN = FALSE, B_GWL_CLASS = 'GtV')
#' ind_gw_recharge(B_LU_BRP = 233, D_PSP = 400, D_WRI_K = 10, I_P_SE = 0.4, I_P_CO = 0.2, 
#' B_DRAIN = TRUE, B_GWL_CLASS = 'GtII')
#'
#' @return 
#' The evaluated score for the soil function to improve groundwater recharge. A numeric value between 0 and 1.
#'          
#' @export
ind_gw_recharge <- function(B_LU_BRP, D_PSP, D_WRI_K, I_P_SE, I_P_CO, B_DRAIN, B_GWL_CLASS){
  
  I_H_GWR = D_I_WRI_K = cf_compaction = cf_drain = D_I_PSP = NULL
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP),length(D_WRI_K),length(D_PSP),length(I_P_SE),length(I_P_CO),length(B_DRAIN))
  
  
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(OBIC::crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(D_WRI_K, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(D_PSP, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(I_P_SE, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(I_P_CO, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(B_DRAIN, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(B_GWL_CLASS, choices = c('GtI','GtII','GtIII','GtIV','GtV','GtVI','GtVII','GtVIII',
                                                    'GtIIb','GtIIIb','GtVb'), empty.ok = FALSE)
  
  # import data into table
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   D_PSP = D_PSP,
                   D_WRI_K = D_WRI_K,
                   I_P_SE = I_P_SE,
                   I_P_CO = I_P_CO,
                   B_DRAIN = B_DRAIN,
                   B_GWL_CLASS = B_GWL_CLASS)
  
  # Calculate the indicator value for water holding capacity
  dt[,D_I_WRI_K := ind_permeability(D_WRI_K)]
  
  # Calculate indicator value for precipitation surplus
  dt[,D_I_PSP := ind_psp(D_PSP,B_LU_BRP)]
  
  # Correct for subsoil compaction or drainage
  dt[B_DRAIN == TRUE & B_GWL_CLASS %in% c('GtIIIb','GtIV'), c('cf_drain','cf_compaction') := list(0.6,1)]
  dt[B_DRAIN == FALSE | B_GWL_CLASS %in% c('GtI','GtII','GtIII','GtV','GtVI','GtVII','GtVIII','GtIIb','GtVb'),
     c('cf_drain','cf_compaction') := list(1,fifelse(I_P_CO < 0.5,0.8,1))]
  
  # Calculate aggregated score
  dt[,I_H_GWR := (0.7 * D_I_PSP + 0.15 * D_I_WRI_K + 0.15 * I_P_SE) * cf_compaction * cf_drain]
  
  # Calculate mean
  out <- dt[,I_H_GWR]
  
  return(out)
}



#' Calculate the permeability of the top soil
#' 
#' This function calculates the permeability of the top soil
#' 
#' 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' 
#'         
#' @export
calc_permeability <- function(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI){
  
  id = thetaS = thetaR = alfa = n = ksat = l = Pleem = mineral = D_WRI_K = NULL
  
  # Check inputs
  arg.length <- max(length(A_CLAY_MI), length(A_SAND_MI),length(A_SILT_MI), length(A_SOM_LOI))
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SAND_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SILT_MI, lower = 0, upper = 100, any.missing = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0, upper = 100, any.missing = FALSE)
  
  # Collect data in a table
  dt <- data.table(
    id = 1:arg.length,
    A_CLAY_MI = A_CLAY_MI,
    A_SAND_MI = A_SAND_MI,
    A_SILT_MI = A_SILT_MI,
    A_SOM_LOI = A_SOM_LOI,
    value = NA_real_
  )
  
  # Calculate mineral fractions
  dt[,mineral   := A_CLAY_MI + A_SAND_MI + A_SILT_MI]
  dt[,A_CLAY_MI := A_CLAY_MI * 100 / mineral]
  dt[,A_SILT_MI := A_SILT_MI * 100 / mineral]
  dt[,Pleem     := A_CLAY_MI + A_SILT_MI]
  
  
  # Calculate unsaturated permeability
  dt[,  c("Dichtheid", "thetaR", "thetaS", "alfa", "n", "ksat", "l") := pFpara_ptf_Wosten2001(A_CLAY_MI, Pleem, A_SOM_LOI, 150, 1)]
  
  # Calculate permeabilty, based on Wosten et al., 2001
  dt[,D_WRI_K := ksat * (((1 + alfa * thetaR^n)^(1 - 1/n) - alfa * thetaR^(n - 1))^2) / ((1 + alfa * thetaR^n)^((1 - 1/n) * (l + 2)))]
  
  # output
  return(dt[,D_WRI_K])
  
}



#' Calculate the indicator score for the permeability of the top soil
#' 
#' This function calculates the indicator score for the permeability of the top soil
#' 
#' 
#' @param D_WRI_K (numeric) The value for top soil permeability (cm/d) as calculated by \code{\link{calc_permeability}}
#' 
#'         
#' @export
ind_permeability <- function(D_WRI_K){
  
  # Check inputs
  checkmate::assert_numeric(D_WRI_K, lower = 0, any.missing = FALSE)
  
  D_I_WRI_K <- evaluate_logistic(D_WRI_K,0.08,50,0.4)
  
  return(D_I_WRI_K)
}

