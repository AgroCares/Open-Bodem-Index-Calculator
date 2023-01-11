#' Calculate groundwater recharge of a soil
#' 
#' This function calculates an index score for groundwater storage based on WHC, sealing risk, drainage and subsoil compaction
#' 
#' 
#' @param D_WRI_WHC (numeric) The value for Water Retention index (WRI) for WHC as calculated by \code{\link{calc_waterretention}}
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_psp}}
#' @param I_P_SE (numeric) The indicator value for soil sealing
#' @param I_P_CO (numeric) The indicator value for occurrence of subsoil compaction
#' @param B_DRAIN (boolean) Are drains installed to drain the field (options: yes or no)
#' 
#' @examples 
#' ind_gw_recharge(D_WRI_WHC = 0.6, D_PSP = 200, I_P_SE = 0.6, I_P_CO = 0.9)
#' ind_gw_recharge(D_WRI_WHC = 0.8, D_PSP = 400, I_P_SE = 0.4, I_P_CO = 0.2)
#'
#' @return 
#' The evaluated score for the soil function to improve groundwater recharge. A numeric value between 0 and 1.
#'          
#' @export
ind_gw_recharge <- function(D_WRI_WHC, D_PSP, I_P_SE, I_P_CO, B_DRAIN = FALSE){
  
  I_WRI_WHC = I_W_GWS = cf_compaction = cf_drain = D_I_PSP = NULL
  
  # Check inputs
  arg.length <- max(length(I_P_CO),length(B_DRAIN))
  checkmate::assert_numeric(D_WRI_WHC, lower = 0, upper = 1000, any.missing = FALSE)
  checkmate::assert_numeric(D_PSP, lower = 0, upper = 1000, any.missing = FALSE)
  checkmate::assert_numeric(I_P_CO, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(I_P_SE, lower = 0, upper = 1, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(B_DRAIN, any.missing = FALSE, len = arg.length)
  
  # import data into table
  dt <- data.table(D_WRI_WHC = D_WRI_WHC,
                   I_P_SE = I_P_SE,
                   I_P_CO = I_P_CO,
                   B_DRAIN = B_DRAIN)
  
  # Calculate the indicator value for water holding capacity
  dt[,I_WRI_WHC := ind_waterretention(D_WRI_WHC,type = 'water holding capacity')]
  
  # Calculate indicator value for precipitation surpluss
  dt[,D_I_PSP := evaluate_logistic(D_PSP,0.05,300,2.5)]
  
  # Correct for subsoil compaction or drainage
  dt[B_DRAIN == TRUE, c('cf_drain','cf_compaction') := list(0.6,1)]
  dt[B_DRAIN == FALSE, c('cf_drain','cf_compaction') := list(1,fifelse(I_P_CO < 0.5,0.8,1))]
  
  # Calculate aggregated score
  dt[,I_W_GWS := (0.35 * I_WRI_WHC + 0.35 * D_I_PSP + 0.3 * I_P_SE) * cf_compaction * cf_drain]
  
  # Calculate mean
  out <- dt[,I_W_GWS]
  
  return(out)
}



