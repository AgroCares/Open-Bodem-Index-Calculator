#' Calculate groundwater recharge of a soil
#' 
#' This function calculates an index score for groundwater storage based on WHC, sealing risk, drainage and subsoil compaction
#' 
#' 
#' @param D_WRI_WHC (numeric) The value for Water Retention index (WRI) for WHC as calculated by \code{\link{calc_waterretention}}
#' @param D_PSP (numeric) The precipitation surplus per crop  calculated by \code{\link{calc_psp}}
#' @param I_P_SE (numeric) The indicator value for soil sealing
#' @param I_P_CO (numeric) The indicator value for occurrence of subsoil compaction
#' @param B_DRAINAGE (boolean) Are drains installed to drain the field (options: yes or no)
#' 
#'         
#' @export
ind_gw_recharge <- function(D_WRI_WHC, D_PSP, I_P_SE, I_P_CO, B_DRAINAGE = FALSE){
  
  I_WRI_WHC = cf_compaction = cf_drain = D_I_PSP = NULL
  
  # Check inputs
  arg.length <- max(length(I_P_CO),length(B_DRAINAGE))
  
  checkmate::assert_numeric(I_P_CO, any.missing = FALSE, len = arg.length)
  checkmate::assert_logical(B_DRAINAGE, any.missing = FALSE, len = arg.length)
  
  # import data into table
  dt <- data.table(D_WRI_WHC = D_WRI_WHC,
                   I_P_SE = I_P_SE,
                   I_P_CO = I_P_CO,
                   B_DRAINAGE = B_DRAINAGE)
  
  # Calculate the indicator value for water holding capacity
  dt[,I_WRI_WHC := ind_waterretention(D_WRI_WHC,type = 'water holding capacity')]
  
  # Calculate indicator value for precipitation surpluss
  dt[,D_I_PSP := evaluate_logistic(D_PSP,0.05,300,2.5)]
  
  # Correct for subsoil compaction or drainage
  dt[B_DRAINAGE == TRUE, c('cf_drain','cf_compaction') := list(0.6,1)]
  dt[B_DRAINAGE == FALSE, c('cf_drain','cf_compaction') := list(1,fifelse(I_P_CO < 0.5,0.8,1))]
  
  # Calculate aggregated score
  dt[,I_W_GWS := (0.35 * I_WRI_WHC + 0.35 * D_I_PSP + 0.3 * I_P_SE) * cf_compaction * cf_drain]
  
  # Calculate mean
  out <- dt[,I_W_GWS]
  
  return(out)
}



