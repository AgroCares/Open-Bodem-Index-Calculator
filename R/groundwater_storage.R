#' Calculate groundwater storage capacity
#' 
#' This function calculates an index score for groundwater storage based on WHC, sealing risk, drainage and subsoil compaction
#' 
#' 
#' @param B_LU_BRP (numeric) The crop code from the BRP 
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param A_SAND_MI (numeric) The sand content of the soil (\%)
#' @param A_SILT_MI (numeric) The silt content of the soil (\%)
#' @param A_SOM_LOI (numeric) The organic matter content of the soil (\%)
#' @param B_COMPACTION (boolean) Is the subsoil compacted (options: yes or no)
#' @param B_DRAINAGE (boolean) Are drains installed to drain the field (options: yes or no)
#' 
#'         
#' @export
ind_gw_storage <- function(){
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP))
  
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(crops.obic$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.5, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_SILT_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_SILT_MI, lower = 0.1, upper = 75, any.missing = FALSE, len = 1)
  checkmate::assert_logical(B_COMPACRION,any.missing = FALSE, len = 1)
  checkmate::assert_logical(M_DRAINAGE,any.missing = FALSE, len = 1)
  
  # import data into table
  dt <- data.table(B_LU_BRP = B_LU_BRP,
                   A_CLAY_MI = A_CLAY_MI,
                   A_SAND_MI = A_SAND_MI,
                   A_SILT_MI = A_SILT_MI,
                   A_SOM_LOI = A_SOM_LOI,
                   B_COMPACTION = B_COMPACTION,
                   B_DRAINAGE = B_DRAINAGE)
  
  
  # Calculate water holding capacity
  dt[,WHC := calc_waterretention(A_CLAY_MI,A_SAND_MI,A_SILT_MI,A_SOM_LOI,type = 'water holding capacity')]
  dt[,I_WHC := ind_waterretention(WHC,type = 'water holding capacity')]
  
  # Calculate soil sealing risk
  dt[,SEAL := calc_sealing_risk(A_SOM_LOI,A_CLAY_MI)]
  dt[,I_SEAL := ind_sealing(SEAL,B_LU_BRP)]
  
  # Correct for subsoil compaction or drainage
  dt[B_DRAINAGE == TRUE, c('cf_drain','cf_compaction') := list(0.6,1)]
  dt[B_DRAINAGE == FALSE,c('cf_drain','cf_compaction') := list(1,fifelse(B_COMPACTION,0.8,1))]

    
  
  # Calculate aggregated score
  dt[,I_GWS := (0.6 * I_WHC + 0.4 * I_SEAL) * cf_comp * cf_drain]
  
  out <- mean(dt[,I_GWS])
  
  return(out)
}



