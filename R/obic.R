#' Calculate the Open Bodem Index score
#' 
#' This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI).
#' 
#' @param dt (data.table) A data.table containing the data of the fields to calcualte the OBI
#' 
#' @import data.table
#' 
#' @export
obic <- function(dt) {
  
  # Check inputs
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(
    colnames(dt), 
    empty.ok = FALSE,
    choices = 
      c("ID", "YEAR", "A_N_PMN", "A_CA_CEC", "A_CEC_CO", "A_CN_RAT", "A_K_CEC", "A_CLAY_MI", "A_K_CC", "A_KALK_MI",
        "A_MG_CEC", "A_MG_PAE", "A_N_TOT", "A_OS_GV", "A_P_PAE", "A_P_PAL","A_PH_CC", "A_P_PW", "A_SILT_MI", "A_S_TOT",
        "A_SAND_MI", "B_GT", "B_BT_AK", "A_C_FR", "A_MB_PFLA", "A_SB_FR", "A_B_BC", "A_VO_BC", "A_RW_BC", "A_BS_BS",    
        "A_GV_BC", "A_PV_BC", "A_AS_BC", "A_SV_BC", "B_LU_BRP", "B_BT_AK", "B_NR_RIVM", "B_RWT_DANK", "B_RWA_DANK", "B_OV_WENR", 
        "B_HELP_WENR", "M_M1", "M_M2", "M_M3", "M_M4", "M_M5", "M_M6", "M_M7", "M_M8", "M_M9")
    )
  
  # Run the preprocessing
  dt.ppr <- OBIC::obic_preprocessing(dt)
  
  # Calculate the indicators
  dt.ind <- OBIC::obic_indicators(dt.ppr)
  
  # Score the fields
  dt.score <- OBIC::obic_score(dt.ind)
  
  # Run the managment measures
  dt.recom <- OBIC::obic_recommendations(dt.score)
  
  return(dt.recom)
  
}