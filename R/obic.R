#' Calculate the Open Bodem Index score
#' 
#' This functions wraps the functions of the OBIC into one main function to calculate the score for Open Bodem Index (OBI).
#' 
#' @param dt (data.table) A data.table containing the data of the fields to calcualte the OBI
#' @param add_relative_score (logical) Should the relative score be calculated? Defaults to TRUE
#' @param add_recommendations (logical) Should recommendations be given to improve the OBI score? Defaults to TRUE
#' 
#' @details The argument add_relative_score calculates relative perforamce of the agricultural fields in the OBI. 
#' To have a meaningfull relative score a large number of fields need be given as input in dt. 
#' 
#' @import data.table
#' 
#' @export
obic <- function(dt, add_relative_score = TRUE, add_recommendations = TRUE) {
  
  # Check inputs
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(
    colnames(dt), 
    empty.ok = FALSE,
    choices = 
      c("ID", "YEAR", "A_N_PMN", "A_CA_CEC", "A_CEC_CO", "A_CN_RAT", "A_K_CEC", "A_CLAY_MI", "A_K_CC", "A_KALK_MI",
        "A_CU_CC","A_MN_CC",'A_ZN_CC',
        "A_MG_CEC", "A_MG_CC", "A_N_TOT", "A_OS_GV", "A_P_PAE", "A_P_PAL","A_PH_CC", "A_P_WA", "A_SILT_MI", "A_S_TOT",
        "A_SAND_MI", "B_GT", "B_BT_AK", "A_C_FR", "A_MB_PFLA", "A_SB_FR", "A_B_BC", "A_VO_BC", "A_RW_BC", "A_BS_BC",    
        "A_GV_BC", "A_PV_BC", "A_AS_BC", "A_SV_BC", "A_RD_BC", "A_SS_BC", "A_CO_BC", 'A_CU_CC','A_MN_CC','A_ZN_CC', 
        "B_LU_BRP", "B_BT_AK", "B_NR_RIVM", "B_RWT_DANK", "B_RWA_DANK", "B_OV_WENR", 
        "B_HELP_WENR", "B_LG_CBS", 
        "M_M1", "M_M2", "M_M3", "M_M4", "M_M5", "M_M6", "M_M7", "M_M8", "M_M9", "M_M10", "M_M11", "M_M12", "M_M13", "M_M14", "M_M15"
        )
    )
  
  # Run the preprocessing
  dt.ppr <- OBIC::obic_preprocessing(dt)
  
  # Calculate the indicators
  dt.ind <- OBIC::obic_indicators(dt.ppr)
  
  # Score the fields
  dt.score <- OBIC::obic_score(dt.ind, add_relative_score = add_relative_score)
  
  if (add_recommendations) {
    
    # evaluate measures
    dt.measure <- OBIC::obic_evalmeasure(dt.score, extensive = FALSE)
    
    # make recommendations of top 3 measures
    dt.recom <- OBIC::obic_recommendations(dt.measure)
    
    # add recommendations to dt.score
    result <- merge(dt.score,dt.recom,by='ID',all.x = TRUE)
    
  } else {
    
    # give score only
    result <- dt.score
    
  }
  
  # return result
  return(result)
  
}