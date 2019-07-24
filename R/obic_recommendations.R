#' Recommend measurements for better soil managment
#' 
#' This function gives recommendations better soil managament based on the OBI score
#' 
#' @param dt.score (data.table) 
#' 
#' @import data.table
#' 
#' @export
obic_recommendations <- function(dt.score) {
  
  # Check inputs
  checkmate::assert_data_table(dt.score)

  # Chemical recommondations ------------------------------------------------
  dt.score[, RM_C_1 := -999]
  dt.score[, RM_C_2 := -999]
  dt.score[, RM_C_3 := -999]
  

  # Physical recommendations ------------------------------------------------
  dt.score[, RM_P_1 := -999]
  dt.score[, RM_P_2 := -999]
  dt.score[, RM_P_3 := -999]
  

  # Biological recommondations ----------------------------------------------
  dt.score[, RM_B_1 := -999]
  dt.score[, RM_B_2 := -999]
  dt.score[, RM_B_3 := -999]
  
  return(dt.score)
  
}