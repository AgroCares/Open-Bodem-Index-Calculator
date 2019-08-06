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
  
  RM_C_1 = RM_C_2 = RM_C_3 = RM_P_1 = RM_P_2 = RM_P_3 = RM_B_1 = RM_B_2 = RM_B_3 = NULL

  # Chemical recommondations ------------------------------------------------
  dt.score[, RM_C_1 := sample(1:21, size = .N, replace = TRUE)]
  dt.score[, RM_C_2 := sample(1:21, size = .N, replace = TRUE)]
  dt.score[, RM_C_3 := sample(1:21, size = .N, replace = TRUE)]
  

  # Physical recommendations ------------------------------------------------
  dt.score[, RM_P_1 := sample(1:21, size = .N, replace = TRUE)]
  dt.score[, RM_P_2 := sample(1:21, size = .N, replace = TRUE)]
  dt.score[, RM_P_3 := sample(1:21, size = .N, replace = TRUE)]
  

  # Biological recommondations ----------------------------------------------
  dt.score[, RM_B_1 := sample(1:21, size = .N, replace = TRUE)]
  dt.score[, RM_B_2 := sample(1:21, size = .N, replace = TRUE)]
  dt.score[, RM_B_3 := sample(1:21, size = .N, replace = TRUE)]
  
  return(dt.score)
  
}