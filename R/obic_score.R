#' Score the indicators for the OBI
#' 
#' This wrapper function contains the functions to perform the scoring
#' 
#' @param dt.ind (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
obic_score <- function(dt.ind) {
  
  ID = NULL
  
  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  # make local copy
  dt.ind <- copy(dt.ind)
  
  # define variables used within the function
  S_C = S_P = S_B = S_M = S_T = ID =  NULL
  # Score on a absolute scale
  dt.score.abs <- score_absolute(dt.ind)
  
  # Score on a relative scale
  dt.score <- score_relative(dt.score.abs)

  # Aggregate per field
  col.sel <- colnames(dt.score)[grepl("ID|^I_|^S_", colnames(dt.score))]
  dt.score <- dt.ind[, lapply(.SD, mean), by = ID, .SDcols = col.sel]
  
  return(dt.score)
}


#' Score the indicators for the OBI on an absolute scale
#' 
#' This function coalculates the absolute score for the OBI baed on the indicators
#' 
#' @param dt.ind (data.table) The table containg the data needed for OBI
#' 
#' @import data.table
#' 
#' @export
score_absolute <- function(dt.ind) {
  
  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  S_A_C = S_A_P = S_A_B = S_A_M = S_A_T =  NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_SF = I_B_SB = I_M = NULL
  I_P_CEC = I_P_WRI = NULL
  rsid = NULL
  
  # Load in the datasets and reshape
  w <- as.data.table(OBIC::weight.obic)
  w <- dcast(w,rsid~var,value.var = 'weight')
  
  # Score the chemical indicators
  dt.ind[, S_C := 	w$W_C_N * I_C_N + w$W_C_P * I_C_P + w$W_C_K * I_C_K + 
					w$W_C_MG * I_C_MG + w$W_C_S * I_C_S + w$W_C_PH * I_C_PH + 
					w$W_C_CEC * I_C_CEC + w$W_C_CU * I_C_CU + w$W_C_ZN * I_C_ZN]
  
  # Score the physical indicators
   dt.ind[, S_P :=  w$W_P_CR * I_P_CR + w$W_P_SE * I_P_SE + w$W_P_MS * I_P_MS +  
					w$W_P_BC * I_P_BC + w$W_P_DU * I_P_DU + w$W_P_CO * I_P_CO + 
					w$W_P_CEC * I_P_CEC + w$W_P_WRI * I_P_WRI]
  
  # Score the biology
  dt.ind[, S_B := w$W_B_DI * I_B_DI + w$W_B_SF * I_B_SF + w$W_B_SB * I_B_SB]
  
  # Score the management
  dt.ind[, S_A_M := I_M]
  
  # Calculate the total score
  dt.ind[, S_T := 0.3*S_C + 0.3*S_P + 0.3*S_B + 0.1*S_M]
  
  # Aggregate per field over the last 10 years
  col.sel <- colnames(dt.ind)[grepl("ID|^I_|^S_", colnames(dt.ind))]
  dt.ind <- dt.ind[, lapply(.SD, mean), by = ID, .SDcols = col.sel]
  
  # return only the indices and the scores
  return(dt.ind)
}

#' Weight of indicators to calculate integrated scores
#' 
#' This table defines the weighting factors (ranging between 0 and 1) of indicator values to calculate integrated scores. 
#' This table is used internally in \code{\link{obic_score}}
#' 
"weight.obic"

#' Score the scoring for the OBI on a relative scale
#' 
#' This function calculates the relative scoring based on the score calculated by \code{\link{score_absolute}
#' 
#' @param dt.score.abs (data.table) The table containing the score calculated by \code{\link{score_absolute}
#' @param id (integer) The id of the field to visualize the score
#' 
#' @import data.table
#' 
#' @export
score_relative <- function(dt.score.abs) {
  
  # TESTING
  load("../development/data/testing_score_relative.Rdata")
  
  # Check inputs
  checkmate::assert_data_table(dt.score.abs)
  
  # Select columns to base the ranking on
  grouping <- c("YEAR", "B_GT", "B_BT_AK")
  groups <- dt.score.abs[, (count = .N), by = grouping]
  groups[, group_id := 1:.N]
  groups[V1 < 20, group_id := -1]
  
  # Join the group_id to 
  col.sel <- c(grouping, "group_id")
  dt.score <- merge(dt.score.abs, groups[, ..col.sel], by = grouping, all.x = TRUE)
  
  # Rank the absolute values and scale them between 1 and 0
  dt.score[, S_R_C := frank(S_A_C), by = group_id]
  dt.score[, S_R_C := (S_R_C - min(S_R_C, na.rm = TRUE)) / (max(S_R_C, na.rm = TRUE) - min(S_R_C, na.rm = TRUE)), by = group_id]
  dt.score[, S_R_P := frank(S_A_P), by = group_id]
  dt.score[, S_R_P := (S_R_P - min(S_R_C, na.rm = TRUE)) / (max(S_R_P, na.rm = TRUE) - min(S_R_P, na.rm = TRUE)), by = group_id]
  dt.score[, S_R_B := frank(S_A_B), by = group_id]
  dt.score[, S_R_B := (S_R_B - min(S_R_B, na.rm = TRUE)) / (max(S_R_B, na.rm = TRUE) - min(S_R_B, na.rm = TRUE)), by = group_id]
  dt.score[, S_R_M := frank(S_A_M), by = group_id]
  dt.score[, S_R_M := (S_R_M - min(S_R_M, na.rm = TRUE)) / (max(S_R_M, na.rm = TRUE) - min(S_R_M, na.rm = TRUE)), by = group_id]
  dt.score[, S_R_T := frank(S_A_T), by = group_id]
  dt.score[, S_R_T := (S_R_T - min(S_R_T, na.rm = TRUE)) / (max(S_R_T, na.rm = TRUE) - min(S_R_T, na.rm = TRUE)), by = group_id]

  return(dt.score)
}


#' Visualize the score
#' 
#' Visualize the score of OBIC in absolute terms and relative terms
#' 
#' @param dt.score.abs (data.table) The table containing the score calculated by \code{\link{score_absolute}
#' @param id (integer) The id of the field to visualize the score
#' 
#' @import data.table
#' @import ggplot2
#' 
#' @param dt.score (dat.table) 
score_visualize <- function(dt.score, id) {
  
  
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install first the package `ggplot2` to use this function")
  }
  
  # TESTING!!!
  load("../development/data/testing_score_visualize.Rdata")
  id = 4613
  year = 2015
  library(ggplot2)

  dt.a <- melt(dt.score, id.vars = c("ID", "YEAR", "group_id"), measure.vars = patterns("^S_A_"), variable.name = "type", value.name = "S_A")
  dt.a$type <- gsub("S_A_", "", dt.a$type)
  dt.r <- melt(dt.score, id.vars = c("ID", "YEAR", "group_id"), measure.vars = patterns("^S_R_"), variable.name = "type", value.name = "S_R")
  dt.r$type <- gsub("S_R_", "", dt.r$type)
  dt.vis <- merge(dt.a, dt.r, by = c("ID", "YEAR", "type", "group_id"))
  dt.vis.sel <- dt.vis[ID == id & YEAR == year]
  dt.vis.not <- dt.vis[ID != id & group_id %in% dt.vis.sel$group_id & YEAR == year]
  
  labels <- c(
    "B" = "Biologisch", 
    "C" = "Chemisch",
    "M" = "Management", 
    "P" = "Physisch",
    "T" = "Totaal"
  )
  
  vis1 <- ggplot(data = dt.vis.not, aes(x = S_A, y = S_R)) +
    geom_point(col = "grey", size = 1, alpha = 0.5) +
    geom_point(data = dt.vis.sel, col = "blue", size = 3) +
    facet_wrap(.~type, labeller = as_labeller(labels)) +
    scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
    labs(
      x = "Absoluut", 
      y= "Relatief",
      title = 'OBI Score',
      caption = paste0("Perceel: ", id ,"\nJaar: ", year, "\nGroep: ", unique(dt.vis.sel$group_id))) +
    theme_bw()
  
  
  dt.vis2 <- melt(dt.score, id.vars = c("ID", "YEAR", "group_id"), measure.vars = patterns("^S_"), variable.name = "type", value.name = "score", variable.factor = FALSE)
  dt.vis2[grepl("^S_A_", type), method := "Absoluut"]
  dt.vis2[grepl("^S_R_", type), method := "Relatief"]
  dt.vis2[, type := gsub("^S_A_|^S_R_", "", type)]
  dt.vis2.sel <- dt.vis2[ID == id & YEAR == year]
  
  vis2 <- ggplot(data = dt.vis2.sel, aes(x = type, y = score)) + 
    geom_col(position = "dodge", aes(fill = method), width = 0.7) +
    coord_flip() +
    labs(
      x = "", 
      y= "Score",
      fill = "Methode",
      title = 'OBI Score',
      caption = paste0("Perceel: ", id ,"\nJaar: ", year, "\nGroep: ", unique(dt.vis.sel$group_id))) +
    theme_bw() 
  vis2
  
  vis3 <- ggplot(data = dt.vis2.sel, aes(x = type, y = score)) + 
    geom_point(aes(col = method), size = 5, alpha = 0.8) +
    coord_flip() +
    labs(
      x = "", 
      y= "Score",
      fill = "Methode",
      title = 'OBI Score',
      caption = paste0("Perceel: ", id ,"\nJaar: ", year, "\nGroep: ", unique(dt.vis.sel$group_id))) +
    theme_bw() 
  vis3
  
}