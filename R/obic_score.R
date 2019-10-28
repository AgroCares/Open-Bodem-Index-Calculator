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
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_OM = I_B_SF = I_B_SB = I_M = NULL
  I_P_CEC = NULL
  
  # Score the chemical indicators
  dt.ind[, S_A_C := (1/9)*I_C_N + (1/9)*I_C_P + (1/9)*I_C_K + (1/9)*I_C_MG + (1/9)*I_C_S + (1/9)*I_C_PH + (1/9)*I_C_CEC + (1/9)*I_C_CU + (1/9)*I_C_ZN ]
  
  # Score the physical indicators
  dt.ind[, S_A_P :=  0.2*I_P_CR + 0.2*I_P_SE + 0*I_P_MS + 0*I_P_BC + 0.2*I_P_DU + 0.2*I_P_CO + 0.2 * I_P_CEC]
  
  # Score the biology
  dt.ind[, S_A_B := 1*I_B_DI + 0*I_B_OM + 0*I_B_SF + 0*I_B_SB]
  
  # Score the management
  dt.ind[, S_A_M := I_M]
  
  # Calculate the total score
  dt.ind[, S_A_T := 0.333*S_A_C + 0.333*S_A_P + 0.333*S_A_B + 0.1*S_A_M]
  
  # Select indicator and scoring columns
  col.sel <- colnames(dt.ind)[grepl("ID|YEAR|^I_|^S_", colnames(dt.ind))]
  dt.ind <- dt.ind[, ..col.sel]
  
  return(dt.ind)
}

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