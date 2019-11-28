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

  # Check inputs
  checkmate::assert_data_table(dt.ind)
  
  # make local copy
  dt.ind <- copy(dt.ind)
  
  # define variables used within the function
  ID =  YEAR = col.sel = cf = NULL
  
  # Score on a absolute scale
  dt.score <- score_absolute(dt.ind)
  
  # Score on a relative scale
  dt.score <- score_relative(dt.score)

  # Aggregate per field
  col.sel <- colnames(dt.score)[grepl("ID|YEAR|^I_|^S_", colnames(dt.score))]
  dt.aggr <- dt.score[, ..col.sel]
  dt.aggr[YEAR < max(YEAR) - 4, cf := 0.4, by = ID]
  dt.aggr[YEAR >= max(YEAR) - 4, cf := 0.6, by = ID]
  dt.aggr <- dt.aggr[, lapply(.SD, mean), by = list(ID, cf)]
  dt.aggr <- dt.aggr[, lapply(.SD, function (x, cf) {mean(x * cf)}, cf), by = list(ID)]

  return(dt.aggr)
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
  
  # make local copy
  dt.ind <- copy(dt.ind)
  
  S_C_A = S_P_A = S_B_A = S_M_A = S_T_A =  NULL
  I_C_N = I_C_P = I_C_K = I_C_MG = I_C_S = I_C_PH = I_C_CEC = I_C_CU = I_C_ZN = NULL
  I_P_CR = I_P_SE = I_P_MS = I_P_BC = I_P_DU = I_P_CO = I_B_DI = I_B_SF = I_B_SB = I_M = NULL
  I_P_CEC = I_P_WRI = NULL
  rsid = NULL
  
  # Load in the datasets and reshape
  w <- as.data.table(OBIC::weight.obic)
  w[, rsid := 1]
  w <- dcast(w,rsid~var,value.var = 'weight')
  
  # Score the chemical indicators
  dt.ind[, S_C_A := ((w$W_C_N + 1/I_C_N) * I_C_N + (w$W_C_P + 1/I_C_P) * I_C_P + w$W_C_K * I_C_K + 
					w$W_C_MG * I_C_MG + w$W_C_S * I_C_S +(w$W_C_PH + 1/I_C_PH) * I_C_PH + 
					w$W_C_CEC * I_C_CEC + w$W_C_CUZN * (I_C_CU + I_C_ZN)) / 
					  (1 + 1/I_C_N + 1/I_C_P + 1/I_C_PH)]
  
  # Score the physical indicators
   dt.ind[, S_P_A :=  w$W_P_CR * I_P_CR + w$W_P_SE * I_P_SE + w$W_P_MS * I_P_MS +  
					w$W_P_BC * I_P_BC + w$W_P_DU * I_P_DU + w$W_P_CO * I_P_CO + 
					w$W_P_CEC * I_P_CEC + w$W_P_WRI * I_P_WRI]
  
  # Score the biology
  dt.ind[, S_B_A := w$W_B_DI * I_B_DI + w$W_B_SF * I_B_SF + w$W_B_SB * I_B_SB]
  
  # Score the management
  dt.ind[, S_M_A := I_M]
  
  # Calculate the total score
  dt.ind[, S_T_A := 0.35 * S_C_A + 0.35 * S_P_A + 0.2 * S_B_A + 0.1 * S_M_A]
  
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
#' This function calculates the relative scoring based on the score calculated by \code{\link{score_absolute}}
#' 
#' @param dt.score.abs (data.table) The table containing the score calculated by \code{\link{score_absolute}}
#' 
#' @import data.table
#' 
#' @export
score_relative <- function(dt.score.abs) {
  
  # Check inputs
  checkmate::assert_data_table(dt.score.abs)
  
  # make local copy
  dt.score.abs <- copy(dt.score.abs)
  
  group_id = V1 = col.sel = waterstress = yield_depression = NULL
  S_C_R = S_P_R = S_B_R = S_M_R = S_T_R = NULL
  S_C_A = S_P_A = S_B_A = S_M_A = S_T_A = NULL
  
  # Join crop categories
  crops.obic <- as.data.table(OBIC::crops.obic)
  col.sel <- c("crop_code", "crop_waterstress")
  dt.score <- merge(dt.score.abs, crops.obic[, ..col.sel], by.x = "B_LU_BRP", by.y = "crop_code", all.x = TRUE)

  # Join the waterstress levels
  waterstress.obic <- as.data.table(OBIC::waterstress.obic)
  waterstress.obic[waterstress < 11, yield_depression := 1]
  waterstress.obic[waterstress >= 11 & waterstress < 30, yield_depression := 2]
  waterstress.obic[waterstress >= 30, yield_depression := 3]
  dt.score <- merge(dt.score, waterstress.obic, by.x = c("crop_waterstress", "B_HELP_WENR", "B_GT"), by.y = c("cropname", "soilunit", "gt"))

  # Select columns to base the ranking on
  grouping <- c("YEAR", "yield_depression", "crop_waterstress", "B_BT_AK")
  groups <- dt.score[, (count = .N), by = grouping]
  groups[, group_id := 1:.N]
  groups[V1 < 10, group_id := -1]

  # Join the group_id to
  col.sel <- c(grouping, "group_id")
  dt.score <- merge(dt.score, groups[, ..col.sel], by = grouping, all.x = TRUE)

  # Rank the absolute values and scale them between 1 and 0
  dt.score[, S_C_R := frank(S_C_A), by = group_id]
  dt.score[, S_C_R := (S_C_R - min(S_C_R, na.rm = TRUE)) / (max(S_C_R, na.rm = TRUE) - min(S_C_R, na.rm = TRUE)), by = group_id]
  dt.score[, S_P_R := frank(S_P_A), by = group_id]
  dt.score[, S_P_R := (S_P_R - min(S_P_R, na.rm = TRUE)) / (max(S_P_R, na.rm = TRUE) - min(S_P_R, na.rm = TRUE)), by = group_id]
  dt.score[, S_B_R := frank(S_B_A), by = group_id]
  dt.score[, S_B_R := (S_B_R - min(S_B_R, na.rm = TRUE)) / (max(S_B_R, na.rm = TRUE) - min(S_B_R, na.rm = TRUE)), by = group_id]
  dt.score[, S_M_R := frank(S_M_A), by = group_id]
  dt.score[, S_M_R := (S_M_R - min(S_M_R, na.rm = TRUE)) / (max(S_M_R, na.rm = TRUE) - min(S_M_R, na.rm = TRUE)), by = group_id]
  dt.score[, S_T_R := frank(S_T_A), by = group_id]
  dt.score[, S_T_R := (S_T_R - min(S_T_R, na.rm = TRUE)) / (max(S_T_R, na.rm = TRUE) - min(S_T_R, na.rm = TRUE)), by = group_id]

  return(dt.score)
}


#' Visualize the score
#' 
#' Visualize the score of OBIC in absolute terms and relative terms
#' 
#' @param dt.score (data.table) The table containing the score calculated by \code{\link{obic_score}}
#' @param id (integer) The id of the field to visualize the score
#' 
#' @import data.table
#' 
score_visualize <- function(dt.score, id) {
  
  # 
  # if(!requireNamespace("ggplot2", quietly = TRUE)) {
  #   stop("Install first the package `ggplot2` to use this function")
  # }
  # 
  # # TESTING!!!
  # load("../development/data/testing_score_visualize.Rdata")
  # id = 4613
  # year = 2015
  # library(ggplot2)
  # 
  # dt.a <- melt(dt.score, id.vars = c("ID", "YEAR", "group_id"), measure.vars = patterns("^S_A_"), variable.name = "type", value.name = "S_A")
  # dt.a$type <- gsub("S_A_", "", dt.a$type)
  # dt.r <- melt(dt.score, id.vars = c("ID", "YEAR", "group_id"), measure.vars = patterns("^S_R_"), variable.name = "type", value.name = "S_R")
  # dt.r$type <- gsub("S_R_", "", dt.r$type)
  # dt.vis <- merge(dt.a, dt.r, by = c("ID", "YEAR", "type", "group_id"))
  # dt.vis.sel <- dt.vis[ID == id & YEAR == year]
  # dt.vis.not <- dt.vis[ID != id & group_id %in% dt.vis.sel$group_id & YEAR == year]
  # 
  # labels <- c(
  #   "B" = "Biologisch", 
  #   "C" = "Chemisch",
  #   "M" = "Management", 
  #   "P" = "Physisch",
  #   "T" = "Totaal"
  # )
  # 
  # vis1 <- ggplot(data = dt.vis.not, aes(x = S_A, y = S_R)) +
  #   geom_point(col = "grey", size = 1, alpha = 0.5) +
  #   geom_point(data = dt.vis.sel, col = "blue", size = 3) +
  #   facet_wrap(.~type, labeller = as_labeller(labels)) +
  #   scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  #   scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  #   labs(
  #     x = "Absoluut", 
  #     y= "Relatief",
  #     title = 'OBI Score',
  #     caption = paste0("Perceel: ", id ,"\nJaar: ", year, "\nGroep: ", unique(dt.vis.sel$group_id))) +
  #   theme_bw()
  # 
  # 
  # dt.vis2 <- melt(dt.score, id.vars = c("ID", "YEAR", "group_id"), measure.vars = patterns("^S_"), variable.name = "type", value.name = "score", variable.factor = FALSE)
  # dt.vis2[grepl("^S_A_", type), method := "Absoluut"]
  # dt.vis2[grepl("^S_R_", type), method := "Relatief"]
  # dt.vis2[, type := gsub("^S_A_|^S_R_", "", type)]
  # dt.vis2.sel <- dt.vis2[ID == id & YEAR == year]
  # 
  # vis2 <- ggplot(data = dt.vis2.sel, aes(x = type, y = score)) + 
  #   geom_col(position = "dodge", aes(fill = method), width = 0.7) +
  #   coord_flip() +
  #   labs(
  #     x = "", 
  #     y= "Score",
  #     fill = "Methode",
  #     title = 'OBI Score',
  #     caption = paste0("Perceel: ", id ,"\nJaar: ", year, "\nGroep: ", unique(dt.vis.sel$group_id))) +
  #   theme_bw() 
  # vis2
  # 
  # vis3 <- ggplot(data = dt.vis2.sel, aes(x = type, y = score)) + 
  #   geom_point(aes(col = method), size = 5, alpha = 0.8) +
  #   coord_flip() +
  #   labs(
  #     x = "", 
  #     y= "Score",
  #     fill = "Methode",
  #     title = 'OBI Score',
  #     caption = paste0("Perceel: ", id ,"\nJaar: ", year, "\nGroep: ", unique(dt.vis.sel$group_id))) +
  #   theme_bw() 
  # vis3
  
}